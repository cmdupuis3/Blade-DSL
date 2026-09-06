// The ONE IR-level matcher of a halo window read (docs/plans/structural/02,
// section 3.1.2). A dense stencil kernel reads its operand as
// `A[.., w + k, ..]` where `w` is the window's centre ordinal and `k` a
// static signed offset (`w(k)` lowers to `Add(w, Lit k)`, negatives as
// `Add(w, Neg(Lit k))` -- src/Lowering.fs, the window-read arm). Four
// consumers used to re-derive that shape with four private scanners -- the
// halo carousel, the codegen and interpreter BL8009 extent guards, and now
// the reverse-mode gather -- and they had drifted on which index positions
// they looked at and whether a computed offset counted. They all read this.
//
// `isWindow` is the caller's test for the window variable: the carousel
// asks for one specific binder id, the extent guards for any variable typed
// with a dense `__halowin|d:` tag. Every index position is examined, not
// only the last (a window may sit on any axis of a rank-k read); `Dim` says
// which. A read whose offset is not a literal is REPORTED, with
// `Offset = None`, never dropped: a consumer that cannot serve it must decline
// (the memo's rule: an unknown access is never "reads nothing").
module Blade.IRAccess

open Blade.Types
open Blade.IR

type WindowRead = {
    /// The IRIndex node itself, by reference -- the SubstMap contract the
    /// carousel keys its substitutions on.
    Node: IRExpr
    /// The array read.
    ArrayId: IRId
    /// The read's rank and the position of the windowed subscript.
    Rank: int
    Dim: int
    /// The subscripts before the windowed one.
    Prefix: IRExpr list
    /// The window variable expression (an IRVar or IRParam), so a consumer
    /// keyed by tag can recover it.
    Window: IRExpr
    /// The static signed offset, None for a computed one.
    Offset: int option
}

let private staticOffset (e: IRExpr) : int option =
    match e with
    | IRLit (IRLitInt k) -> Some (int k)
    | IRUnaryOp (IRNeg, IRLit (IRLitInt k)) -> Some (int -k)
    | _ -> None

/// Every window read in `body`, in traversal order.
let windowReadsOf (isWindow: IRExpr -> bool) (body: IRExpr) : WindowRead list =
    let found = System.Collections.Generic.List<WindowRead>()
    iterIRExpr (fun e ->
        match e with
        | IRIndex (IRVar (aid, _), idxs, _) when not (List.isEmpty idxs) ->
            idxs |> List.iteri (fun d ix ->
                match ix with
                | IRBinOp (IRElementwise, IRAdd, wv, offExpr) when isWindow wv ->
                    found.Add
                        { Node = e; ArrayId = aid; Rank = idxs.Length; Dim = d
                          Prefix = idxs |> List.take d; Window = wv
                          Offset = staticOffset offExpr }
                | _ -> ())
        | _ -> ()) body
    List.ofSeq found

/// The dense-halo tag on a window variable, if it carries one.
let denseHaloTagOf (wv: IRExpr) : string option =
    match wv with
    | IRVar (_, IRTIdxTagged (_, IRefNamed t))
    | IRParam (_, _, IRTIdxTagged (_, IRefNamed t))
        when t.StartsWith (Blade.Types.haloWinTagPrefix + "d:") -> Some t
    | _ -> None
