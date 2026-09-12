// Run deep-recursion-prone work on a thread with a large stack.
//
// The compile pipeline walks the AST/IR by mutual recursion -- one native
// frame per nesting level (TypeCheck.inferExpr/inferBinOp, CodeGen.exprToCpp,
// Lowering, IR validation, ...). The ppl jet elaborator (dist_jet/dist_map)
// generates arithmetic chains ~150+ operators deep, and those blow the default
// ~1 MB thread stack. Debug builds overflow first (no tail-call/frame-size
// optimizations); Release only has more headroom, so a deep enough program
// would overflow there too. Repro: tests/corpus/ppl/{033,034,036,039}.blade,
// which stack-overflow under a Debug `blade check` and are fine on a big stack.
//
// The fix, standard for recursive-descent compilers (the F# compiler itself
// does this), is to run the pipeline on a dedicated large-stack thread. The
// two chokepoints wrapped are Cli.dispatch (all CLI commands) and the test
// runner's per-test F# pipeline.
module Blade.Runtime

open System.Threading
open System.Runtime.ExceptionServices

/// Reserved stack for compile-pipeline worker threads: 64 MB, ~60x the
/// observed worst case (~=316 frames / ~1 MB for the deepest elaborated chain).
/// This is a RESERVATION only -- pages commit on demand as recursion touches
/// them -- so the cost is nil until a program actually recurses that deep.
let largeStackBytes = 64 * 1024 * 1024

/// Run `work` on a dedicated thread with a large stack and return its result.
/// Any exception is re-raised on the caller's thread with its original stack
/// trace preserved (via ExceptionDispatchInfo), so callers that pattern-match
/// on thrown exceptions see identical behavior to a direct call.
let runOnLargeStack (work: unit -> 'T) : 'T =
    let mutable result = Unchecked.defaultof<'T>
    let mutable captured : ExceptionDispatchInfo = null
    let body () =
        try result <- work ()
        with ex -> captured <- ExceptionDispatchInfo.Capture ex
    let t = Thread(ThreadStart body, largeStackBytes)
    t.Start()
    t.Join()
    match captured with
    | null -> result
    | edi -> edi.Throw(); Unchecked.defaultof<'T>   // Throw() always throws; line unreachable

/// Drain a redirected child-process stream to its end on a DEDICATED thread.
///
/// `StreamReader.ReadToEndAsync` on a `Process` pipe is sync-over-async: the
/// pipe handle is not opened for overlapped I/O, so every read is a blocking
/// `ReadFile` queued to the THREAD POOL. The test harness runs its tests on
/// pool threads (`Array.Parallel.mapi`), and each of those blocks in
/// `WaitForExit` while its child runs -- so under load the pool has no thread
/// left to run the reads. The child then fills the 4 KB pipe, blocks on its
/// next write, and sits there until the harness timeout kills it: a hang that
/// lives entirely on the harness side and reads, from the log, as the program
/// stalling right before a large print (its last flushed line intact, nothing
/// of the next one). Seen as 120 s "Execution timed out" reds on the 4-core
/// nightly runner -- a different pair of corpus tests each night, the same
/// commit green one night and red the next. Reproduced by capping the pool
/// (runtimeconfig `System.Threading.ThreadPool.MaxThreads` = 4): `blade test
/// rand` wedges with no test finishing; on this helper the same cap runs it
/// in two seconds.
///
/// A thread of our own has no such dependency. Returned as a Task so the
/// call sites keep their `.Result` / `.Wait` shape: same bytes, and a faulted
/// read still surfaces as an AggregateException on `.Result`, exactly as
/// before. Background, so a wedged pipe cannot keep the process alive; a small
/// stack, since all it does is copy bytes.
let readToEndOffPool (reader: System.IO.StreamReader) : System.Threading.Tasks.Task<string> =
    let tcs = System.Threading.Tasks.TaskCompletionSource<string>()
    let body () =
        try tcs.SetResult(reader.ReadToEnd())
        with ex -> tcs.SetException ex
    let t = Thread(ThreadStart body, 256 * 1024)
    t.IsBackground <- true
    t.Name <- "blade-pipe-drain"
    t.Start()
    tcs.Task
