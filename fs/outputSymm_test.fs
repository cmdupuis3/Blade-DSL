
let nc1 = {
    Info = NetCDF {FileName = "file1"; VariableName = "var1"}
    Name = "nc1"
    Rank = 4
    Symm = Some([1;1;2;3])
    Type = "float"
}

let nc2 = {
    Info = NetCDF {FileName = "file1"; VariableName = "var2"}
    Name = "nc2"
    Rank = 4
    Symm = Some([1;1;2;3])
    Type = "float"
}

let iarrays = [nc1; nc2]

let func = {
    Name = "func"
    Arity = Some(2)
    INames = ["rav1"; "rav2"]
    IRank = [2;2]
    OName = "res"
    OType = "float"
    ORank = 6
    TDimSymm = [1;2]
    Comm = Some [1;1]
    ParallelismLevels = [1;0]
    Inner = ["res = rav1 + rav2;"]
    TDimExtents = [20;20]
    NCInfo = Some { DimNames = ["dim1"; "dim2"; "dim3"]; 
                    DimValNames = ["dim1"; "dim2"; "dim3"]; 
                    DimValTypes = ["int"; "int"; "int"]}
}


OutputSymmetry iarrays func;;