#!/bin/bash

IN_FILE_NAME=`echo "$(pwd)/$1"`
OUT_FILE_NAME=$2


#DOTNET_VERSION=`dotnet --version`
DOTNET_PATH='/usr/share/dotnet'
#FS_PATH="$DOTNET_PATH/sdk/$DOTNET_VERSION/FSharp/"
#echo "F\# Path: $FS_PATH"


# need these for NetCDF compatability
NC_LIBS="-L/usr/local/lib/ -lnetcdf"
NC_INCLUDES="-I/usr/include/"


BLADE_ROOT=`pwd`
mkdir exec


# Step 1: compile nested_funcs.c
pushd $BLADE_ROOT/src
gcc nested_funcs.c -o $BLADE_ROOT/exec/nested_funcs.so --std=c11 -fPIC -shared $NC_INCLUDES $NC_LIBS
popd


# Step 2: compile parser.fsx and run parser on user file
pushd $BLADE_ROOT/exec
$DOTNET_PATH/dotnet new console -lang F# -n execFS
cp $BLADE_ROOT/src/parser.fs $BLADE_ROOT/exec/execFS

pushd $BLADE_ROOT/exec/execFS

##### hack alert!
rm Program.fs
cat << EOF > Program.fsx
open Parser
let iFileName = @"$IN_FILE_NAME"
let oFileName = @"$BLADE_ROOT/exec/blade.cpp"
Parser.compile iFileName oFileName
EOF

rm execFS.fsproj
cat << EOF > execFS.fsproj
<Project Sdk="Microsoft.NET.Sdk">

  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net6.0</TargetFramework>
  </PropertyGroup>

  <ItemGroup>
    <Compile Include="parser.fs" />
    <Compile Include="Program.fsx" />
  </ItemGroup>

</Project>
EOF

$DOTNET_PATH/dotnet build
$DOTNET_PATH/dotnet run

popd


# Step 3: compile intermediate file with nested_array_utilities.cpp

cp $BLADE_ROOT/src/nested_array_utilities.cpp .
g++ nested_array_utilities.cpp blade.cpp -fopenmp -std=c++17 -O3 -o $OUT_FILE_NAME $NC_INCLUDES $NC_LIBS

cp $OUT_FILE_NAME $BLADE_ROOT
popd


# Step 4: run executable
# ./$OUT_FILE_NAME







