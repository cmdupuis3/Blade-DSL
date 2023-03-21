#!/bin/bash

IN_FILE_NAME=$1
OUT_FILE_NAME=$2 # do we need this?


#DOTNET_VERSION=`dotnet --version`
DOTNET_PATH='/usr/share/dotnet'
#FS_PATH="$DOTNET_PATH/sdk/$DOTNET_VERSION/FSharp/"
#echo "F\# Path: $FS_PATH"


# need these for NetCDF compatability
NC_LIBS="-L/usr/local/lib/ -lnetcdf"
NC_INCLUDES="-I/usr/include/"


USER_PWD=`pwd`

mkdir -p ~/tmp2/blade/
pushd ~/tmp2/blade/
git clone https://github.com/cmdupuis3/EDGI_nested_iterators.git


# Step 1: compile nested_funcs.c
pushd ./EDGI_nested_iterators/fs
gcc nested_funcs.c -o nested_funcs.so --std=c11 -fPIC -shared $NC_INCLUDES $NC_LIBS
popd

# Step 2: compile parser.fsx and run parser on user file
$DOTNET_PATH/dotnet new console -lang F# -n blade
cp ./EDGI_nested_iterators/fs/parser.fs ./blade
pushd blade

##### hack alert!
rm Program.fs
cat << EOF > Program.fsx
open Parser
let iFileName = @"$IN_FILE_NAME"
let oFileName = @"$USER_PWD/$OUT_FILE_NAME"
Parser.compile iFileName oFileName
EOF

rm blade.fsproj
cat << EOF > blade.fsproj
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

mkdir exec
pushd exec
cp $USER_PWD/$OUT_FILE_NAME .
cp ~/tmp2/blade/EDGI_nested_iterators/nested_array_utilities.cpp .
g++ nested_array_utilities.cpp $OUT_FILE_NAME -fopenmp -std=c++17 -o blade.x $NC_INCLUDES $NC_LIBS

cp blade.x $USER_PWD
popd

popd


# Step 4: run executable
./blade.x







