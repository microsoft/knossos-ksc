# This doesn't work:
# dotnet run '/p:DefineConstants="DiffSharp"'  .\ml-gmm.fsproj

dotnet clean
dotnet msbuild '/p:DefineConstants="DiffSharp"'
