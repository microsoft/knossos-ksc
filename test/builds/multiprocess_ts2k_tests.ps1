# The tests run out of memory on CI see https://github.com/microsoft/knossos-ksc/issues/679 for details of the leak in module creation
# pytest-forked on Windows doesn't help, so: collect the tests, run one at a time, exit on any failure
$alltests = pytest $PSScriptRoot\..\ts2k\ --collect-only --quiet --disable-pytest-warnings | Select-Object -skip 2 -last 1000000
Foreach ($test in $alltests)
{
    $test
    $test_output = pytest $PSScriptRoot\..\ts2k\ $test
    if ( 0 -ne $LastExitcode)
    {
        Write-Output $test_output
        exit $LastExitcode
    } 
}