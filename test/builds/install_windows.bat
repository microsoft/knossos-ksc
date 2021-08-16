echo ----- install g++ -----
choco install mingw --version 8.1.0 -y || exit /b
echo ----- test g++ installation -----
g++ --version
where g++
echo ----- install Ninja -----
choco install ninja || exit /b
echo ----- install ghc -----
choco install cabal --allow-downgrade --version 3.0.0.0 -y || exit /b
choco install ghc --allow-downgrade --version 8.10.6 -y || exit /b
echo ----- cabal install -----
call refreshenv && C:/ProgramData/chocolatey/lib/cabal/tools/cabal-3.0.0.0/cabal new-update || exit /b
echo ----- installation done -----
