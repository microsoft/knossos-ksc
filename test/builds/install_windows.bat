echo ----- install g++ -----
choco install mingw --version 8.1.0 -y || exit /b
echo ----- test g++ installation -----
g++ --version
where g++
echo ----- install ghc -----
choco install cabal --allow-downgrade --version 2.4.1.0 -y || exit /b
choco install ghc --allow-downgrade --version 8.4.4 -y || exit /b
echo ----- cabal install -----
refreshenv && cabal new-update && cabal install hspec parsec mtl hashable || exit /b
echo ----- installation done -----
