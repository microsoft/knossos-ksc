GHC=$1

$GHC -main-is test -isrc/ksc -Werror -Wunused-local-binds -Wunused-matches -Wunused-imports -Wunused-top-binds -Wmissing-signatures src/ksc/Main.hs
$GHC -main-is profile -o profile -isrc/ksc -Werror -Wunused-local-binds -Wunused-matches -Wunused-imports -Wunused-top-binds -Wmissing-signatures src/ksc/Main.hs
