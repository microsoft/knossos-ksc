GHC=$1

$GHC -main-is test -isrc/ksc -Werror -Wunused-imports -Wunused-top-binds -Wmissing-signatures src/ksc/Main.hs
