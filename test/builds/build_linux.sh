GHC=$1

$GHC -main-is test -isrc/ksc -Werror -Wunused-matches -Wunused-imports -Wunused-top-binds -Wmissing-signatures src/ksc/Main.hs
