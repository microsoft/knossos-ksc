set -e

GHC=$1

ERRORS="-Wincomplete-patterns -Wunused-local-binds -Wunused-matches -Wunused-imports -Wunused-top-binds -Wmissing-signatures"

$GHC -main-is test -isrc/ksc -Werror $ERRORS src/ksc/Main.hs
$GHC -main-is profile -o profile -isrc/ksc -Werror $ERRORS src/ksc/Main.hs
