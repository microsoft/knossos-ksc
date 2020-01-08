set -e

GHC=$1

ERRORS="-Wall -Wno-name-shadowing -Wno-unticked-promoted-constructors -Wno-type-defaults -Wno-unused-do-bind -Wincomplete-uni-patterns -Wincomplete-record-updates"

$GHC -o ksc -isrc/ksc -Werror $ERRORS src/ksc/Main.hs
