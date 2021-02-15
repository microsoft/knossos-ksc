# Make our output C++ a tiny bit prettier (possibly introducing name clashes)
sed -r\
 -e 's/aten\$8.8/at::/g'\
 -e 's/\$alloc/alloc/g'\
 -e 's/^([ \t]+)ty\$[a-zA-Z0-9_$]+/\1auto/g'\
 -e 's/rev\$/Grad_/g'\
 -e 's/\bc\$/c/g'\
 -e 's/\$a[A-Za-z0-9_$]+//' $*
