# Make our output C++ a tiny bit prettier
# This might introducing name clashes and syntax errors, 
# so is just get a quick gisty view of the generated code
# To use: 
#   $ ./ksc  test/ksc/gmm.ks 
#   $  sh etc/prettify-cpp.sh /tmp/t.cpp | grep -A 30 '^ty\$gmm_knossos_gmm_objective '
# Will produce something like
#   ty$gmm_knossos_gmm_objective gmm_knossos_gmm_objective(ks::allocator * alloc, tensor<1, tensor<1, double>> x, tensor<1, double> alphas, tensor<1, tensor<1, double>> means, tensor<1, tensor<1, double>> qs, tensor<1, tensor<1, double>> ls, tuple<double,int> wishart) {
#   int c0 = size(x);
#   int N = c0;
#   int c1 = size(alphas);
#   int K = c1;
#   tensor<1, double> c2 = index(0, x);
#   ...


sed -r\
 -e 's/\$alloc/alloc/g'\
 -e 's/^([ \t]+)ty\$[a-zA-Z0-9_$]+/\1auto/g'\
 -e 's/rev\$/Grad_/g'\
 -e 's/\bc\$/c/g'\
 -e 's/\$a[A-Za-z0-9_$]+//g'\
 -e 's/ks::alloc_mark_t (.*) = alloc->mark\(\)/ks::mark \1(alloc)/' \
 -e 's/aten\$8.8/at::/g'\
 $*
