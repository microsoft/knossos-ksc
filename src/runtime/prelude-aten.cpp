
#include "knossos.h"

#include <cmath>

namespace ks {

tensor<1, double> 
aten$8$8matmul$aT2fT1f(allocator * alloc, tensor<2,double> const& M, tensor<1,double> const& v)
{
	auto [r,c] = size(M);
    KS_ASSERT(c == size(v));
	tensor<1,double> ret(alloc, r);
	for(int i = 0; i < r; ++i)
		ret[i] = ts_dot(M[i], v);
	return ret;
}

template <size_t Dim, class T>
tensor<Dim, T>
aten$8$8pow$aT2fi(allocator * alloc, tensor<Dim,T> const& a, int const& i)
{
	return elementwise_map(alloc, a, [i](T const& v) { return std::pow(v, i); });
}

tuple<tensor<2,double>,tensor<1,double>> 
rev$aten$8$8matmul$a$dT2fT1f$bT1f(allocator * alloc, std::tuple<tensor<2,double>, tensor<1,double>> const& M_v, tensor<1,double> const& dr)
{
    auto [M, v] = M_v;
	auto [r, c] = size(M);
	KS_ASSERT(c == size(v));

	tensor<2,double> retM(alloc, size(M));
	for(int i = 0; i < r; ++i)
		retM[i] = ts_scale(alloc, dr[i], v);

	tensor<1,double> retv(alloc, c);
	for(int i = 0; i < c; ++i) {
		double retvi = 0;
		for(int j = 0; j < r; ++j)
			retvi += M[j][i] * dr[j];
		retv[i] = retvi;
	}

	return {retM,retv};
}

}
