// Defines everything in Knossos.ks

namespace ks {
// ===============================  Test edef  ========================
// These only exist so that we can test edef functionality.
// We should probably come up with a better story for the
// tests but at the time of writing I didn't want to hold back
// edef support any longer.
Float edef_example$af(allocator *, Float x) { return x; }
Float fwd$edef_example$af(allocator *, Float x, Float dx) { return dx; }
Float rev$edef_example$af(allocator *, Float x, Float ddr) { return ddr; }
ks::Tuple<Float,ks::Tuple<>> suffwdpass$edef_example$af(allocator *, Float x) { return ks::make_Tuple(x, ks::make_Tuple()); }
Float sufrevpass$edef_example$af(allocator *, Float ddr, ks::Tuple<>) { return ddr; }

Float dot$aT1fT1f(allocator *, tensor<1, Float> const& a, tensor<1, Float> const& b)
{
	return ts_dot(a,b);
}

tensor<1, Float>
mul$aT2fT1f(allocator * alloc, tensor<2, Float> const& M, tensor<1, Float> const& v)
{
	int r = M.outer_dimension();
	tensor<1, Float> ret(alloc, r);
	for(int i = 0; i < r; ++i)
		ret[i] = ts_dot(M[i], v);
	return ret;
}

Tuple<tensor<2, Float>,tensor<1, Float>>
rev$mul$aT2fT1f(allocator * alloc, Tuple<tensor<2, Float>, tensor<1, Float>> const& M_v, tensor<1, Float> const& dr)
{
	auto [M, v] = M_v;
	int r = M.outer_dimension();
	int c = size(v);
	tensor<2, Float> retM(alloc, size(M));
	for(int i = 0; i < r; ++i) {
		// Inlined retM[i].assign(ts_scale(dr[i], v))
		tensor<1, Float> retrow = retM[i];
		for (int j = 0; j < c; ++j)
			retrow[j] = dr[i] * v[j];
	}

	tensor<1, Float> retv(alloc, c);
	for(int i = 0; i < c; ++i) {
		Float retvi = 0;
		for(int j = 0; j < r; ++j)
			retvi += M.index(ks::make_Tuple(j, i)) * dr[j];
		retv[i] = retvi;
	}

	return ks::make_Tuple(retM,retv);
}

size_t imax$aT1f(allocator *, tensor<1, Float> const &v)
{
    KS_ASSERT(size(v) > 0);
    size_t imax = 0;
    Float vmax = v[imax];
    for (int i = 1; i < size(v); ++i)
        if (v[i] > vmax)
        {
            vmax = v[i];
            imax = i;
        }
    return imax;
}

Float max$aT1f(allocator * alloc, tensor<1, Float> const& v)
{
    return v[imax$aT1f(alloc, v)];
}

Float rev$lgamma$af(allocator *, Float x, Float dr)
{
	std::cerr << "rev$lgamma unimp!\n" << std::endl;
	throw "rev$gamma unimp!\n";
}
Float fwd$lgamma$af(allocator *, Float x, Float dx)
{
  if (dx == 0.0) {
    return 0.0;
  } else {
    std::cerr << "fwd$lgamma unimp!\n" << std::endl;
    throw "fwd$gamma unimp except at dx == 0!\n";
  }
}

Float pow$afi(allocator *, Float x, Integer e)
{
    return std::pow(x,e);
}

Tuple<> fwd$gt(allocator *, Float a,Float b,Float d$a,Float d$b)
{
    return Tuple<>();
}

Tuple<Float,Float> rev$gt(allocator *, Float a,Float b, Tuple<> d$r)
{
	std::cerr << "rev$gt unimp!\n" << std::endl;
	throw "rev$gt unimp!\n";
}

inline Float sub$aff(allocator *, Float t1, Float t2)
{
	return t1 - t2;
}

inline Integer sub$aii(allocator *, Integer t1, Integer t2)
{
	return t1 - t2;
}

inline Float div$aff(allocator *, Float t1, Float t2)
{
	return t1 / t2;
}

inline Integer div$aii(allocator *, Integer t1, Integer t2)
{
	return t1 / t2;
}

inline Float neg$af(allocator *, Float t)
{
	return -t;
}

inline Integer neg$ai(allocator *, Integer t)
{
	return -t;
}

inline Float exp$af(allocator *, Float d) { return exp(d); }
inline Float log$af(allocator *, Float d) { return log(d); }
inline Float sin$af(allocator *, Float d) { return sin(d); }
inline Float cos$af(allocator *, Float d) { return cos(d); }
inline Float cosh$af(allocator *, Float d) { return cosh(d); }
inline Float tanh$af(allocator *, Float d) { return tanh(d); }
inline Float lgamma$af(allocator *, Float d) { return lgamma(d); }
inline Float erf$af(allocator *, Float d) { return erf(d); }
inline Float sqrt$af(allocator *, Float d) { return sqrt(d); }

inline Float to_float$ai(allocator *, Integer d) { return d; }

inline Bool or$abb(allocator *, Bool b1, Bool b2)  { return b1 || b2; }
inline Bool and$abb(allocator *, Bool b1, Bool b2) { return b1 && b2; }
}

#include "knossos-prelude-lm.h"
