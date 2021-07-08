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
ks::tuple<Float,ks::tuple<>> suffwdpass$edef_example$af(allocator *, Float x) { return ks::make_tuple(x, ks::make_tuple()); }
Float sufrevpass$edef_example$af(allocator *, Float ddr, ks::tuple<>) { return ddr; }

Float dot$aT1fT1f(allocator *, vec<Float> const& a, vec<Float> const& b)
{
	return ts_dot(a,b);
}

vec<Float>
mul$aT2fT1f(allocator * alloc, tensor<2, Float> const& M, vec<Float> const& v)
{
	int r = M.outer_dimension();
	vec<Float> ret(alloc, r);
	for(int i = 0; i < r; ++i)
		ret[i] = ts_dot(M[i], v);
	return ret;
}

tuple<tensor<2, Float>,vec<Float>>
rev$mul$aT2fT1f(allocator * alloc, tuple<tensor<2, Float>, vec<Float>> const& M_v, vec<Float> const& dr)
{
	auto [M, v] = M_v;
	int r = M.outer_dimension();
	int c = size(v);
	tensor<2, Float> retM(alloc, size(M));
	for(int i = 0; i < r; ++i) {
		// Inlined retM[i].assign(ts_scale(dr[i], v))
		vec<Float> retrow = retM[i];
		for (int j = 0; j < c; ++j)
			retrow[j] = dr[i] * v[j];
	}

	vec<Float> retv(alloc, c);
	for(int i = 0; i < c; ++i) {
		Float retvi = 0;
		for(int j = 0; j < r; ++j)
			retvi += M.index(ks::make_tuple(j, i)) * dr[j];
		retv[i] = retvi;
	}

	return ks::make_tuple(retM,retv);
}

size_t imax$aT1f(allocator *, vec<Float> const &v)
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

Float max$aT1f(allocator * alloc, vec<Float> const& v)
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

Float pow$afi(allocator *, Float x, int e)
{
    return std::pow(x,e);
}

tuple<> fwd$gt(allocator *, Float a,Float b,Float d$a,Float d$b)
{
    return tuple<>();
}

tuple<Float,Float> rev$gt(allocator *, Float a,Float b, tuple<> d$r)
{
	std::cerr << "rev$gt unimp!\n" << std::endl;
	throw "rev$gt unimp!\n";
}

inline Float sub$aff(allocator *, Float t1, Float t2)
{
	return t1 - t2;
}

inline int sub$aii(allocator *, int t1, int t2)
{
	return t1 - t2;
}

inline Float div$aff(allocator *, Float t1, Float t2)
{
	return t1 / t2;
}

inline int div$aii(allocator *, int t1, int t2)
{
	return t1 / t2;
}

inline Float neg$af(allocator *, Float t)
{
	return -t;
}

inline int neg$ai(allocator *, int t)
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

inline Float to_float$ai(allocator *, int d) { return d; }

inline bool or$abb(allocator *, int b1, int b2)  { return b1 || b2; }
inline bool and$abb(allocator *, int b1, int b2) { return b1 && b2; }
}

#include "knossos-prelude-lm.h"
