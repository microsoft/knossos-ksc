// Defines everything in Knossos.ks
#pragma once

#include "knossos.h"

namespace ks {
// ===============================  Test edef  ========================
// These only exist so that we can test edef functionality.
// We should probably come up with a better story for the
// tests but at the time of writing I didn't want to hold back
// edef support any longer.
inline KS_FUNCTION Float edef_example$af(allocator *, Float x) { return x; }
inline KS_FUNCTION Float fwd$edef_example$af(allocator *, Float x, Float dx) { return dx; }
inline KS_FUNCTION Float rev$edef_example$af(allocator *, Float x, Float ddr) { return ddr; }
inline KS_FUNCTION ks::Tuple<Float,ks::Tuple<>> suffwdpass$edef_example$af(allocator *, Float x) { return ks::make_Tuple(x, ks::make_Tuple()); }
inline KS_FUNCTION Float sufrevpass$edef_example$af(allocator *, Float ddr, ks::Tuple<>) { return ddr; }

inline KS_FUNCTION Bool lt$aff(allocator *, Float t1, Float t2)
{
	return t1 < t2;
}

inline KS_FUNCTION Bool lt$aii(allocator *, Integer t1, Integer t2)
{
	return t1 < t2;
}

inline KS_FUNCTION Bool gt$aff(allocator *, Float t1, Float t2)
{
	return t1 > t2;
}

inline KS_FUNCTION Bool gt$aii(allocator *, Integer t1, Integer t2)
{
	return t1 > t2;
}

inline KS_FUNCTION Bool lte$aff(allocator *, Float t1, Float t2)
{
	return t1 <= t2;
}

inline KS_FUNCTION Bool lte$aii(allocator *, Integer t1, Integer t2)
{
	return t1 <= t2;
}

inline KS_FUNCTION Bool gte$aff(allocator *, Float t1, Float t2)
{
	return t1 >= t2;
}

inline KS_FUNCTION Bool gte$aii(allocator *, Integer t1, Integer t2)
{
	return t1 >= t2;
}

inline KS_FUNCTION Float add$aff(allocator *, Float t1, Float t2)
{
	return t1 + t2;
}

inline KS_FUNCTION Integer add$aii(allocator *, Integer t1, Integer t2)
{
	return t1 + t2;
}

inline KS_FUNCTION Float mul$aff(allocator *, Float t1, Float t2)
{
	return t1 * t2;
}

inline KS_FUNCTION Integer mul$aii(allocator *, Integer t1, Integer t2)
{
	return t1 * t2;
}

inline KS_FUNCTION Float abs$af(allocator *, Float d) { return d > 0 ? d : -d; }

inline KS_FUNCTION Float max$aff(allocator *, Float a, Float b) { return a > b ? a : b; }

inline KS_FUNCTION Float dot$aT1fT1f(allocator *, tensor<1, Float> const& a, tensor<1, Float> const& b)
{
	return ts_dot(a,b);
}

inline KS_FUNCTION tensor<1, Float> mul$aT2fT1f(allocator * alloc, tensor<2, Float> const& M, tensor<1, Float> const& v)
{
	int r = M.outer_dimension();
	auto ret = tensor<1, Float>::create(alloc, r);
	for(int i = 0; i < r; ++i)
		ret[i] = ts_dot(M[i], v);
	return ret;
}

inline KS_FUNCTION Tuple<tensor<2, Float>,tensor<1, Float>>
rev$mul$aT2fT1f(allocator * alloc, Tuple<tensor<2, Float>, tensor<1, Float>> const& M_v, tensor<1, Float> const& dr)
{
	auto [M, v] = M_v;
	int r = M.outer_dimension();
	int c = size(v);
	auto retM = tensor<2, Float>::create(alloc, size(M));
	for(int i = 0; i < r; ++i) {
		// Inlined retM[i].assign(ts_scale(dr[i], v))
		tensor<1, Float> retrow = retM[i];
		for (int j = 0; j < c; ++j)
			retrow[j] = dr[i] * v[j];
	}

	auto retv = tensor<1, Float>::create(alloc, c);
	for(int i = 0; i < c; ++i) {
		Float retvi = 0;
		for(int j = 0; j < r; ++j)
			retvi += M.index(ks::make_Tuple(j, i)) * dr[j];
		retv[i] = retvi;
	}

	return ks::make_Tuple(retM,retv);
}

inline KS_FUNCTION size_t imax$aT1f(allocator *, tensor<1, Float> const &v)
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

inline KS_FUNCTION Float max$aT1f(allocator * alloc, tensor<1, Float> const& v)
{
    return v[imax$aT1f(alloc, v)];
}

inline KS_FUNCTION Float rev$lgamma$af(allocator *, Float x, Float dr)
{
	KS_ASSERT(false && "[rev lgamma] not implemented");
}
inline KS_FUNCTION Float fwd$lgamma$af(allocator *, Float x, Float dx)
{
	KS_ASSERT(dx == 0.0 && "[fwd lgamma] not implemented");
	return 0.0;
}

inline KS_FUNCTION Float pow$afi(allocator *, Float x, Integer e)
{
    return std::pow(x,e);
}

inline KS_FUNCTION Float sub$aff(allocator *, Float t1, Float t2)
{
	return t1 - t2;
}

inline KS_FUNCTION Integer sub$aii(allocator *, Integer t1, Integer t2)
{
	return t1 - t2;
}

inline KS_FUNCTION Float div$aff(allocator *, Float t1, Float t2)
{
	return t1 / t2;
}

inline KS_FUNCTION Integer div$aii(allocator *, Integer t1, Integer t2)
{
	return t1 / t2;
}

inline KS_FUNCTION Float neg$af(allocator *, Float t)
{
	return -t;
}

inline KS_FUNCTION Integer neg$ai(allocator *, Integer t)
{
	return -t;
}

inline KS_FUNCTION Float exp$af(allocator *, Float d) { return exp(d); }
inline KS_FUNCTION Float log$af(allocator *, Float d) { return log(d); }
inline KS_FUNCTION Float sin$af(allocator *, Float d) { return sin(d); }
inline KS_FUNCTION Float cos$af(allocator *, Float d) { return cos(d); }
inline KS_FUNCTION Float cosh$af(allocator *, Float d) { return cosh(d); }
inline KS_FUNCTION Float tanh$af(allocator *, Float d) { return tanh(d); }
inline KS_FUNCTION Float lgamma$af(allocator *, Float d) { return lgamma(d); }
inline KS_FUNCTION Float erf$af(allocator *, Float d) { return erf(d); }
inline KS_FUNCTION Float sqrt$af(allocator *, Float d) { return sqrt(d); }

inline KS_FUNCTION Float to_float$ai(allocator *, Integer d) { return d; }

inline KS_FUNCTION Bool or$abb(allocator *, Bool b1, Bool b2)  { return b1 || b2; }
inline KS_FUNCTION Bool and$abb(allocator *, Bool b1, Bool b2) { return b1 && b2; }
inline KS_FUNCTION Float bool_to_float$ab(allocator *, Bool b) { return b; }

// ranhash functions from
//     https://mathoverflow.net/questions/104915/pseudo-random-algorithm-allowing-o1-computation-of-nth-element
inline KS_FUNCTION uint64_t $ranhash(allocator *, uint64_t v) {
	v *= 3935559000370003845LL;
	v += 2691343689449507681LL;
	v ^= v >> 21; v ^= v << 37; v ^= v >> 4;
	v *= 4768777513237032717LL;
	v ^= v << 20; v ^= v >> 41; v ^= v << 5;
	return v;
}

inline KS_FUNCTION Float $ranhashdoub$ai(allocator * alloc, int32_t v) {
	return Float(5.42101086242752217E-20 * $ranhash(alloc, v));
}

}

#include "prelude-lm.h"
