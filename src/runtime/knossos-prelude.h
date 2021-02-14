// Defines everything in Knossos.ks

namespace ks {
// ===============================  Test edef  ========================
// These only exist so that we can test edef functionality.
// We should probably come up with a better story for the
// tests but at the time of writing I didn't want to hold back
// edef support any longer.
double edef_example$af(allocator *, double x) { return x; }
double fwd$edef_example$af(allocator *, double x, double dx) { return dx; }
double rev$edef_example$af(allocator *, double x, double ddr) { return ddr; }

vec<double>
mul$aT2fT1f(allocator * alloc, tensor<2, double> const& M, vec<double> const& v)
{
	int r = M.outer_dimension();
	vec<double> ret(alloc, r);
	for(int i = 0; i < r; ++i)
		ret[i] = ts_dot(M[i], v);
	return ret;
}

tuple<tensor<2, double>,vec<double>>
rev$mul$aT2fT1f(allocator * alloc, tuple<tensor<2, double>, vec<double>> const& M_v, vec<double> const& dr)
{
	auto [M, v] = M_v;
	int r = M.outer_dimension();
	int c = size(v);
	tensor<2, double> retM(alloc, size(M));
	for(int i = 0; i < r; ++i) {
		// Inlined retM[i].assign(ts_scale(dr[i], v))
		vec<double> retrow = retM[i];
		for (int j = 0; j < c; ++j)
			retrow[j] = dr[i] * v[j];
	}

	vec<double> retv(alloc, c);
	for(int i = 0; i < c; ++i) {
		double retvi = 0;
		for(int j = 0; j < r; ++j)
			retvi += M.index(ks::make_tuple(j, i)) * dr[j];
		retv[i] = retvi;
	}

	return ks::make_tuple(retM,retv);
}

size_t imax$aT1f(allocator *, vec<double> const &v)
{
    KS_ASSERT(size(v) > 0);
    size_t imax = 0;
    double vmax = v[imax];
    for (int i = 1; i < size(v); ++i)
        if (v[i] > vmax)
        {
            vmax = v[i];
            imax = i;
        }
    return imax;
}

double max$aT1f(allocator * alloc, vec<double> const& v)
{
    return v[imax$aT1f(alloc, v)];
}

double rev$lgamma$af(allocator *, double x, double dr)
{
	std::cerr << "rev$lgamma unimp!\n" << std::endl;
	throw "rev$gamma unimp!\n";
}
double fwd$lgamma$af(allocator *, double x, double dx)
{
  if (dx == 0.0) {
    return 0.0;
  } else {
    std::cerr << "fwd$lgamma unimp!\n" << std::endl;
    throw "fwd$gamma unimp except at dx == 0!\n";
  }
}

double pow$aFloat(allocator *, double x, double e)
{
    return std::pow(x,e);
}

double fwd$pow$aFloat(allocator *, double x, double e, double dx, double de)
{
	std::cerr << "fwd$pow unimp!\n" << std::endl;
	throw "fwd$pow unimp!\n";
}

tuple<double,double> rev$pow$aFloat(allocator *, double x, double e, double dr)
{
	std::cerr << "rev$pow unimp!\n" << std::endl;
	throw "rev$pow unimp!\n";
}

tuple<> fwd$gt(allocator *, double a,double b,double d$a,double d$b)
{
    return tuple<>();
}

tuple<double,double> rev$gt(allocator *, double a,double b, tuple<> d$r)
{
	std::cerr << "rev$gt unimp!\n" << std::endl;
	throw "rev$gt unimp!\n";
}

inline double sub$aff(allocator *, double t1, double t2)
{
	return t1 - t2;
}

inline int sub$aii(allocator *, int t1, int t2)
{
	return t1 - t2;
}

inline double div$aff(allocator *, double t1, double t2)
{
	return t1 / t2;
}

inline int div$aii(allocator *, int t1, int t2)
{
	return t1 / t2;
}

inline double neg$af(allocator *, double t)
{
	return -t;
}

inline int neg$ai(allocator *, int t)
{
	return -t;
}

inline double exp$af(allocator *, double d) { return exp(d); }
inline double log$af(allocator *, double d) { return log(d); }
inline double sin$af(allocator *, double d) { return sin(d); }
inline double cos$af(allocator *, double d) { return cos(d); }
inline double cosh$af(allocator *, double d) { return cosh(d); }
inline double tanh$af(allocator *, double d) { return tanh(d); }
inline double lgamma$af(allocator *, double d) { return lgamma(d); }

inline double to_float$ai(allocator *, int d) { return d; }

inline bool or$abb(allocator *, int b1, int b2)  { return b1 || b2; }
inline bool and$abb(allocator *, int b1, int b2) { return b1 && b2; }
}

#include "knossos-prelude-lm.h"
