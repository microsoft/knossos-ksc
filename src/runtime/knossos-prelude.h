// Defines everything in Knossos.ks

namespace ks {
// ===============================  Test edef  ========================
// These only exist so that we can test edef functionality.
// We should probably come up with a better story for the
// tests but at the time of writing I didn't want to hold back
// edef support any longer.
double edef_example$af(allocator *, double x) { return x; }
double fwd$edef_example$aff(allocator *, double x, double dx) { return dx; }
double rev$edef_example$aff(allocator *, double x, double ddr) { return ddr; }

double dot$aT1fT1f(allocator *, vec<double> const& a, vec<double> const& b)
{
	return dot(a,b);
}

vec<double>
mul$Mat$Vec$aT1T1fT1f(allocator * alloc, vec<vec<double>> const& M, vec<double> const& v)
{
	int r = size(M);
	vec<double> ret(alloc, r);
	for(int i = 0; i < r; ++i)
		ret[i] = ts_dot(M[i], v);
	return ret;
}

vec<double>
mul$aT2fT1f(allocator * alloc, tensor<2, double> const& M, vec<double> const& v)
{
	int r = M.outer_dimension();
	vec<double> ret(alloc, r);
	for(int i = 0; i < r; ++i)
		ret[i] = dot(M[i], v);
	return ret;
}

tuple<vec<vec<double>>,vec<double>> 
rev$mul$Mat$Vec$a$dT1T1fT1f$bT1f(allocator * alloc, std::tuple<vec<vec<double>>, vec<double>> const& M_v, vec<double> const& dr)
{
        auto [M, v] = M_v;
	int r = size(M);
	int c = size(v);
	vec<vec<double>> retM(alloc, r);
	for(int i = 0; i < r; ++i)
		retM[i] = ts_scale(alloc, dr[i], v);

	vec<double> retv(alloc, c);
	for(int i = 0; i < c; ++i) {
		double retvi = 0;
		for(int j = 0; j < r; ++j)
			retvi += M[j][i] * dr[j];
		retv[i] = retvi;
	}

	return std::make_tuple(retM,retv);
}

tuple<tensor<2, double>,vec<double>>
rev$mul$a$dT2fT1f$bT1f(allocator * alloc, std::tuple<tensor<2, double>, vec<double>> const& M_v, vec<double> const& dr)
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
			retvi += M.index(std::make_tuple(j, i)) * dr[j];
		retv[i] = retvi;
	}

	return std::make_tuple(retM,retv);
}

size_t imaximum(vec<double> const &v)
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

double maximum(vec<double> const& v) 
{
    return v[imaximum(v)];
}

double fwd$maximum(allocator *, vec<double> const& v, vec<double> const& dv) 
{
    std::cerr << "[fwd$maximum untested]";
    size_t i = imaximum(v);
    return dv[i];
}

vec<double> rev$maximum(allocator * alloc, vec<double> const& v, double dr)
{
    std::cerr << "[rev$maximum untested]";
    int i = imaximum(v);
    return deltaVec(alloc, size(v), i, dr);
}

double digamma(allocator *, double x)
{
	std::cerr << "digamma unimp!\n" << std::endl;
	throw "digamma unimp!\n";
}

double rev$lgamma$aff(allocator *, double x, double dr)
{
	std::cerr << "rev$lgamma unimp!\n" << std::endl;
	throw "rev$gamma unimp!\n";
}
double fwd$lgamma$aff(allocator *, double x, double dx)
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
inline double tanh$af(allocator *, double d) { return tanh(d); }
inline double lgamma$af(allocator *, double d) { return lgamma(d); }

inline double to_float$ai(allocator *, int d) { return d; }
inline double to_float(allocator *, int d) { return d; }

inline bool or$abb(allocator *, int b1, int b2)  { return b1 || b2; }
inline bool and$abb(allocator *, int b1, int b2) { return b1 && b2; }
}

#include "knossos-prelude-lm.h"
