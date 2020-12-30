#include <cstddef>
#include <tuple>
using std::tuple;
typedef tuple<> void_t;
template <class F> class ks_function; // not defined
template <class F, class BOG> struct DerivedFunctionsPack;
// How to get the tangent_type of T
template <class T>
struct ks_tangent_type {
    typedef T type;
};

// Simple wrapper for C-style function pointer T (*f)(S), no closures
template <class S, class... T>
struct ks_function<S(T...)> {
   S (*f)(T...);
   S operator()(T... t...) { return f(t...); }
};

// ^^^ consider the above to be unimportant boilerplate (for talking about DF tables)

template <class S, class T, class BOG>
struct DerivedFunctionsPack<S(T), BOG> {
    typedef typename ks_tangent_type<S>::type dS;
    typedef typename ks_tangent_type<T>::type dT;

    ks_function<T (S)>                f;         
    ks_function<T (S, S)>             fwd$f;
    ks_function<dS (S, dT)>           rev$f;
    ks_function<tuple<T,BOG> (S)>     fwdpass$f;
    ks_function<dS (BOG, dT)>         revpass$f;
    ks_function<size_t (S)>           bytes$f;
//    ks_function<shape<T> (absint<S>)> shape$f;
};

extern double cosh(double);
extern double rev$cosh(double, double);
DerivedFunctionsPack<double(double), void_t> DF$cosh = {
    cosh,
    0,
    rev$cosh,
    0,
    0,
    0
};

#include <vector>

template<class T>
using Vec = std::vector<T>;

Vec<double> map(double f(double), Vec<double> xs) 
{
    Vec<double> ret(xs.size());
    for(size_t i = 0; i < xs.size(); ++i)
        ret[i] = f(xs[i]);
    return ret;
}

template <class S, class T, class BOG>
Vec<double> rev$map(DerivedFunctionsPack<S(T), BOG> f, Vec<double> xs, Vec<double> dret) 
{
    Vec<double> ret(xs.size());
    for(size_t i = 0; i < xs.size(); ++i)
        ret[i] = f.rev$f(xs[i], dret[i]);
    return ret;
}

double cosh(double x) { return x * x; }
double rev$cosh(double x, double dr) { return 2*x*dr; }
#include <iostream>
std::ostream& operator<<(std::ostream& s, Vec<double> const& v) {
    s << "[";
    for(auto i : v) s << " " << i;
    return s << " ]\n";
}

int main()
{
    Vec<double> v = {1.1,2.2,3.3};
    Vec<double> ret = map(cosh, v);    
    std::cout << ret;

    Vec<double> dret = {0.01,0.01,0.01};
    auto dv = rev$map(DF$cosh, v, dret);
    std::cout << dv;
}
