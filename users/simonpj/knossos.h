// C++ "runtime" for Knossos

#include <tuple>
#include <functional>
#include <cmath>

using std::make_tuple;
using std::tuple;
using std::get;

namespace ks {
// Functions defined polymorphically by Cgen.typeofFun
template <class T1, class T2> T1 add(T1 t1, T2 t2) { return t1 + t2;}
template <class T1, class T2> T1 sub(T1 t1, T2 t2) { return t1 - t2;}
template <class T1, class T2> T1 mul(T1 t1, T2 t2) { return t1 * t2;}
template <class T1, class T2> T1 div(T1 t1, T2 t2) { return t1 / t2;}
template <class T1, class T2> T1  eq(T1 t1, T2 t2) { return t1 == t2;}
template <class T1, class T2> T1  lt(T1 t1, T2 t2) { return t1 < t2;}

double neg(double d) { return -d; }

// ASSERT
#define ASSERT(expr) if (expr) ; else { throw ("Assert failed [" #expr "]");} 

// Vector builtins
template <class T> 
struct vec { 
    int size; 
    T* data; 
};

template <class T> 
int size(vec<T> v) { return v.size; }

template <class T> 
T const& index(int t1, vec<T> t2) { 
    return t2.data[t1];
}

template <class T> 
vec<T> build(int size, std::function<T(int)> f) 
{ 
    vec<T> ret {size, new T[size]}; // To be replaced with DPS as appropriate
    for(int i = 0; i < size; ++i)
    ret.data[i] = f(i);
    return ret;
}

// Additional Vec functions
double sum(vec<double> v) { 
    double ret = 0.0;
    for(int i = 0; i < v.size; ++i)
        ret += v.data[i];
    return ret; 
}
}
