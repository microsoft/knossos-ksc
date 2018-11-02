// C++ "runtime" for Knossos

#include <tuple>
#include <functional>
#include <cmath>

using std::make_tuple;
using std::tuple;
using std::get;

// Functions defined polymorphically by Cgen.typeofFun
template <class T1, class T2> T1 add(tuple<T1, T2> t) { return get<0>(t) + get<1>(t);}
template <class T1, class T2> T1 sub(tuple<T1, T2> t) { return get<0>(t) - get<1>(t);}
template <class T1, class T2> T1 mul(tuple<T1, T2> t) { return get<0>(t) * get<1>(t);}
template <class T1, class T2> T1 div(tuple<T1, T2> t) { return get<0>(t) / get<1>(t);}
template <class T1, class T2> T1  eq(tuple<T1, T2> t) { return get<0>(t) == get<1>(t);}
template <class T1, class T2> T1  lt(tuple<T1, T2> t) { return get<0>(t) < get<1>(t);}

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
T const& index(tuple<int, vec<T>> t) { 
    return get<1>(t).data[get<0>(t)];
}

template <class T> 
vec<T> build0(int size, std::function<T(int)> f) 
{ 
    vec<T> ret {size, new T[size]}; // To be replaced with DPS as appropriate
    for(int i = 0; i < size; ++i)
    ret.data[i] = f(i);
    return ret;
}

template <class T> 
vec<T> build(tuple<int, std::function<T(int)>> t) 
{ 
    return build0(get<0>(t), get<1>(t)); 
}

// Additional Vec functions

double sum(vec<double> v) { 
    double ret = 0.0;
    for(int i = 0; i < v.size; ++i)
        ret += v.data[i];
    return ret; 
}
