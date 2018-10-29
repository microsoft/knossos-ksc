// C++ "runtime" for Knossos

#include <tuple>
#include <functional>
#include <cmath>

using std::make_tuple;
using std::tuple;
using std::get;

template <class T> 
struct vec { 
    int size; 
    T* data; 
};



double mul(tuple<double,double> arg) { return get<0>(arg) * get<1>(arg); }
double add(tuple<double,double> arg) { return get<0>(arg) + get<1>(arg); }
double div(tuple<double,double> arg) { return get<0>(arg) / get<1>(arg); }
double sub(tuple<double,double> arg) { return get<0>(arg) - get<1>(arg); }
double neg(double d) { return -d; }
double exp(double d) { return std::exp(d); }
double log(double d) { return std::log(d); }

template <typename T1, typename T2>
bool ks_equals(tuple<T1,T2> t) { return get<0>(t) == get<1>(t); }

template <typename T1, typename T2>
bool ks_less(tuple<T1,T2> t) { return get<0>(t) < get<1>(t); }


double sum(vec<double> v) { 
    double ret = 0.0;
    for(int i = 0; i < v.size; ++i)
        ret += v.data[i];
    return ret; 
}

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
