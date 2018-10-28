// C++ "runtime" for Knossos

#include <tuple>
#include <functional>
#include <cmath>

bool ks_fail() {
    throw "OIK";
    return true;
}

#define ASSERT(X) ((X) || ks_fail())

using std::make_tuple;
using std::tuple;
using std::get;

template <class T> 
struct vec { 
    int size; 
    T* data; 
};

typedef double R;

int add$int$int(tuple<int,int> arg) { return get<0>(arg) + get<1>(arg);; }
int add$R$int(tuple<R,int> arg) { return get<0>(arg) + get<1>(arg);; }
R mul(tuple<R,R> arg) { return get<0>(arg) * get<1>(arg); }
R add(tuple<R,R> arg) { return get<0>(arg) + get<1>(arg); }
R div(tuple<R,R> arg) { return get<0>(arg) / get<1>(arg); }
R sub(tuple<R,R> arg) { return get<0>(arg) - get<1>(arg); }
R neg(R d) { return -d; }
R exp(R d) { return std::exp(d); }
R log(R d) { return std::log(d); }

template <typename T1, typename T2>
bool ks_equals(tuple<T1,T2> t) { return get<0>(t) == get<1>(t); }

template <typename T1, typename T2>
bool ks_less(tuple<T1,T2> t) { return get<0>(t) < get<1>(t); }


R sum(vec<R> v) { 
    R ret = 0.0;
    for(int i = 0; i < v.size; ++i)
        ret += v.data[i];
    return ret; 
}

template <class T> 
int size(vec<T> v) { return v.size; }

template <class T> 
T const& index(tuple<vec<T>, int> t) { 
    return get<0>(t).data[get<1>(t)];
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


// F# defines
