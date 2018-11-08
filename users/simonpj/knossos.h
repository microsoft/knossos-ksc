// C++ "runtime" for Knossos

#include <tuple>
#include <functional>
#include <sstream>
#include <cmath>

using std::get;
using std::make_tuple;
using std::tuple;

namespace ks
{
// Linear maps

namespace LM
{

template <class From, class To>
struct lm
{
    std::string ty;
};

template <class From, class To>
lm<From, To> lmOne()
{
    return lm<From, To>{"1"};
}

template <class From, class To>
lm<From, To> lmZero()
{
    return lm<From, To>{"0"};
}

template <class T>
std::string str(T val) {  
    std::ostringstream s;
    s << val;
    return s.str();
}

template <class T1, class T2>
lm<T1, T2> lmScale(T1 val)
{
    return lm<T1, T2>{"Scale(" + str(val) + ")"};
}

template <class From, class _, class To1, class To2>
lm<From, tuple<To1, To2>> lmVCat(lm<From, To1> const &lm1, lm<From, To2> const &lm2)
{
    return lm<From, tuple<To1, To2>>{"VCat(" + lm1.ty + "," + lm2.ty + ")"};
}

template <class _, class To, class From1, class From2>
lm<tuple<From1, From2>, To> lmHCat(lm<From1, To> const &lm1, lm<From2, To> const &lm2)
{
    return lm<tuple<From1, From2>, To>{"HCat(" + lm1.ty + "," + lm2.ty + ")"};
}

template <class A, class C, class B>
lm<A, C> lmCompose(lm<B, C> const &lm1, lm<A, B> const &lm2)
{
    return lm<A, C>{"Compose(" + lm1.ty + "," + lm2.ty + ")"};
}

template <class tupVecTT, class VecT, class T>
lm<tupVecTT, VecT> lmBuild(int v, std::function<lm<tupVecTT, T>(int)> t)
{
    return lm<tupVecTT, VecT> {"Build(" + str(v) + "," + t(v).ty + ")"};
}

template <class VecT, class tupVecTT, class T>
lm<VecT, tupVecTT> lmBuildT(int v, std::function<lm<tupVecTT, T>(int)> t)
{
    return lm<VecT, tupVecTT> {"BuildT(" + str(v) + "," + t(v).ty + ")"};
}

} // namespace LM

struct zero_t
{
};

// Functions defined polymorphically by Cgen.typeofFun
template <class T1, class T2>
T1 add(T1 t1, T2 t2) { return t1 + t2; }
template <class T1, class T2>
T1 sub(T1 t1, T2 t2) { return t1 - t2; }

template <class T1, class T2>
T1 mul(T1 t1, T2 t2) { return t1 * t2; }
template <class T1, class T2>
LM::lm<tuple<T1, T2>, T1> D$mul(T1 t1, T2 t2)
{
    return LM::lmHCat<tuple<T1, T2>, T1>(LM::lmScale<T1,T1>(t2), LM::lmScale<T2,T1>(t1));
}

template <class T1, class T2>
T1 div(T1 t1, T2 t2) { return t1 / t2; }
template <class T1, class T2>
LM::lm<tuple<T1, T2>, T1> D$div(T1 t1, T2 t2)
{
    return LM::lmHCat<tuple<T1, T2>, T1>(LM::lmScale<T1,T1>(1/t2), LM::lmScale<T2,T1>(-1.0/(t1*t1)));
}

template <class T1, class T2>
T1 eq(T1 t1, T2 t2) { return t1 == t2; }
template <class T1, class T2>
T1 lt(T1 t1, T2 t2) { return t1 < t2; }

template <typename T>
T delta(int i, int j, T)
{
    if (i == j)
        return T {1};
    else
      return T {0};
}

double neg(double d) { return -d; }

// ASSERT
#define ASSERT(expr)                        \
    if (expr)                               \
        ;                                   \
    else                                    \
    {                                       \
        throw("Assert failed [" #expr "]"); \
    }

// Vector builtins
template <class T>
struct vec
{
    int size;
    T *data;
};

template <class T>
int size(vec<T> v) { return v.size; }

template <class T>
T const &index(int t1, vec<T> t2)
{
    return t2.data[t1];
}

template <class T>
vec<T> build(int size, std::function<T(int)> f)
{
    vec<T> ret{size, new T[size]}; // To be replaced with DPS as appropriate
    for (int i = 0; i < size; ++i)
        ret.data[i] = f(i);
    return ret;
}

// Additional Vec functions
double sum(vec<double> v)
{
    double ret = 0.0;
    for (int i = 0; i < v.size; ++i)
        ret += v.data[i];
    return ret;
}

template <class T, int i, int n>
T selfun() {
  return 1.0;  
}

template <int i, int n>
LM::lm<double, double> D$selfun()
{
    return LM::lmOne<double,double>();
}

} // namespace ks

#include <iostream>
#include <string>

template <class T1, class T2>
std::ostream &operator<<(std::ostream &s, tuple<T1, T2> const &ts)
{
    return s << "tuple<" << std::get<0>(ts) << "," << std::get<1>(ts) << ">";
}

template <class T>
struct type_to_string
{
};
template <>
struct type_to_string<double>
{
    static std::string name() { return "double"; }
};

template <typename T1, typename T2>
struct type_to_string<tuple<T1, T2>>
{
    static std::string name()
    {
        return "tuple<" + type_to_string<T1>::name() + "," + type_to_string<T2>::name() + ">";
    }
};

template <typename T>
struct type_to_string<ks::vec<T>>
{
    static std::string name()
    {
        return "vec<" + type_to_string<T>::name() + ">";
    }
};

template <typename T1, typename T2>
struct type_to_string<ks::LM::lm<T1, T2>>
{
    static std::string name()
    {
        return "LM<" + type_to_string<T1>::name() + "," + type_to_string<T2>::name() + ">";
    }
};

template <class T1, class T2>
std::ostream &operator<<(std::ostream &s, ks::LM::lm<T1, T2> const &l)
{
    return s << type_to_string<ks::LM::lm<T1, T2>>::name() << "(" + l.ty + ")";
}

template <class T1>
ks::zero_t pr(T1 a)
{
    std::cout << "----\n";
    std::cout << "A= " << a << std::endl;
    return ks::zero_t{};
}
template <class T1, class T2>
ks::zero_t pr(T1 a, T2 b)
{
    std::cout << "----\n";
    std::cout << "A= " << a << std::endl;
    std::cout << "B= " << b << std::endl;
    return ks::zero_t{};
}
