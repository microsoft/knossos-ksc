// C++ "runtime" for Knossos

#include <tuple>
#include <functional>
#include <sstream>
#include <iostream>
#include <cmath>
#include <string>

using std::get;
using std::make_tuple;
using std::tuple;

namespace ks
{
// Linear maps

namespace LM
{

struct Nop {};

template <class From, class To>
struct lm
{
    std::string ty;
};

template <class From, class To>
To lmApply(lm<From, To>, From)
{
    return To {};
}

template <class From, class To>
struct One
{
    To Apply(From& f) { return f; }
};

template <class From, class To>
struct Zero
{
    To Apply(From& f) { return To { 0 }; }
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

template <class _, class To, class From>
lm<From, To> lmAdd(lm<From, To> const &lm1, lm<From, To> const &lm2)
{
    return lm<From, To>{"Add(" + lm1.ty + "," + lm2.ty + ")"};
}

template <class _, class To, class From1, class From2>
lm<tuple<From1, From2>, To> lmHCat(lm<From1, To> const &lm1, lm<From2, To> const &lm2)
{
    return lm<tuple<From1, From2>, To>{"HCat(" + lm1.ty + "," + lm2.ty + ")"};
}

#define A(n) lm<F##n, To> const&lm##n
template <class _, class To, class F1, class F2, class F3, class F4, class F5, class F6, class F7>
lm<tuple<F1, F2, F3, F4, F5, F6, F7>, To> lmHCat(A(1), A(2), A(3), A(4), A(5), A(6), A(7))
{
    return lm<tuple<F1, F2, F3, F4, F5, F6, F7>, To>{"HCat(" + lm1.ty + "," + lm2.ty + ")"};
}
#undef A

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
    operator double() { return 0.0; }
};

// Functions defined polymorphically by Cgen.typeofFun
template <class T1, class T2>
T1 add(T1 t1, T2 t2) { return t1 + t2; }

template <class T1, class T2>
T1 sub(T1 t1, T2 t2) { return t1 - t2; }
template <class T1, class T2>
LM::lm<tuple<T1, T2>, T1> D$sub(T1 t1, T2 t2)
{
    return LM::lmHCat<tuple<T1, T2>, T1>(LM::lmOne<T1,T1>(), LM::lmScale<T2,T1>(-1.0));
}


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

double to_float(int d) { return d; }
auto D$to_float(int d) { return LM::lmScale<int,double>(d); }

// ASSERT
#define ASSERT(expr)                        \
    if (expr)                               \
        ;                                   \
    else                                    \
        ks::fail(__FILE__, __LINE__, #expr);

void fail(char const* file, int line, char const* expr)
{
    std::cerr << file << ":" << line << ":Assert failed [" << expr << "]\n";
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

// TODO: parameter packs for these
template <typename T1, typename T2>
T1 selfun$2_1(tuple<T1,T2> ts) {
  return get<0>(ts);  
}

template <typename T1, typename T2>
T2 selfun$2_2(tuple<T1,T2> ts) {
  return get<1>(ts);  
}

template <typename T1, typename T2>
LM::lm<tuple<T1,T2>, T1> D$selfun$2_1(T1,T2)
{
    return LM::lmHCat<tuple<T1,T2>, T1>(LM::lmOne<T1,T1>(), LM::lmZero<T2,T1>());
}

template <typename T1, typename T2>
LM::lm<tuple<T1,T2>, T2> D$selfun$2_2(T1,T2)
{
    return LM::lmHCat<tuple<T1,T2>, T2>(LM::lmZero<T1,T2>(), LM::lmOne<T2,T2>());
}

} // namespace ks

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

template <typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7>
struct type_to_string<tuple<T1, T2, T3, T4, T5, T6, T7>>
{
    static std::string name()
    {
        return "tuple<" + type_to_string<T1>::name() + ","
                        + type_to_string<T2>::name() + ","
                        + type_to_string<T3>::name() + ","
                        + type_to_string<T4>::name() + ","
                        + type_to_string<T5>::name() + ","
                        + type_to_string<T6>::name() + ","
                        + type_to_string<T7>::name() + ">";
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

template <class T>
std::ostream &operator<<(std::ostream &s, ks::vec<T> const &v)
{
    for (int i = 0; i < v.size; ++i)
      s << (i == 0 ? "[" : ", ") << v.data[i];
    return s << "]";
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

template <class T1, class T2, class T3>
ks::zero_t pr(T1 a, T2 b, T3 c)
{
    std::cout << "----\n";
    std::cout << "A= " << a << std::endl;
    std::cout << "B= " << b << std::endl;
    std::cout << "C= " << c << std::endl;
    return ks::zero_t{};
}
