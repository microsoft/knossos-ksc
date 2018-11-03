// C++ "runtime" for Knossos

#include <tuple>
#include <functional>
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
    struct lm {
    };

    // Future: return tag types for 1,0,vcat etc.
    template <class From, class To>
    lm<From, To> lmOne() {
        return lm<From, To> {};
    }

    // Future: return tag types for 1,0,vcat etc.
    template <class From, class _, class To1, class To2>
    lm<From, tuple<To1, To2>> lmVCat(lm<From, To1> const& lm1, lm<From, To2> const & lm2) {
        return lm<From, tuple<To1, To2>> {};
    }

    template <class A, class C, class B>
    lm<A,C> lmCompose(lm<B, C> const& lm1, lm<A, B> const & lm2) {
        return lm<A, C> {};
    }

    } // namespace LM

    struct zero_t {};

// Functions defined polymorphically by Cgen.typeofFun
template <class T1, class T2>
T1 add(T1 t1, T2 t2) { return t1 + t2; }
template <class T1, class T2>
T1 sub(T1 t1, T2 t2) { return t1 - t2; }

template <class T1, class T2>
T1 mul(T1 t1, T2 t2) { return t1 * t2; }
template <class T1, class T2>
LM::lm<tuple<T1, T2>, T1> D$mul(T1 t1, T2 t2) {
    return LM::lm<tuple<T1, T2>, T1>{};
}

template <class T1, class T2>
T1 div(T1 t1, T2 t2) { return t1 / t2; }
template <class T1, class T2>
T1 eq(T1 t1, T2 t2) { return t1 == t2; }
template <class T1, class T2>
T1 lt(T1 t1, T2 t2) { return t1 < t2; }

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

} // namespace ks


#include <iostream>
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
    std::cout << "B= " << a << std::endl;
    return ks::zero_t{};
}

