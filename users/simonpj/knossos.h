// C++ "runtime" for Knossos

#include <tuple>
#include <functional>
#include <sstream>
#include <iostream>
#include <cmath>
#include <string>
#include <variant>

using std::get;
using std::make_tuple;
using std::tuple;

namespace ks
{
// Functions defined polymorphically by Cgen.typeofFun
template <class T1, class T2>
T1 add(T1 t1, T2 t2) { return t1 + t2; }

template <class T1, class T2>
T1 sub(T1 t1, T2 t2) { return t1 - t2; }

// Tuple utils
template < typename T, typename... Ts >
auto head(std::tuple<T, Ts...> t)
{
	return  std::get<0>(t);
}

template < std::size_t... Ns, typename... Ts >
auto tail_impl(std::index_sequence<Ns...>, std::tuple<Ts...> t)
{
	return  std::make_tuple(std::get<Ns + 1u>(t)...);
}

template < typename... Ts >
auto tail(std::tuple<Ts...> t)
{
	return  tail_impl(std::make_index_sequence<sizeof...(Ts) - 1u>(), t);
}

template <typename T, typename... Ts >
auto prepend(T t, tuple<Ts...> tup)
{
	return std::tuple_cat(std::make_tuple(t), tup);
}

// val to string
template <class T>
std::string str(T val) {  
    std::ostringstream s;
    s << val;
    return s.str();
}

// Type to string

template <class T>
struct type_to_string
{
};
template <>
struct type_to_string<double>
{
    static std::string name() { return "double"; }
};

template <>
struct type_to_string<tuple<>>
{
    static std::string name()
    {
        return "tuple<>";
    }
};

template <typename T>
struct type_to_string_aux {};

template <typename T, typename... Ts>
struct type_to_string_aux<tuple<T, Ts...>>
{
    static std::string name()
    {
        return type_to_string<T>::name() + "," + type_to_string_aux<tuple<Ts...>>::name();
    }

};
template <typename T>
struct type_to_string_aux<tuple<T>>
{
    static std::string name()
    {
        return type_to_string<T>::name();
    }

};

template <typename... Ts>
struct type_to_string<tuple<Ts...>>
{
    static std::string name()
    {
        return "tuple<" + type_to_string_aux<tuple<Ts...>>::name() + ">";
    }
};

// Vector builtins
template <class T>
struct vec
{
    int size;
    T *data;
};

template <typename T>
struct type_to_string<ks::vec<T>>
{
    static std::string name()
    {
        return "vec<" + type_to_string<T>::name() + ">";
    }
};

#define DECLARE_TYPE_TO_STRING(T) \
    struct type_to_string<T>  \
    {                             \
        static std::string name() \
        {                         \
            return #T;            \
        }                         \
    };
#define DECLARE_TYPE_TO_STRING2(T1,T2) \
    struct type_to_string<T1,T2>  \
    {                             \
        static std::string name() \
        {                         \
            return #T1 "," #T2;            \
        }                         \
    };

template <class T>
int size(vec<T> v) { return v.size; }

template <class T>
T const &index(int t1, vec<T> t2)
{
    return t2.data[t1];
}

template <class T, class F>
vec<T> build(int size, F f)
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


// Linear maps
namespace LM
{
// LM operators
template <class T>
struct One
{
    typedef T To;
    typedef T From;

    static One mk() { return One { }; }

    To Apply(From f) const { return f; }
};

template <class From_t, class To_t>
struct Zero
{
    typedef To_t To;
    typedef From_t From;

    static Zero mk() { return Zero { }; }

    To Apply(From f) const { return To { 0 }; }
};

// Scale
template <class T>
T Scale_aux(T t, double);

template <class... Ts>
tuple<Ts...> Scale_aux(tuple<Ts...> t, double val)
{
	return prepend(Scale_aux(std::get<0>(t), val), 
				   Scale_aux(tail(t), val));
}


tuple<> Scale_aux(tuple<> t, double val)
{
	return t;
}

template <class T>
vec<T> Scale_aux(vec<T> v, double val)
{
    int size = v.size;
    vec<T> ret{size, new T[size]}; // To be replaced with DPS as appropriate
    for (int i = 0; i < size; ++i)
        ret.data[i] = Scale_aux(v.data[i], val);
    return ret;
}

template <class T>
T Scale_aux(T t, double val)
{
	return t * val;
}

template <class T>
struct Scale
{
    double val;

    typedef T To;
    typedef T From;

    static Scale mk(double val) { return Scale { val }; }

	To Apply(From f) const { return To { Scale_aux(f, val) }; }
};

template <class LM1, class LM2>
struct HCat {
    LM1 lm1;
    LM2 lm2;

    typedef typename LM1::From From1;
    typedef typename LM1::To To1;

    typedef typename LM2::From From2;
    typedef typename LM2::To To2;

    static_assert(std::is_same<To1, To2>::value, "To1==To2");

    typedef tuple<From1,From2> From;
    typedef To1 To;

    static HCat mk(LM1 lm1, LM2 lm2) { return HCat { lm1, lm2}; }

    To Apply(From f) const { return add(lm1.Apply(std::get<0>(f)), lm2.Apply(std::get<1>(f))); }

};

template <class LM1, class LM2>
struct VCat {
    LM1 lm1;
    LM2 lm2;

    typedef typename LM1::From From1;
    typedef typename LM1::To To1;

    typedef typename LM2::From From2;
    typedef typename LM2::To To2;

    static_assert(std::is_same<From1, From2>::value, "To1==To2");

    typedef From1 From;
    typedef tuple<To1,To2>  To;

    static VCat mk(LM1 lm1, LM2 lm2) { return VCat { lm1, lm2}; }

    To Apply(From f) const { return std::make_tuple(lm1.Apply(f), lm2.Apply(f)); }

};

template <class Lbc, class Lab>
struct Compose
{
    Lbc bc;
    Lab ab;

    typedef typename Lbc::From B1;
    typedef typename Lbc::To C;

    typedef typename Lab::From A;
    typedef typename Lab::To B2;

    static_assert(std::is_same<B1, B2>::value, "To1==To2");

    typedef A From;
    typedef C To;

    static Compose mk(Lbc bc, Lab ab) { return Compose { bc, ab }; }

    To Apply(From f) const { return bc.Apply(ab.Apply(f)); }
};

template <typename Tuple, typename Ti>
struct SelFun
{
    typedef Tuple From;
    typedef Ti To;

    int index;

    static SelFun mk(int index, int n) { return SelFun { index }; }

    To Apply(From f) const { return std::get(f,index); }
};

template <class LM, class A>
auto lmApply(LM lm, A a) {
    return lm.Apply(a);
}

} // namespace LM

template <class T>
DECLARE_TYPE_TO_STRING(LM::One<T>);

template <class From, class To>
DECLARE_TYPE_TO_STRING2(LM::Zero<From,To>);

struct zero_t
{
    operator double() { return 0.0; }
};

// Gradients of standard fns
template <class T1, class T2>
auto D$sub(T1 t1, T2 t2)
{
    return 1;// LM::HCat<tuple<T1, T2>, T1>(LM::lmOne<T1,T1>(), LM::lmScale<T2,T1>(-1.0));
}


template <class T1, class T2>
T1 mul(T1 t1, T2 t2) { return t1 * t2; }
template <class T1, class T2>
auto D$mul(T1 t1, T2 t2)
{
    typedef LM::Scale<T1> M1;
    typedef LM::Scale<T2> M2;
    return LM::HCat<M1,M2>::mk(M1::mk(t2), M2::mk(t1));
}

template <class T1, class T2>
T1 div(T1 t1, T2 t2) { return t1 / t2; }
template <class T1, class T2>
auto D$div(T1 t1, T2 t2)
{
    return 1;//LM::lmHCat<tuple<T1, T2>, T1>(LM::lmScale<T1,T1>(1/t2), LM::lmScale<T2,T1>(-1.0/(t1*t1)));
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
auto D$to_float(int d) { return LM::Zero<int,double>(); }

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
auto D$selfun$2_1(T1,T2)
{
    typedef LM::One<T1> L1;
    typedef LM::Zero<T2,T1> L2;
    return LM::HCat<L1,L2>::mk(L1::mk(), L2::mk());
}

template <typename T1, typename T2>
auto D$selfun$2_2(T1,T2)
{
    typedef LM::Zero<T1,T2> L1;
    typedef LM::One<T2> L2;
    return LM::HCat<L1,L2>::mk(L1::mk(), L2::mk());
}

} // namespace ks

template <class T1, class T2>
std::ostream &operator<<(std::ostream &s, tuple<T1, T2> const &ts)
{
    return s << "tuple<" << std::get<0>(ts) << "," << std::get<1>(ts) << ">";
}

template <class T>
std::ostream &operator<<(std::ostream &s, ks::vec<T> const &v)
{
    for (int i = 0; i < v.size; ++i)
      s << (i == 0 ? "[" : ", ") << v.data[i];
    return s << "]";
}

template <class T>
ks::zero_t pr(T a)
{
    std::cout << "A= " << a << std::endl;
    return ks::zero_t{};
}

template <class T, class... Ts>
ks::zero_t pr(T a, Ts... t)
{
    std::cout << "----\n";
    pr(a);
    return pr(t...);
}

