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
template <class From, class To>
struct Nop 
{
    To Apply(From f) const { return To {}; }
};

template <class From, class To>
struct One
{
    To Apply(From f) const { return f; }
};

template <class From, class To>
struct Zero
{
    To Apply(From f) const { return To { 0 }; }
};

template <class From, class To, class Opab, class Opbc>
struct Compose
{
    Opbc opbc;
    Opab opab;
    To Apply(From f) const { return opbc.Apply(opab.Apply(f)); }
};
}

template <class From, class To>
DECLARE_TYPE_TO_STRING2(LM::Nop<From,To>);

template <class From, class To>
DECLARE_TYPE_TO_STRING2(LM::One<From,To>);

template <class From, class To>
DECLARE_TYPE_TO_STRING2(LM::Zero<From,To>);


namespace LM {

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

template <class From, class To>
struct Scale
{
    double val;

	To Apply(From f) const { return To { Scale_aux(f, val) }; }
};


template <class From, class To, class Op1, class Op2>
struct VariantOp
{
    union {
        Op1 op1;
        Op2 op2;
    } ops;
    int which;

    VariantOp() {
        which = 0;
    }

    VariantOp& operator=(Op1 const& o) {
        which = 1;
        ops.op1 = o;
    }
    VariantOp& operator=(Op2 const& o) {
        which = 2;
        ops.op2 = o;
    }

	To Apply(From f) const 
    { 
        if (which == 1) return ops.op1.Apply(f); 
        if (which == 2) return ops.op2.Apply(f); 
        throw std::exception("zoiks");
    }
};

template <class From, class To, class Op>
struct lm
{
    std::string ty;
	Op op;

    typedef lm<From,To,Op> this_t;
    template <class U>
    this_t& operator=(U that){
        ty = that.ty;
        op = that.op;
    }
};

template <class From, class To, class Op>
To lmApply(lm<From,To,Op> m, From f)
{
    return m.op.Apply(f);
}

template <class From, class To>
auto lmOne()
{
    return lm<From, To, One<From, To>>{"1"};
}

template <class From, class To>
auto lmZero()
{
    return lm<From, To, Zero<From, To>>{"0"};
}

template <class T1, class T2>
auto lmScale(double val)
{
    return lm<T1, T2, Scale<T1,T2>>{"Scale(" + str(val) + ")", val};
}

template <class From, class To, class Op1, class To1, class Op2, class To2>
auto lmVCat(lm<From, To1, Op1> const &lm1, lm<From, To2, Op2> const &lm2)
{
    return lm<From, tuple<To1, To2>, Nop<From,To>>{"VCat(" + lm1.ty + "," + lm2.ty + ")"};
}

template <class From, class To, class Op1, class Op2>
auto lmAdd(lm<From, To, Op1> const &lm1, lm<From, To, Op2> const &lm2)
{
    return lm<From, To, Nop<From, To>>{"Add(" + lm1.ty + "," + lm2.ty + ")"};
}

template <class From, class To, class From1, class Op1, class From2, class Op2>
auto lmHCat(lm<From1, To, Op1> const &lm1, lm<From2, To, Op2> const &lm2)
{
    return lm<tuple<From1, From2>, To, Nop<From,To>>{"HCat(" + lm1.ty + "," + lm2.ty + ")"};
}

template <class From, class To, class F1, class... Fs>
auto lmHCat(Fs... as)
{
    return lm<From, To, Nop<From,To>>{"HCat(...)"};
}

template <class A, class C, class B, class OpBC, class OpAB>
auto lmCompose(lm<B, C, OpBC> const &lmbc, lm<A, B, OpAB> const &lmab)
{
    typedef Compose<A,C,OpAB,OpBC> Op_t;
    return lm<A, C, Op_t>{"Compose(" + lmbc.ty + "," + lmab.ty + ")", {lmbc.op, lmab.op} };
}
/*
template <class A, class C, class B, class OpBC>
auto lmCompose(lm<B, C, OpBC> const &lm1, lm<A, B, Zero<A, B>> const &lm2)
{
    return lmZero<A, C>();
}

template <class A, class C, class B, class OpAB>
auto lmCompose(lm<B, C, Zero<B,C>> const &lm1, lm<A, B, OpAB> const &lm2)
{
    return lmZero<A, C>();
}
*/
template <class tupVecTT, class VecT, class T, class Functor>
auto lmBuild(int v, Functor t)
{
    return lm<tupVecTT, VecT, Nop<tupVecTT, VecT>> {"Build(" + str(v) + "," + t(0).ty + ")"};
}

template <class VecT, class tupVecTT, class T, class Functor>
auto lmBuildT(int v, Functor t)
{
    return lm<VecT, tupVecTT, Nop<VecT, tupVecTT>> {"BuildT(" + str(v) + "," + t(0).ty + ")"};
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
auto D$sub(T1 t1, T2 t2)
{
    return LM::lmHCat<tuple<T1, T2>, T1>(LM::lmOne<T1,T1>(), LM::lmScale<T2,T1>(-1.0));
}


template <class T1, class T2>
T1 mul(T1 t1, T2 t2) { return t1 * t2; }
template <class T1, class T2>
auto D$mul(T1 t1, T2 t2)
{
    return LM::lmHCat<tuple<T1, T2>, T1>(LM::lmScale<T1,T1>(t2), LM::lmScale<T2,T1>(t1));
}

template <class T1, class T2>
T1 div(T1 t1, T2 t2) { return t1 / t2; }
template <class T1, class T2>
auto D$div(T1 t1, T2 t2)
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
    return LM::lmHCat<tuple<T1,T2>, T1>(LM::lmOne<T1,T1>(), LM::lmZero<T2,T1>());
}

template <typename T1, typename T2>
auto D$selfun$2_2(T1,T2)
{
    return LM::lmHCat<tuple<T1,T2>, T2>(LM::lmZero<T1,T2>(), LM::lmOne<T2,T2>());
}

} // namespace ks

template <class T1, class T2>
std::ostream &operator<<(std::ostream &s, tuple<T1, T2> const &ts)
{
    return s << "tuple<" << std::get<0>(ts) << "," << std::get<1>(ts) << ">";
}

template <typename T1, typename T2, typename Op>
struct ks::type_to_string<ks::LM::lm<T1, T2, Op>>
{
    static std::string name()
    {
        return "LM<" + type_to_string<T1>::name() + "," + type_to_string<T2>::name() + "," + type_to_string<Op>::name() + ">";
    }
};

template <class T1, class T2, class Op>
std::ostream &operator<<(std::ostream &s, ks::LM::lm<T1, T2, Op> const &l)
{
    return s << ks::type_to_string<ks::LM::lm<T1, T2, Op>>::name() << "\n   (" + l.ty + ")";
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

