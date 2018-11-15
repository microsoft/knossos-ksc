// C++ "runtime" for Knossos

#include <tuple>
#include <functional>
#include <set>
#include <sstream>
#include <iostream>
#include <cmath>
#include <string>
#include <variant>

using std::get;
using std::make_tuple;
using std::tuple;

// ASSERT
#define ASSERT(expr)                        \
    if (expr)                               \
        ;                                   \
    else                                    \
        ks::fail(__FILE__, __LINE__, #expr);

namespace ks {
	void fail(char const* file, int line, char const* expr)
	{
		std::cerr << file << ":" << line << ":Assert failed [" << expr << "]\n";
		throw expr;
	}
};

static int log_indent = 8;
static bool do_log = false;
#define LOG(msg, n) if (!do_log) ; else { std::cerr << std::string((n), ' ') << msg << ":" << __FUNCTION__ << " " << this << std::endl; }

std::set<void*> objects;
#define FIND (objects.find(this) != objects.end())
#define ENTER { objects.insert(this); LOG("ctor", log_indent++); }
#define NOTE { LOG("note " << (FIND ? (void*)this : (void*)0), log_indent); }
#define LEAVE { LOG("dtor " << FIND, --log_indent);  objects.erase(this); }


namespace ks
{
	// Allocator
	struct allocator {
		size_t max_size;
		unsigned char* buf;
		size_t top;

		allocator(size_t max_size) :
			max_size(max_size),
			buf(new unsigned char[max_size]),
			top(0)
		{}
		~allocator() {
			delete[] buf;
		}
		void* alloc(size_t size)
		{
			ASSERT(size < 1000);
			void* ret = buf + top;
			top += ((size + 15) / 16) * 16;
			ASSERT(top < max_size);
			return ret;
		}

		void reset()
		{
			top = 0;
		}
	};

	allocator g_alloc{ 10000000 };

	// Zero
	struct zero_t
	{
		// Be alert for "double to int" warnings on this type -- they can mask things like a vector constructor's size argument
		explicit operator double() { return 0.0; }
		explicit operator int() { return 0; }
	};



	// Functions defined polymorphically by Cgen.typeofFun
	template <class T1, class T2>
	T1 add(T1 t1, T2 t2) { return t1 + t2; }

	template <class T2>
	T2 add(zero_t t1, T2 t2) { return t2; }

	template <class T1>
	T1 add(T1 t1, zero_t t2) { return t1; }

	template <class T1, class T2>
	T1 sub(T1 t1, T2 t2) { return t1 - t2; }

	// ===============================  Tuple utils  ==================================

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

	// Prepend at the type level, e.g. 
	// decltype(prepend(A{}, B{}))
	template <class A, class B>
	struct tuple_prepend {
	};

	template <class T, class... Ts>
	struct tuple_prepend<T, tuple<Ts...>> {
		typedef tuple<T, Ts...> type;
	};

	template <size_t i, class... Ts>
	std::ostream& tuple_print(std::ostream& s, tuple<Ts...> const& t)
	{
		if constexpr(i < sizeof...(Ts))
		{
			s << std::get<i>(t);
			if (i > 0) s << ",";
			return tuple_print<i + 1>(s, t);
		}
		return s;
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

#define DECLARE_TYPE_TO_STRING(Tmpl, T)                                        \
	template <class T>                                                         \
	struct type_to_string<Tmpl<T>>                                             \
	{                                                                          \
		static std::string name()                                              \
		{                                                                      \
			return std::string(#Tmpl) + "<" + type_to_string<T>::name() + ">"; \
		}                                                                      \
	};

#define DECLARE_TYPE_TO_STRING2(Tmpl, T1, T2)                                                                      \
	template <class T1, class T2>                                                                                  \
	struct type_to_string<Tmpl<T1, T2>>                                                                            \
	{                                                                                                              \
		static std::string name()                                                                                  \
		{                                                                                                          \
			return std::string(#Tmpl) + "<" + type_to_string<T1>::name() + "," + type_to_string<T2>::name() + ">"; \
		}                                                                                                          \
	};

#define DECLARE_TYPE_TO_STRING_Pack(Tmpl)                         \
	template <class... Ts>                                        \
	struct type_to_string<Tmpl<Ts...>>                            \
	{                                                             \
		static std::string name()                                 \
		{                                                         \
			typedef typename Tmpl<Ts...>::Tup Tup;                \
			std::ostringstream s;                                 \
			s << #Tmpl << "<" <<                                    \
			ks::type_to_string<Tup>::name() << ">"; \
			return s.str(); \
		}                                                         \
	};

	// Vector builtins
	template <class T>
	class vec
	{
#ifdef BUMPY
//TODO Full set of 5 ctors/assigners
		size_t size_;
		T* data_;
#else
		std::vector<T> data_;
#endif

		// Private ctor -- use create
		vec(size_t  size) : data_(size) {}

	public:
#ifdef BUMPY
		vec() :size_(0), data_(0) {}
		int size() const { return size_; }
#else
		vec() {}
		int size() const { return data_.size(); }
#endif

		T& operator[](int i) { return data_[i]; }
		T const& operator[](int i) const { return data_[i]; }

		static vec<T> create(size_t size);
	};

	template <typename T>
	struct type_to_string<ks::vec<T>>
	{
		static std::string name()
		{
			return "vec<" + type_to_string<T>::name() + ">";
		}
	};


	template <class T>
	int size(vec<T> const & v)
	{
		return v.size(); 
	}

	template <class T>
	T const &index(int i, vec<T> const & v)
	{
		ASSERT(i >= 0);
		ASSERT(i < v.size());
		return v[i];
	}

	template <class T>
	vec<T> vec<T>::create(size_t size)
	{
		ASSERT(size < 1000); // Quick sanity check -- later we may well have larger vecs
#ifdef BUMPY
		vec<T> ret;
		void *storage = g_alloc.alloc(sizeof(T) * size);
		ret.size_ = size;
		ret.data_ = (T*)storage;
		return ret;
#else
		return vec{ size };
#endif
	}

	template <class T, class F>
	vec<T> build(size_t size, F f)
	{
		vec<T> ret = vec<T>::create(size);

		for (size_t i = 0; i < size; ++i)
			ret[i] = T{ f(i) }; // Explicit ctor for zero_t
		return ret;
	}

	// Additional Vec functions
	template <class T>
	vec<T> add(vec<T> const& a, vec<T> const& b)
	{
		ASSERT(a.size() == b.size());
		vec<T> ret = vec<T>::create(a.size());

		for (int i = 0; i < a.size(); ++i)
			ret[i] = add(a[i], b[i]);
		return ret;
	}

	// Additional Vec functions
	template <class T>
	T sum(vec<T> const& v)
	{
		if (v.size() == 0) throw std::string("oik");
		if (v.size() == 1) return v[0];
		T ret = add(v[0], v[1]);
		for (int i = 2; i < v.size(); ++i)
			ret = add(ret, v[i]);
		return ret;
	}


	// Linear maps
	namespace LM
	{

		// LM operators

		// ---------------- One  ------------------
		template <class T>
		struct One
		{
			typedef T To;
			typedef T From;

			static One mk(T) { return One{}; }

			To Apply(From f) const { return f; }
		};

		template <class T>
		std::ostream &operator<<(std::ostream &s, One<T> const &t)
		{
			return s << "One" <<
				//"<" << type_to_string<T>::name() << ">"
				"";
		}

		// ---------------- Zero  ------------------
		template <class From_t, class To_t>
		struct Zero
		{
			typedef To_t To;
			typedef From_t From;

			static Zero mk(From, To) { return Zero{}; }

			zero_t Apply(From f) const { 
				return zero_t { }; 
			}
		};

		template <class From, class To>
		std::ostream &operator<<(std::ostream &s, Zero<From, To> const &t)
		{
			return s << "Zero" <<
				//"<" << type_to_string<From>::name() << "," << type_to_string<From>::name() << ">"
				"";
		}

		// ---------------- Scale  ------------------
		template <class T>
		T Scale_aux(T t, double);

		// General case for Scale_aux<tuple>.
		// Scale first element, then scale tail.
		template <class... Ts>
		tuple<Ts...> Scale_aux(tuple<Ts...> t, double val)
		{
			return prepend(Scale_aux(std::get<0>(t), val),
		  				   Scale_aux(tail(t), val));
		}

		// Base case for Scale_aux<Tuple>.  Empty tuple * val = empty tuple.
		tuple<> Scale_aux(tuple<> t, double val)
		{
			return t;
		}

		// Scale a vec
		template <class T>
		vec<T> Scale_aux(vec<T> const& v, double val)
		{
			int size = v.size();
			vec<T> ret = vec<T>::create(size); // To be replaced with DPS as appropriate
			for (int i = 0; i < size; ++i)
				ret[i] = Scale_aux(v[i], val);
			return ret;
		}

		// Scale anything else
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

			static Scale mk(To, double val) { return Scale{ val }; }

			To Apply(From f) const { return To{ Scale_aux(f, val) }; }
		};

		template <class T>
		std::ostream &operator<<(std::ostream &s, Scale<T> const &t)
		{
			return s << "Scale" <<
				"<" << type_to_string<T>::name() << ">" <<
				"(" << t.val << ")";
		}

		// ---------------- Add ------------------
		template <class LM1, class LM2>
		struct Add {
			LM1 lm1;
			LM2 lm2;

			typedef typename LM1::From From1;
			typedef typename LM1::To To1;

			typedef typename LM2::From From2;
			typedef typename LM2::To To2;

			static_assert(std::is_same<From1, From2>::value, "To1==To2");
			static_assert(std::is_same<To1, To2>::value, "To1==To2");

			typedef From1 From;
			typedef To1 To;

			static Add mk(LM1 lm1, LM2 lm2) { return Add{ lm1, lm2 }; }

			To Apply(From f) const { return add(lm1.Apply(f), lm2.Apply(f)); }
		};

		template <class T1, class T2>
		std::ostream &operator<<(std::ostream &s, Add<T1, T2> const &t)
		{
			return s << "Add" <<
				//"<" << type_to_string<T1>::name() << "," << type_to_string<T2>::name() << ">" <<
				"(" << t.lm1 << "," << t.lm2 << ")";
		}

		// ---------------- HCat ------------------

		template <class T>
		struct HCat_infer_From {
		};

		template <>
		struct HCat_infer_From<tuple<>> {
			typedef tuple<> type;
		};

		template <class T, class... Ts>
		struct HCat_infer_From<tuple<T, Ts...>> {
			typedef typename T::From From;
			typedef typename HCat_infer_From<tuple<Ts...>>::type Froms;
			typedef typename tuple_prepend<From, Froms>::type type;
		};

		namespace test_HCat_infer_From {
			struct A {
				typedef int From;
			};
			struct B {
				typedef float From;
			};
			typedef typename HCat_infer_From<tuple<A, B, A>>::type ABA;
			static_assert(std::is_same<ABA, tuple<int, float, int>>::value);
		};

		template <class T>
		struct HCat_infer_To {
			typedef typename std::tuple_element<0, T>::type T0;
			typedef typename T0::To type;
		};

		template <class... LMs>
		struct HCat {
			static constexpr size_t n = sizeof...(LMs);

			typedef tuple<LMs...> Tup;

			Tup lms;

			typedef typename HCat_infer_To<Tup>::type To;
			typedef typename HCat_infer_From<Tup>::type From;

			static HCat mk(LMs... lm) { return HCat{ { lm... } }; }

#define APP(i) std::get<i>(lms).Apply(std::get<i>(f))

			template <size_t i>
			To Apply_aux(To accum, From const& f) const {
				if constexpr (i < n)
					return Apply_aux<i + 1>(add(accum, APP(i)), f);
				else
					return accum;
			}

			template <size_t i>
			To Apply_aux(zero_t accum, From const& f) const {
				if constexpr (i < n)
					return Apply_aux<i + 1>(/*add zero to ..*/ APP(i), f);
				else
					return accum;
			}

			To Apply(From const& f) const {
				static_assert(n > 1);
				return Apply_aux<1>(APP(0), f);
			}
#undef APP
		};

		
		template <class T, class... Ts>
		std::ostream &operator<<(std::ostream &s, HCat<T, Ts...> const &t)
		{
			return s << "HCat"
				//"<" << type_to_string<T1>::name() << "," << type_to_string<T2>::name() << ">" <<
				;
			return tuple_print<0>(s << "(", t.lms) << ")";
		}

		// ---------------- VCat ------------------
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
			typedef tuple<To1, To2>  To;

			static VCat mk(LM1 lm1, LM2 lm2) { return VCat { lm1, lm2 }; }

			To Apply(From f) const { return std::make_tuple(lm1.Apply(f), lm2.Apply(f)); }

		};

		template <class T1, class T2>
		std::ostream &operator<<(std::ostream &s, VCat<T1, T2> const &t)
		{
			return s << "VCat" <<
				//"<" << type_to_string<T1>::name() << "," << type_to_string<T2>::name() << ">" <<
				"(" << t.lm1 << "," << t.lm2 << ")";
		}

		// ---------------- Compose ------------------
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

			static Compose mk(Lbc bc, Lab ab) { return Compose{ bc, ab }; }

			To Apply(From f) const {
				auto g = ab.Apply(f);
				return bc.Apply(g); 
			}
		};

		template <class T1, class T2>
		std::ostream &operator<<(std::ostream &s, Compose<T1, T2> const &t)
		{
			return s << "Compose" <<
				//"<" << type_to_string<T1>::name() << "," << type_to_string<T2>::name() << ">" <<
				"(" << t.bc << "," << t.ab << ")";
		}

		// ---------------- SelFun ------------------
		template <typename Tuple, typename Ti>
		struct SelFun
		{
			typedef Tuple From;
			typedef Ti To;

			int index;

			static SelFun mk(int index, int n) { return SelFun{ index }; }

			To Apply(From f) const { return std::get(f, index); }
		};

		template <class Tuple, class Ti>
		std::ostream &operator<<(std::ostream &s, SelFun<Tuple, Ti> const &t)
		{
			return s << "SelFun" <<
				//  "<" << type_to_string<T1>::name() << "," << type_to_string<T2>::name() << ">" <<
				"(" << t.index << ")";
		}


		// ---------------- Build ------------------
		template <typename L>
		struct Build
		{
			typedef typename L::To LTo;

			typedef typename L::From From;
			typedef vec<LTo> To;

			size_t n;
			std::function<L(size_t)> f;

			template <class Functor>
			static Build mk(size_t n, Functor f) { 
				ASSERT(n != 0);
				return Build{ n, f }; 
			}

			To Apply(From x) const 
			{ 
				std::function<L(size_t)> f1 = f;
				return build<LTo>(n, [x,f1](int i) {
													 auto lm = f1(i);
													 return lmApply(lm, x);
												   }); 
			}
		};

		template <class L>
		std::ostream &operator<<(std::ostream &s, Build<L> const &t)
		{
			return s << "Build" <<
				"<" << type_to_string<L>::name() << ">" <<
				"(" << t.n << ", <ftype>)";
		}

		// ---------------- BuildT ------------------
		template <typename L>
		struct BuildT
		{
			typedef typename L::From LFrom;

			typedef vec<typename L::From> From;
			typedef typename L::To To;

			size_t n;
			std::function<L(size_t)> f;

			BuildT() : n{ 0 }
			{
				ENTER; //  std::cerr << std::string(3 + indent++, '.') << "[build() " << this << "]\n";
			}

			BuildT(const BuildT& that) :
				n(that.n),
				f(that.f)
			{
				ENTER;
			}

			BuildT& operator=(const BuildT& that)
			{
				n = that.n;
				f = that.f;
				return *this;
				NOTE("copy");
			}

			template <class Functor>
			BuildT(size_t n, Functor f) :
				n(n),
				f(f)
			{
				ENTER;
				//std::cerr << std::string(3+indent++, '.') << "[build " << this << "]\n";
			}

			~BuildT()
			{
				LEAVE;
				//std::cerr << std::string(3+ --indent, '.') << "[~build " << this << "]\n";
			}

			template <class Functor>
			static BuildT mk(size_t n, Functor f) { return BuildT{ n, f }; }

			To Apply(From x) const
			{
				if (n != x.size())
					std::cerr << "BuildT:" << n << " != " << x.size() << std::endl;
				ASSERT(n == x.size());        // TODO: copying arrays here -- should not need to..
				std::function<L(size_t)> f_local = f;
				return sum(build<LFrom>(n, [f_local,x](int i) { return lmApply(f_local(i), x[i]); }));
			}
		};

		template <class L>
		std::ostream &operator<<(std::ostream &s, BuildT<L> const &t)
		{
			return s << "BuildT" <<
				"<" << type_to_string<L>::name() << ">" <<
				"(" << t.n << ", <ftype>)";
		}

		// ---------------- Variant ------------------
		template <class L1, class L2>
		struct Variant
		{
			std::variant<L1, L2> v;

			typedef typename L1::From From;
			typedef typename L1::To To;

			static_assert(std::is_same<typename L1::From, typename L2::From>::value, "To1==To2");
			static_assert(std::is_same<typename L1::To, typename L2::To>::value, "To1==To2");

			//TODO
			//Variant& operator=(const L1& l1) { v = l1; return *this; }
			//Variant& operator=(const L2& l2) { v = l2; return *this; }

			To Apply(From f) const {
				if (v.index() == 0) return To { std::get<0>(v).Apply(f) };
				if (v.index() == 1) return To { std::get<1>(v).Apply(f) };
				throw std::string("Bad Variant");
			}
		};

		template <size_t n, class T, class... Ts>
		struct variant_print {
			static std::ostream& go(std::ostream &s, std::variant<T, Ts...> const & v)
			{
				if (v.index() == n - 1)
					s << std::get<n - 1>(v);
				return variant_print<n - 1, T, Ts...>::go(s, v);
			}
		};

		template <class T, class... Ts>
		struct variant_print<0, T, Ts...> {
			static std::ostream& go(std::ostream &s, std::variant<T, Ts...> const & v)
			{
				return s;
			}
		};

		template <class T, class... Ts>
		std::ostream &operator<<(std::ostream &s, std::variant<T, Ts...> const & v)
		{
			constexpr size_t n = std::variant_size<std::variant<T, Ts...>>::value;
			return variant_print<n, T, Ts...>::go(s, v);
		}

		template <class T1, class T2>
		std::ostream &operator<<(std::ostream &s, Variant<T1, T2> const &t)
		{
			return s << "Variant" <<
				//"<" << type_to_string<T1>::name() << "," << type_to_string<T2>::name() << ">" <<
				"{" << t.v << "}";
		}

		// ---------------- lmApply ------------------
		template <class LM, class A>
		auto lmApply(LM lm, A a) {
			return lm.Apply(a);
		}

	} // namespace LM

	DECLARE_TYPE_TO_STRING(LM::One, T);
	DECLARE_TYPE_TO_STRING2(LM::Zero, From, To);
	DECLARE_TYPE_TO_STRING(LM::Scale, T);
	DECLARE_TYPE_TO_STRING(LM::Build, L);
	DECLARE_TYPE_TO_STRING(LM::BuildT, L);
	DECLARE_TYPE_TO_STRING_Pack(LM::HCat);
	DECLARE_TYPE_TO_STRING2(LM::VCat, From, To);
	DECLARE_TYPE_TO_STRING2(LM::Compose, From, To);
	DECLARE_TYPE_TO_STRING2(LM::Variant, From, To);
	DECLARE_TYPE_TO_STRING2(LM::Add, From, To);

	// Gradients of standard fns
	template <class T1, class T2>
	auto D$sub(T1 t1, T2 t2)
	{
		return LM::HCat<LM::Scale<T1>, LM::Scale<T1>>::mk({ 1.0 }, { -1.0 });
	}


	template <class T1, class T2>
	T1 mul(T1 t1, T2 t2)
	{
		return t1 * t2;
	}

	template <class T1, class T2>
	auto D$mul(T1 t1, T2 t2)
	{
		typedef LM::Scale<T1> M1;
		typedef LM::Scale<T2> M2;
		return LM::HCat<M1, M2>::mk(M1::mk(t2), M2::mk(t1));
	}

	template <class T1, class T2>
	T1 div(T1 t1, T2 t2)
	{
		return t1 / t2;
	}

	auto D$div(double t1, double t2)
	{
		typedef double T1;
		typedef double T2;
		// TODO: Not sure this is correct for anything other than scalars
		return LM::HCat<LM::Scale<T1>, LM::Scale<T2>>::mk(LM::Scale<T1>::mk(T1{}, 1.0 / t2), LM::Scale<T1>::mk(T1{}, -1.0 / (t1*t1)));
	}

	template <class T1, class T2>
	T1 eq(T1 t1, T2 t2)
	{
		return t1 == t2;
	}

	template <class T1, class T2>
	T1 lt(T1 t1, T2 t2)
	{
		return t1 < t2;
	}

	template <typename T>
	T delta(int i, int j, T)
	{
		if (i == j)
			return T{ 1 };
		else
			return T{ 0 };
	}

	double neg(double d) { return -d; }

	double to_float(int d) { return d; }
	auto D$to_float(int d) { return LM::Zero<int, double>(); }

	// TODO: parameter packs for these
	template <typename T1, typename T2>
	T1 selfun$2_1(tuple<T1, T2> ts) {
		return get<0>(ts);
	}

	template <typename T1, typename T2>
	T2 selfun$2_2(tuple<T1, T2> ts) {
		return get<1>(ts);
	}

	template <typename T1, typename T2>
	auto D$selfun$2_1(T1, T2)
	{
		typedef LM::One<T1> L1;
		typedef LM::Zero<T2, T1> L2;
		return LM::HCat<L1, L2>::mk(L1::mk(), L2::mk());
	}

	template <typename T1, typename T2>
	auto D$selfun$2_2(T1, T2)
	{
		typedef LM::Zero<T1, T2> L1;
		typedef LM::One<T2> L2;
		return LM::HCat<L1, L2>::mk(L1::mk(), L2::mk());
	}

} // namespace ks

template <class T>
std::ostream &operator<<(std::ostream &s, ks::vec<T> const &v)
{
	s << "[";
	for (int i = 0; i < v.size(); ++i)
		s << (i > 0 ? ", " : "") << v[i];
	return s << "]";
}

template <class T>
ks::zero_t pr(T a)
{
	std::cout << a << std::endl;
	return ks::zero_t{};
}

template <class T, class... Ts>
ks::zero_t pr(T a, Ts... t)
{
	pr(a);
	std::cout << "----\n";
	return pr(t...);
}
