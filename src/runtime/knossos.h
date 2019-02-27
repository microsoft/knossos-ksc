// C++ "runtime" for Knossos
#pragma once

#include <assert.h>
#include <type_traits>
#include <variant>
#include <tuple>
#include <set>
#include <functional>
#include <sstream>
#include <iostream>
#include <random>
#include <cmath>
#include <string>
#include <chrono>

using std::get;
using std::make_tuple;
using std::tuple;

// Enable bump allocator.  See mark/reset below.
#define BUMPY

#define COMMENT(x)

// ASSERT
#define ASSERT(expr)                        \
    if (expr)                               \
        ;                                   \
    else                                    \
        ks::fail(__FILE__, __LINE__, #expr);

namespace ks {
	inline void fail(char const* file, int line, char const* expr)
	{
		std::cerr << file << ":" << line << ":Assert failed [" << expr << "]\n";
		throw expr;
	}
};

namespace ks
{
	extern int log_indent;
	extern bool do_log;
#define LOG(msg, n) if (!do_log) ; else { std::cerr << std::string((n), ' ') << msg << ":" << __FUNCTION__ << " " << this << std::endl; }

	extern std::set<void*> objects;
#define FIND (objects.find(this) != objects.end())
#define ENTER { objects.insert(this); LOG("ctor", log_indent++); }
#define NOTE { LOG("note " << (FIND ? (void*)this : (void*)0), log_indent); }
#define LEAVE { LOG("dtor " << FIND, --log_indent);  objects.erase(this); }

        // Inplace add requires a couple of forward declarations.
        // This seemed to be the minimal we could get away with.
        template <class T>
        class vec;
        template <class T>
        void inplace_add(vec<T> *t1, const vec<T> &t2);
        void inplace_add(double *t1, const double &t2);

	// ===============================  String utils  ==================================

	// val to string
	template <class T>
	std::string str(T val) {
		std::ostringstream s;
		s << val;
		return s.str();
	}

	// ===============================  Type to string  ==================================

	template <class T>
	struct type_to_string
	{
		static std::string name() { return typeid(T).name(); }
	};

	template <>
	struct type_to_string<double>
	{
		static std::string name() { return "double"; }
	};

	// Specialize for some types we use
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

#define DECLARE_TYPE_TO_STRING3(Tmpl, T1, T2, T3)                                                                      \
	template <class T1, class T2, class T3>                                                                                  \
	struct type_to_string<Tmpl<T1, T2, T3>>                                                                            \
	{                                                                                                              \
		static std::string name()                                                                                  \
		{                                                                                                          \
			return std::string(#Tmpl) + "<" + type_to_string<T1>::name() + "," + type_to_string<T2>::name() + type_to_string<T3>::name() + ">"; \
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

	inline void
	test_type_to_string()
	{
		auto s = type_to_string<tuple<int, float>>::name();
		ASSERT(s == "tuple<int,float>");
	}

	// ===============================  Allocator  ==================================
#ifdef BUMPY
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
			ASSERT(size < 1000000);
			void* ret = buf + top;
			top += ((size + 15) / 16) * 16;
			ASSERT(top < max_size);
			return ret;
		}

		size_t mark() { return top;  }

		void reset(size_t top_ = 0)
		{
			top = top_;
		}
	};

	extern allocator g_alloc;
	typedef size_t alloc_mark_t;
	inline alloc_mark_t mark_bump_allocator_if_present() { return g_alloc.mark(); }
	inline void reset_bump_allocator_if_present(alloc_mark_t top) { g_alloc.reset(top); }
#else
	typedef size_t alloc_mark_t;
	inline alloc_mark_t mark_bump_allocator_if_present() { return 0; }
	inline void reset_bump_allocator_if_present(alloc_mark_t top) { }
#endif


	// ===============================  Zero  ==================================
	template <typename T>
	struct zero_t
	{
		zero_t(T) { }
		
		zero_t() { }

		// operator T() { return T {}; }
	};

	template <>
	struct zero_t<double>
	{
		zero_t(double) { }

		zero_t() { }

		operator double() { return 0.0; }
	};


	template <>
	struct zero_t<int>
	{
		zero_t(int) { }

		zero_t() { }

		operator int() { return 0; }
	};

	template <>
	struct zero_t<tuple<>>
	{
		zero_t(tuple<>) { }

		zero_t() { }

		operator tuple<>() { return tuple<>{}; }
	};

	template <class T>
	std::ostream &operator<<(std::ostream &s, ks::zero_t<T> const &v)
	{
		return s << "zero_t<" << type_to_string<T>::name() << ">{}";
	}

	template <class T>
	bool is_zero(T)
	{
		return false;   // It might be tempting to put a runtime check in here, but beware of lost static optimization opportunities
	}

	template <typename T>
	bool is_zero(zero_t<T>)
	{
		return true;
	}

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
			if (i > 0) s << ",";
			s << std::get<i>(t);
			tuple_print<i + 1>(s, t);
		}
		return s;
	}

	template <class... Ts>
	std::ostream& operator<<(std::ostream& s, tuple<Ts...> const& t)
	{
		return tuple_print<0>(s, t);
	}

	// =============================== Type conversion ============================
	template <class T>
	struct convert 
	{
		template <class U>
		static T go(U const& u) {
			return static_cast<T>(u);
		}
	};

	template <>
	struct convert<double> 
	{
		static double go(zero_t<double> const&) {
			return 0.0;
		}
	};

	template <>
	struct convert<int> 
	{
		static int go(zero_t<int> const&) {
			return 0;
		}
	};

	template<>
	struct convert<tuple<>> 
	{
		static tuple<> go(zero_t<tuple<>> const&) {
			return tuple<>{};
		}
	};

	template <class T>
	struct convert<zero_t<T>> 
	{
		static zero_t<T> go(zero_t<zero_t<T>> const& u) {
			return zero_t<T>{};
		}
	};

	template <class T0, class... Ts>
	struct convert<tuple<T0, Ts...>> 
	{
		template <class U0, class... Us>
		static tuple<T0, Ts...> go(zero_t<tuple<U0, Us...>> const& u)
		{
			return prepend(convert<T0>::go(zero_t<U0>{}), convert<tuple<Ts...>>::go(zero_t<tuple<Us...>>{}));
		}

		template <class U0, class... Us>
		static tuple<T0, Ts...> go(tuple<U0, Us...> const& u)
		{
			return prepend(convert<T0>::go(head(u)), convert<tuple<Ts...>>::go(tail(u)));
		}
	};

	// ===============================  Addition ==================================
	// Adding is special because it needs to be defined before linear maps,
	// And needs to be defined before any primitives are used in linear maps.
	// ... At least for gcc, which seems to require more info at template decl time.

	template <class T1, class T2>
	T1 add(T1 t1, T2 t2) { return t1 + t2; }

	template <class T2>
	T2 add(zero_t<T2> t1, T2 t2) { return t2; }

	template <class T1>
	T1 add(T1 t1, zero_t<T1> t2) { return t1; }

	template <class T1>
        zero_t<T1> add(zero_t<T1> t1, zero_t<T1> t2) { return t1; }

	template <>
	inline tuple<> add(tuple<> t1, tuple<> t2)
	{
		return tuple<>{};
	}

	template <class T0, class... Ts, class U0, class... Us>
	auto add(tuple<T0, Ts...> t1, tuple<U0, Us...> t2)
	{
		return prepend(add(head(t1), head(t2)),
			add(tail(t1), tail(t2)));
	}

	// ===============================  Vector class ==================================

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
		// Runtime flag indicating this is an all-zero vector.  
		// TODO: Ideally this would propagate nicely through the type system, 
		// without a runtime flag but, need to pass initializer lists through
		// zero_t {}
		// As is, many instances are collapsed statically and the remainder 
		// come throug here.
		bool is_zero_ = false;  

	public:

		typedef T value_type;

#ifdef BUMPY
		vec(): 
			size_{ 0 },
			data_{ nullptr },
			is_zero_{ true }
		{
		}

		vec(size_t  size) {
			void *storage = g_alloc.alloc(sizeof(T) * size);
			this->size_ = size;
			this->data_ = (T*)storage;
			this->is_zero_ = false;
		}

		template <class U>
		vec(vec<U> const& that) : vec{ that.size() }
		{
			if (that.is_zero_)
				this->is_zero = true;
			else {
				assert(!this->is_zero_);
				for (int i = 0; i < that.size(); ++i)
					this->data_[i] = that[i];
			}
		}

		vec(std::vector<T> const& that) : vec{ that.size() }
		{
			for (int i = 0; i < that.size(); ++i)
				data_[i] = that[i];
		}

		int size() const { return int(size_); }

		vec& operator=(const vec<T>& that)
		{
			ASSERT(that.size() != 0 || that.is_zero_);
			this->size_ = that.size_;
			this->data_ = that.data_; // Yes, this will alias, but there is no mutation, and no deletion.
			this->is_zero_ = that.is_zero_;
			return *this;
		}

#else
		vec() {}

		vec(size_t  size) : data_(size), is_zero_(false) {}

		vec(std::vector<T> const& that) :data_{ that } {}

		int size() const { return static_cast<int>(data_.size()); }

		vec& operator=(const vec<T>& that)
		{
			ASSERT(that.size() != 0 || that.is_zero_);
			data_ = that.data_;
			is_zero_ = that.is_zero_;
			return *this;
		}

#endif

		vec(zero_t<vec<T>>):
			size_ {0},
			data_ {0},
			is_zero_ {true}
		{
		}

		T& operator[](int i) { return data_[i]; }
		T const& operator[](int i) const { return data_[i]; }
	
		static vec<T> create(size_t size);

		bool is_zero() const { return is_zero_; }

                friend void inplace_add <>(vec<T> *t1, const vec<T> &t2);
	};

	// ===============================  Inplace add ==================================
        void inplace_add(double *t1, const double &t2) { *t1 += t2; }

        inline void inplace_add(tuple<> *t1, const tuple<> &t2)
	{
            return;
	}

	template <class T0, class... Ts, class U0, class... Us>
	void inplace_add(tuple<T0, Ts...> *t1, const tuple<U0, Us...> &t2)
	{
            auto ht1 = head(*t1);
            auto tt1 = tail(*t1);
            inplace_add(&ht1, head(t2));
            inplace_add(&tt1, tail(t2));
	}

	template <class T2>
        void inplace_add(zero_t<T2> *t1, const zero_t<T2> &t2) { return; }

        template <class T>
        void inplace_add(vec<T> *t1, const vec<T> &t2)
        {
            ASSERT(t2.size() != 0 || t2.is_zero_);
            ASSERT(t1->size() == t2.size()
                   || t1->is_zero()
                   || t2.is_zero());

            if (t2.is_zero_) return;

            if (t1->is_zero_) *t1 = t2;

            for (int i = 0; i < t2.size(); ++i)
              ks::inplace_add(&t1->data_[i], t2[i]);
        }

	template <class T>
	int size(vec<T> const & v)
	{
		return v.size(); 
	}

	template <class T>
	T const &index(int i, vec<T> const & v)
	{
		static T z{ zero_t<T> {} };
		if (v.is_zero())
			return z;

#ifndef xxx_NDEBUG
		if (i >= v.size()) {
			std::cerr << "ERROR: Accessing element " << i << " of vec of length " << v.size() << std::endl;
			throw "oiks";
		} 
#endif

		return v[i];
	}

	template <class T>
	vec<T> vec<T>::create(size_t size)
	{
		return vec{ size };
	}

	template <class T, class F>
	vec<T> build(int size, F f)
	{
		vec<T> ret = vec<T>::create(size);

		for (int i = 0; i < size; ++i)
			ret[i] = T { f(i) }; 
		return ret;
	}

	template <class T, class F>
	T sumbuild(int size, F f)
	{
		if (size == 0) { return convert<T>::go(zero_t<T>{}); }

                if (size == 1) return T { f(0) };

                T ret = add(T { f(0) }, T { f(1) });
		for (int i = 2; i < size; ++i) {
	          auto mark = mark_bump_allocator_if_present();
                  inplace_add(&ret, T { f(i) });
	          reset_bump_allocator_if_present(mark);
                }
		return ret;
	}

	template <class T>
	T delta(int i, int j, T val)
	{
		return (i == j) ? val : convert<T>::go(zero_t<T>{});
	}

	template <class T>
	vec<T> deltaVec(int n, int i, T val)
	{
		return build<T>(n, [i,val](int ii) { 
			return (i == ii) ? val : convert<T>::go(zero_t<T>{}); 
		});
	} 
	
	template <class F>
	auto diag(int rows, int cols, F f)
	{
		ASSERT(rows == cols);
		typedef decltype(f(int{})) T;
		return build<vec<T>>(rows, [cols,f](int i) { 
					return deltaVec(cols, i, f(i)); });
	}


	// -- Specialize is_zero
	template <class T>
	bool is_zero(vec<T> const& v)
	{
		return v.is_zero();
	}

	// -- Specialize type_to_string
	template <typename T>
	struct type_to_string<ks::vec<T>>
	{
		static std::string name()
		{
			return "vec<" + type_to_string<T>::name() + ">";
		}
	};

	// Elementwise addition
	template <class T>
	vec<T> operator+(vec<T> const& a, vec<T> const& b)
	{
		if (is_zero(a))
			return b;

		if (is_zero(b))
			return a;

		ASSERT(a.size() != 0);
		ASSERT(a.size() == b.size());
		vec<T> ret = vec<T>::create(a.size());

		for (int i = 0; i < a.size(); ++i)
			ret[i] = add(a[i], b[i]);
		return ret;
	}

	// Addition of a scalar
	template <class T>
	vec<T> operator+(vec<T> const& a, T const& b)
	{
		ASSERT(false); // Can't handle zero_t + const
		if (is_zero(a))
			return a;
	
		ASSERT(a.size() != 0);
		vec<T> ret{ a.size() };

		for (int i = 0; i < a.size(); ++i)
			ret[i] = add(a[i], b);
		return ret;
	}

	// Elemetwise subtraction
	template <class T>
	vec<T> operator-(vec<T> const& a, vec<T> const& b)
	{
		ASSERT(!is_zero(a));
			// return -b;

		if (is_zero(b))
			return a;

		ASSERT(a.size() != 0);
		ASSERT(a.size() == b.size());
		vec<T> ret = vec<T>::create(a.size());

		for (int i = 0; i < a.size(); ++i)
			ret[i] = a[i] - b[i];
		return ret;
	}

	// Subtraction of a scalar
	template <class T>
	vec<T> operator-(vec<T> const& a, T const& b)
	{
		ASSERT(false); // Can't handle zero_t - const
		if (is_zero(a))
			return a;
	
		ASSERT(a.size() != 0);
		vec<T> ret = vec<T>::create(a.size());

		for (int i = 0; i < a.size(); ++i)
			ret[i] = a[i] - b;
		return ret;
	}

	// Scale a vec
	template <class T>
	vec<T> operator*(vec<T> const& v, double val)
	{
		if (is_zero(v))
			return v;

		ASSERT(v.size() != 0);
		int size = v.size();
		vec<T> ret = vec<T>::create(size); 
		for (int i = 0; i < size; ++i)
			ret[i] = v[i] * val;
		return ret;
	}

	// Scale a vec
	template <class T>
	vec<T> operator*(double val, vec<T> const& v)
	{
		return v * val;
	}

	// sum of elements
	template <class T>
	T sum(vec<T> const& v)
	{
		if (is_zero(v))
			return convert<T>::go(zero_t<T>{});

		if (v.size() == 0) { return convert<T>::go(zero_t<T>{}); }

		if (v.size() == 1) return v[0];
		T ret = add(v[0], v[1]);
		for (int i = 2; i < v.size(); ++i)
			ret = add(ret, v[i]);
		return ret;
	}

	// sum of elements
	template <class T>
	zero_t<T> sum(zero_t<vec<T>> const& v)
	{
		return zero_t<T>{};
	}

	template <class T>
	std::ostream &operator<<(std::ostream &s, ks::vec<T> const &v)
	{
		s << "[";
		if (v.is_zero()) return s << "ZERO]";
		for (int i = 0; i < v.size(); ++i)
			s << (i > 0 ? ", " : "") << v[i];
		return s << "]";
	}
	// ===============================  Linear maps ==================================
	namespace LM
	{
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

			zero_t<To> Apply(From f) const { 
				return { }; 
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
		template <class scale_t, class From, class To>
		To Scale_aux(scale_t val, From const&);

		// General case for Scale_aux<tuple>.
		// Scale first element, then scale tail.
		template <class scale_t, class... Ts>
		tuple<Ts...> Scale_aux(scale_t val, tuple<Ts...> const& t)
		{
			return prepend(Scale_aux(val, head(t)),
		  				   Scale_aux(val, tail(t)));
		}

		// Base case for Scale_aux<Tuple>.  Empty tuple * val = empty tuple.
		template <class scale_t>
		tuple<> Scale_aux(scale_t val, tuple<> const& t)
		{
			return t;
		}

		// Scale anything else
		template <class scale_t, class From, class To>
		To Scale_aux(scale_t val, From const& t)
		{
			return To { val * t };
		}

		struct Scale
		{
			typedef double scale_t;
			scale_t val;

			typedef double To;
			typedef double From;

			static Scale mk(scale_t val) { return Scale{ val }; }

			To Apply(From f) const { return val * f; }
		};

		inline std::ostream &operator<<(std::ostream &s, Scale const &t)
		{
			return s << "Scale" <<
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

		// Helper to gather "From" information from the incoming types
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

		template <class Tuple>
		struct HCat_infer_To {
			// Will fail if Tuple doesn't have at least one element
			typedef typename std::tuple_element<0, Tuple>::type T0;
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

			To Apply(From const& f) const {
				static_assert(n > 1);
				typedef typename std::tuple_element<0, Tup>::type T0;
				To a0 = head(lms).Apply(typename T0::From{ head(f) });
				return Apply_aux<1>(a0, f);
			}

			template <size_t i>
			To Apply_aux(To accum, From const& f) const {
				typedef typename std::tuple_element<i, Tup>::type T0;
				To ai = std::get<i>(lms).Apply(typename T0::From{ std::get<i>(f) });
				if constexpr (i + 1 < n)
					return Apply_aux<i + 1>(add(accum, ai), f);
				else
					return add(accum, ai);
			}

			template <size_t i>
			To Apply_aux(zero_t<To> accum, From const& f) const {
				if constexpr (i < n) {
					// Accumulator could be zero if first terms are zero,
					// just move on to case 1
					auto ai = std::get<i>(lms).Apply(std::get<i>(f));
					return Apply_aux<i + 1>(ai, f);
				}
				else
					return accum;
			}

		};
		
		template <class T, class... Ts>
		std::ostream &operator<<(std::ostream &s, HCat<T, Ts...> const &t)
		{
			s << "HCat";
				//"<" << type_to_string<T1>::name() << "," << type_to_string<T2>::name() << ">" <<
				;
			return tuple_print<0>(s << "(", t.lms) << ")";
		}

		// ---------------- VCat ------------------

		// Helper to gather "From" information from the incoming types
		template <class T>
		struct VCat_infer_To {
		};

		template <>
		struct VCat_infer_To<tuple<>> {
			typedef tuple<> type;
		};

		template <class T, class... Ts>
		struct VCat_infer_To<tuple<T, Ts...>> {
			typedef typename T::To To;
			typedef typename VCat_infer_To<tuple<Ts...>>::type Tos;
			typedef typename tuple_prepend<To, Tos>::type type;
		};

		namespace test_VCat_infer_To {
			struct A {
				typedef int To;
			};
			struct B {
				typedef float To;
			};
			typedef typename VCat_infer_To<tuple<A, B, A>>::type ABA;
			static_assert(std::is_same<ABA, tuple<int, float, int>>::value);
		};

		template <class Tuple>
		struct VCat_infer_From {
			// Will fail if Tuple doesn't have at least one element
			typedef typename std::tuple_element<0, Tuple>::type T0;
			typedef typename T0::From type;
		};

		template <class... LMs>
		struct VCat {
			static constexpr size_t n = sizeof...(LMs);

			typedef tuple<LMs...> Tup;

			Tup lms;

			typedef typename VCat_infer_To<Tup>::type To;
			typedef typename VCat_infer_From<Tup>::type From;

			static VCat mk(LMs... lm) { return VCat{ { lm... } }; }

			To Apply(From const& f) const {
				static_assert(n > 1);
				To ret;
				Apply_aux<0>(f, &ret);
				return ret;
			}

			template <size_t i>
			void Apply_aux(From const& f, To* ret) const
			{
				typedef typename std::tuple_element<i, Tup>::type LM;
				typedef typename LM::To type;
				type ai = std::get<i>(lms).Apply(f);
				std::get<i>(*ret) = type{ ai };
				if constexpr (i + 1 < n) {
					Apply_aux<i + 1>(f, ret);
				}
			}
/*
			template <size_t i>
			To Apply_aux(zero_t<To> accum, From const& f) const {
				if constexpr (i < n) {
					// Accumulator could be zero if first terms are zero,
					// just move on to case 1
					auto ai = std::get<i>(lms).Apply(std::get<i>(f));
					return Apply_aux<i + 1>(ai, f);
				}
				else
					return accum;
			}
*/
		};
		
		template <class T, class... Ts>
		std::ostream &operator<<(std::ostream &s, VCat<T, Ts...> const &t)
		{
			s << "VCat";
				//"<" << type_to_string<T1>::name() << "," << type_to_string<T2>::name() << ">" <<
				;
			return tuple_print<0>(s << "(", t.lms) << ")";
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

			static_assert(std::is_same<B1, B2>::value, "LM::Compose: From2 != To1");

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
		template <typename Functor>
		struct Build
		{
			typedef typename std::invoke_result<Functor, int>::type L;

			typedef typename L::To LTo;

			typedef typename L::From From;
			typedef vec<LTo> To;

			int n;
			Functor/*std::function<L(int)>*/ f;

			template <class Functor2>
			static Build mk(int n, Functor2 f) { 
				ASSERT(n != 0);
				return Build{ n, f }; 
			}

			To Apply(From x) const 
			{ 
				Functor/*std::function<L(int)>*/ f1 = f;
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
				"<" << type_to_string<typename Build<L>::To>::name() << ">" <<
				"(" << t.n << ", <ftype>)";
		}

		// ---------------- BuildT ------------------
		template <typename Functor>
		struct BuildT
		{
			typedef typename std::invoke_result<Functor, int>::type L;

			typedef typename L::From LFrom;

			typedef vec<typename L::From> From;
			typedef typename L::To To;

			int n;
			Functor /*std::function<L(int)>*/ f;

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
			}

			template <class Functor2>
			BuildT(int n, Functor2 f) :
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

			template <class Functor2>
			static BuildT mk(int n, Functor2 f) { return BuildT{ n, f }; }

			To Apply(From x) const
			{
				if (is_zero(x)) return To{ zero_t<To> {} };

				if (n != x.size())
					std::cerr << "BuildT:" << n << " != " << x.size() << std::endl;
				ASSERT(n == x.size());        // TODO: copying arrays here -- should not need to..
				std::function<L(int)> f_local = f;
				return sum(build<LFrom>(n, [f_local,x](int i) { return lmApply(f_local(i), x[i]); }));
			}
		};

		template <class L>
		std::ostream &operator<<(std::ostream &s, BuildT<L> const &t)
		{
			return s << "BuildT" <<
				"<" << type_to_string<typename BuildT<L>::To>::name() << ">" <<
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
				if (v.index() == 0) return std::get<0>(v).Apply(f);
				if (v.index() == 1) return std::get<1>(v).Apply(f);
				throw std::string("Bad Variant");
			}
		};

		template <size_t n, class T, class... Ts>
		struct variant_print {
			static std::ostream& go(std::ostream &s, std::variant<T, Ts...> const & v)
			{
				if (v.index() == n - 1)
					s << (n-1) << ":" << std::get<n - 1>(v);
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
		template <class LM>
		typename LM::To lmApply(LM lm, typename LM::From a) {
			return typename LM::To{ lm.Apply(a) };
		}

	} // namespace LM

	DECLARE_TYPE_TO_STRING(LM::One, T);
	DECLARE_TYPE_TO_STRING2(LM::Zero, From, To);
	DECLARE_TYPE_TO_STRING(LM::Build, L);
	DECLARE_TYPE_TO_STRING(LM::BuildT, L);
	DECLARE_TYPE_TO_STRING_Pack(LM::HCat);
	DECLARE_TYPE_TO_STRING_Pack(LM::VCat);
	DECLARE_TYPE_TO_STRING2(LM::VCat, From, To);
	DECLARE_TYPE_TO_STRING2(LM::Compose, From, To);
	DECLARE_TYPE_TO_STRING2(LM::Variant, From, To);
	DECLARE_TYPE_TO_STRING2(LM::Add, From, To);

	// ===============================  Primitives  ==================================

	inline 
	double sub(double t1, double t2) { return t1 - t2; }
	inline 
	auto D$sub(double, double) {
		typedef LM::Scale M1;
		typedef LM::Scale M2;
		return LM::HCat<M1, M2>::mk(M1::mk(1.0), M2::mk(-1.0));
	}
	inline 
	int sub(int t1, int t2) { return t1 - t2; }

	template <class T>
	T mul(double s, T const& t)
	{
		return s * t;
	}

	template <class T>
	zero_t<T> mul(double s, zero_t<T> const& t)
	{
		return t;
	}

	template <>
	inline tuple<> mul(double s, tuple<> const& t)
	{
		return t;
	}

	template <class U0, class... Us>
	auto mul(double s, tuple<U0, Us...> const& t)
	{
		return prepend(mul(s, head(t)),
					   mul(s, tail(t)));
	}

	inline
	int mul(int const& t1, int const& t2)
	{
		return t1 * t2;
	}

	template <class T1, class T2>
	auto D$mul(T1 t1, T2 t2)
	{
		typedef LM::Scale M1;
		typedef LM::Scale M2;
		return LM::HCat<M1, M2>::mk(M1::mk(t2), M2::mk(t1));
	}

	template <class T1, class T2>
	T1 div(T1 t1, T2 t2)
	{
		return t1 / t2;
	}

	inline
	auto D$div(double t1, double t2)
	{
		return LM::HCat<LM::Scale, LM::Scale>::mk(LM::Scale::mk(1.0 / t2), LM::Scale::mk(-1.0 / (t1*t1)));
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

	inline double abs(double d) { return d > 0 ? d : -d; }
	inline auto D$abs(double d) { return LM::Scale::mk(d > 0 ? 1.0 : -1.0); }

	inline double max(double a, double b) { return a > b ? a : b; }
	inline auto D$max(double a, double b) {
		double s = a > b ? 1.0 : 0.0;
		return LM::HCat<LM::Scale, LM::Scale>::mk(LM::Scale::mk(s), LM::Scale::mk(1.0 - s));
	}

	inline double neg(double d) { return -d; }
	inline auto D$neg(double d) { return LM::Scale::mk(-1.0); }

	inline double to_float(int d) { return d; }
	inline auto D$to_float(int d) { return LM::Zero<int, double>(); }

/*
	template <class I, class Vec>
	using D_t$index = LM::HCat<LM::Zero<I, typename Vec::value_type>,
			 				   LM::Build<std::function<LM::Scale(I)>>>;

	template <class I, class Vec>
	D_t$index<I, Vec>
	D$index(I i, Vec const & v)
	{
		typedef typename Vec::value_type T;
		auto di = LM::Zero<I, T>::mk();
		auto delta = [i](I ii) { return LM::Scale<T,T,double>::mk(T{}, T{}, ii == i ? 1.0 : 0.0); };
		auto df = LM::Build<std::function<LM::Scale<T,T,double>(I)>>::mk(delta);
		return D_t$index<I, Vec>::mk(di, df);
	}

*/

	// ========================= Trace primitive ===============
	inline double $rand(double max)
	{
		static std::mt19937 rng(42);
  		std::uniform_real_distribution<double> dist(0, max);
	
		return dist(rng);
	}

	// ========================= Trace primitive ===============
	template <class T>
	T $trace(T const& a)
	{
		std::cout << "Trace[" << a << "]" << std::endl;
		return a;
	}

	template <class T>
	LM::One<T> D$$trace(T const& a)
	{
		std::cout << "Grad Trace[" << a << "]" << std::endl;
		return LM::One<T>::mk(a);
	}

	// ========================= Printing primitive for KS files ===============
	template <class T>
	int pr(T a)
	{
		std::cout << a << std::endl;
		return 0;
	}

	template <class T, class... Ts>
	int pr(T a, Ts... t)
	{
		pr(a);
		std::cout << "----\n";
		return 1 + pr(t...);
	}

	// =========================== Timing ===============================
	template <class Functor>
	std::function<void(int)> repeat(Functor const& f) {
		return [=](int n) {
			for (int i = 0; i < n; ++i)
				f();
		};
	}


	// BENCHMARK
	// Call with e.g. 
	// benchmark(repeat([&]() { ... my code ... ; }));
	void benchmark(std::function<void(int)> f);
#define BENCHMARK(CODE) ks::benchmark(ks::repeat([&]() { CODE; }))
	/* e.g:
		alloc_mark_t mark = mark_bump_allocator_if_present();
		BENCHMARK(
			reset_bump_allocator_if_present(mark);
			c$68 = gmm_knossos_gmm_objective(c$62, c$63, c$64, c$65, c$66, c$67)
		);
	*/
} // namespace ks
