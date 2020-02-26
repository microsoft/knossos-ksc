// C++ "runtime" for Knossos
#pragma once

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

using std::tuple;

// Enable bump allocator.  See mark/reset below.
#define BUMPY

#define COMMENT(x)

// KS_ASSERT
#define KS_ASSERT(expr)                     \
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

// ======================== Extra math =====================

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
		KS_ASSERT(s == "tuple<int,float>");
	}

	// ===============================  Tuple utils  ==================================

	template < typename T, typename... Ts >
	auto head(tuple<T, Ts...> const& t)
	{
		return  std::get<0>(t);
	}

	template < std::size_t... Ns, typename... Ts >
	auto tail_impl(std::index_sequence<Ns...>, tuple<Ts...> t)
	{
		return  std::make_tuple(std::get<Ns + 1u>(t)...);
	}

	template < typename... Ts >
	auto tail(tuple<Ts...> t)
	{
		return  tail_impl(std::make_index_sequence<sizeof...(Ts) - 1u>(), t);
	}

	template <typename T, typename... Ts >
	auto prepend(T t, tuple<Ts...> tup)
	{
		return tuple_cat(std::make_tuple(t), tup);
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

	template < size_t N, class T >
	auto get(T const& t)
	{
		return std::get<N>(t);
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
			KS_ASSERT(size < 1000000);
			void* ret = buf + top;
			top += ((size + 15) / 16) * 16;
			KS_ASSERT(top < max_size);
			return ret;
		}

		size_t mark() { return top;  }

		void* top_ptr() { return buf + top; }

		void reset(size_t top_ = 0)
		{
			top = top_;
		}
	};

	extern allocator g_alloc;
	typedef size_t alloc_mark_t;
	inline alloc_mark_t mark_bump_allocator_if_present() { return g_alloc.mark(); }
	inline void reset_bump_allocator_if_present(alloc_mark_t top) { g_alloc.reset(top); }
#define $MRK(var) alloc_mark_t var = mark_bump_allocator_if_present()
#define $REL(var) reset_bump_allocator_if_present(var)
#else
	typedef size_t alloc_mark_t;
	inline alloc_mark_t mark_bump_allocator_if_present() { return 0; }
	inline void reset_bump_allocator_if_present(alloc_mark_t top) { }
#define $MRK(var)
#define $REL(var)
#endif


	// ===============================  Zero  ==================================
	// This template to be overloaded when e.g. a vector of T needs to use val to discover a size
	template <class T>
	T zero(T const& val)
	{
		KS_ASSERT(false && "Need to overload zero for this type");
		return T{};
	}

	template <>
	double zero(double const& val)
	{
		return 0.0;
	}

	template <>
	int zero(int const& val)
	{
		return 0;
	}

	tuple<> zero(tuple<> const& val)
	{
		return tuple<> ();
	}

	template <class... Ts>
	tuple<Ts...> zero(tuple<Ts...> const& val)
	{
		return prepend(zero(head(val)), zero(tail(val)));
	}

	// Zero of multiple args is a tuple
	template <class T1, class T2, class... Ts>
	tuple<T1, T2, Ts...> zero(T1 const& t1, T2 const& t2, Ts const&... ts) 
	{
		return zero(std::make_tuple(t1, t2, ts...));
	}

	// ===============================  Inflated deep copy  ==================================

	template <class T>
	T inflated_deep_copy(T z)
	{
		return z;
	}

	template <class T, class... Ts>
	tuple<T, Ts...> inflated_deep_copy(tuple<T, Ts...> val)
	{
		return prepend(inflated_deep_copy(head(val)), inflated_deep_copy(tail(val)));
	}

	// ===============================  Addition ==================================
	// Adding is special because it needs to be defined before linear maps,
	// And needs to be defined before any primitives are used in linear maps.
	// ... At least for gcc, which seems to require more info at template decl time.

	template <class T1, class T2>
	T1 add(T1 t1, T2 t2) { return t1 + t2; }

	template <class T1>
	T1 add(T1 t1, tuple<> t2) { return t1; }

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

	// ===============================  Inplace add ==================================
	template <class T>
	struct inplace_add_t {
		static void go(T *t1, const T &t2) { *t1 += t2; }
	};

	template <class T>
	void inplace_add(T* t1, T const& t2)
	{
		inplace_add_t<T>::go(t1, t2);
	}

	template <>
	struct inplace_add_t<tuple<>> {
		static void go(tuple<> *t1, const tuple<> &t2) { }
	};

	template <size_t i, class T, class... Ts>
	void inplace_add_aux(tuple<T, Ts...> *t1, const tuple<T, Ts...> &t2)
	{
		static constexpr size_t n = sizeof...(Ts);

		inplace_add(&std::get<i>(*t1), std::get<i>(t2));
		if constexpr (i < n)
			inplace_add_aux<i + 1>(t1, t2);
	}

	template <class T, class... Ts>
	struct inplace_add_t<tuple<T, Ts...>> {
		static void go(tuple<T, Ts...> *t1, const tuple<T, Ts...> &t2)
		{
			inplace_add_aux<0>(t1, t2);
		}
	};

	// ===============================  Vector class ==================================
	struct zero_tag_t {};
	const struct zero_tag_t zero_tag;

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
		// come through here.
		bool is_zero_ = false;
		T z_; // ToDo save this cost.

	public:

		typedef T value_type;

#ifdef BUMPY
		vec() :
			size_{ 0 },
			data_{ nullptr },
			is_zero_{ false },
			z_{T{}}
		{
		}

		vec(zero_tag_t, T const& z, size_t size) :
			size_{ size },
			data_{ nullptr },
			is_zero_{ true },
			z_(z)
		{
		}

		vec(size_t  size) {
			alloc(size);
			z_ = T{};
		}

		void alloc(size_t size)
		{
			void *storage = g_alloc.alloc(sizeof(T) * size);
			this->size_ = size;
			this->data_ = (T*)storage;
			this->is_zero_ = false;
		}

                // We can efficiently copy construct (i.e. from a vec
                // of the same type).  We just use operator=.
		vec(vec<T> const& that) 
		{
			this->operator=(that);
		}

                // operator= is fast because it merely involves
                // assigning small things
		vec& operator=(const vec<T>& that)
		{
			this->size_ = that.size_;
			// Yes, this will alias, but there is no
			// deletion, and mutation only happens under
			// controlled circumstances (inplace_add used
			// in mutating sumbuild) where we promise to
			// be careful.
			this->data_ = that.data_;
			this->is_zero_ = that.is_zero_;
			this->z_ = that.z_;
			return *this;
		}

                // We cannot efficiently construct from a vec of a
                // different type.  Instead we have to allocate and
                // copy all the data item by item to give a C++ a
                // chance to auto-convert it.
		template <class U>
		vec(vec<U> const& that) : vec{ that.size() }
		{
			KS_ASSERT(size_ != 0);

			// Copying from another type - allocate.
			if (this->is_zero_)
				alloc(size_);

			for (int i = 0; i < that.size(); ++i)
				this->data_[i] = that[i];
		}

                // We cannot efficiently construct from a std::vector.
                // When constructing from a std::vector we need to
                // allocate and copy because we have no guarantee
                // that the std::vector will not mutate or vanish
                // beneath our feet.
		vec(std::vector<T> const& that) : vec{ that.size() }
		{
			// Copying from std vector - allocate.
			for (size_t i = 0; i < that.size(); ++i)
				data_[i] = that[i];
		}

		int size() const { return int(size_); }

#else
		vec() {}

		vec(size_t  size) : data_(size), is_zero_(false) {}

		vec(std::vector<T> const& that) :data_{ that } {}

		int size() const { return static_cast<int>(data_.size()); }

		vec& operator=(const vec<T>& that)
		{
			KS_ASSERT(that.size() != 0 || that.is_zero_);
			data_ = that.data_;
			is_zero_ = that.is_zero_;
			return *this;
		}

#endif

		T& operator[](int i) { if (is_zero_) return z_; else return data_[i]; }
		T const& operator[](int i) const {  if (is_zero_) return z_; else return data_[i]; }

		static vec<T> create(size_t size);

		bool is_zero() const { return is_zero_; }

		T zero_element() const {
			if (is_zero())
				return z_;
			else {
				KS_ASSERT(size() > 0);
				return zero(data_[0]);
			}
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
#ifndef NDEBUG
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
			ret[i] = T{ f(i) };
		return ret;
	}

	template <class T, class F, class A>
        A fold(F f, A z, vec<T> v)
	{
          A acc = z;

          for (int i = 0; i < v.size(); i++) {
            acc = f(tuple(acc, v[i]));
          }

          return acc;
	}

        template <class T, class F, class F_, class A, class S, class dA, class dT>
          tuple<S, tuple<dA, vec<dT>>> RFold(const dT &dummy, S s_zero, F f, F_ f_, A acc, vec<T> v, dA dr) {
	  auto forward_pass = std::vector<A>(v.size());

	  for (int i = 0; i < v.size(); i++) {
	    forward_pass[i] = acc;
	    acc = f(tuple(acc, v[i]));
	  }

	  S dScope = s_zero;
	  auto dv = vec<dT>(v.size());

	  for (int i = v.size() - 1; i >= 0; i--) {
            tuple<S, tuple<dA, dT>> f_call = f_(tuple(tuple(forward_pass[i], v[i]), dr));

	    S  f_call_dScope = std::get<0>(f_call);
	    dA f_call_dacc   = std::get<0>(std::get<1>(f_call));
	    dT f_call_dT     = std::get<1>(std::get<1>(f_call));

	    dr = f_call_dacc;
	    dScope = add(dScope, f_call_dScope);
	    dv[i] = f_call_dT;
	  }

          return tuple(dScope, tuple(dr, dv));
        }

        // Probably should implement this as a loop
        template <class T, class F, class F_, class A, class dA, class dT>
        dA FFold_recursive(int i, F f, A acc, vec<T> v, F_ f_, dA dacc, vec<dT> dv) {
          if (i == v.size()) {
            return dacc;
          } else {
            dA fwd_f = f_(tuple(tuple(acc, v[i]), tuple(dacc, dv[i])));
            return FFold_recursive(i + 1, f, f(tuple(acc, v[i])), v, f_, fwd_f, dv);
          }
        }

        template <class T, class F, class F_, class A, class dA, class dT>
        dA FFold(F f, A acc, vec<T> v, F_ f_, dA dacc, vec<dT> dv) {
          return FFold_recursive(0, f, acc, v, f_, dacc, dv);
        }

	// specialize zero(vec<T>)
	template <class T>
	vec<T> zero(vec<T> const& val)
	{
		return vec<T> {zero_tag, val.zero_element(), (size_t)val.size()};
	}

	template <class T>
	vec<T> inflated_deep_copy(vec<T> t)
	{
		vec<T> ret = vec<T>::create(t.size());

		for (int i = 0; i < t.size(); ++i)
			ret[i] = inflated_deep_copy(t[i]);
		return ret;
	}

	// specialize inplace_add(vec<T>*,vec<T>)
	template <class T>
	struct inplace_add_t<vec<T>> {
		static void go(vec<T> *t1, const vec<T> &t2)
		{
			int n = t2.size();
			KS_ASSERT(t1->size() == n);

			if (t2.is_zero()) return;

			if (t1->is_zero()) {
#ifdef BUMPY
				std::cerr << "aliasing: " << &t2[0] << ", mark at " << (char*)g_alloc.top_ptr() - (char*)&t2[0] << std::endl;
#endif
				*t1 = t2; // TODO: memory aliasing here?   When we free, this will get lost.  We need another heap....
				return;
			}

			for (int i = 0; i < n; ++i)
				ks::inplace_add_t<T>::go(&(*t1)[i], t2[i]);
		}
	};

	template <class T, class F>
	T sumbuild(int size, F f)
	{
		if (size == 0)
		{
			std::cerr << "hope this is ok";
			return zero(T{});
		}

		if (size == 1)
			return T{f(0)};

		T f0 = f(0);
		T ret = inflated_deep_copy(f0);
		for (int i = 1; i < size; ++i)
		{
			auto mark = mark_bump_allocator_if_present();
			T fi = f(i);
			inplace_add_t<T>::go(&ret, fi);
			reset_bump_allocator_if_present(mark);
		}
		return ret;
	}

	template <class T>
	T delta(int i, int j, T val)
	{
		return (i == j) ? val : zero(val);
	}

	template <class T>
	vec<T> deltaVec(int n, int i, T val)
	{
		vec<T> ret(n);
		T z = zero(val);
		for(int j = 0; j < n; ++j)
			if (j != i)
			  ret[j] = z;
			else
			  ret[j] = val;
		return ret;
	}

	vec<double> constVec(int n, double val)
	{
          if (val == 0.0) {
            return vec(zero_tag, 0.0, n);
          } else {
		vec<double> ret(n);
		for(int j = 0; j < n; ++j)
			ret[j] = val;
		return ret;
          }
	}

	template <class T>
	vec<T> constVec(int n, T val)
	{
		vec<T> ret(n);
		for(int j = 0; j < n; ++j)
			ret[j] = val;
		return ret;
	}

	template <class T, class dT>
        vec<dT> fwd$constVec(std::tuple<int, T> n_val, std::tuple<std::tuple<>, dT> unit_dval)
	{
                auto n = std::get<0>(n_val);
                auto dval = std::get<1>(unit_dval);
		return constVec(n, dval);
	}

	template <class F>
	auto diag(int rows, int cols, F f)
	{
		KS_ASSERT(rows == cols);
		typedef decltype(f(int{})) T;
		return build<vec<T>>(rows, [cols,f](int i) { 
					return deltaVec(cols, i, f(i)); 
		});
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
		if (a.is_zero())
			return b;

		if (b.is_zero())
			return a;

		KS_ASSERT(a.size() == b.size());
		vec<T> ret = vec<T>::create(a.size());

		for (int i = 0; i < a.size(); ++i)
			ret[i] = add(a[i], b[i]);
		return ret;
	}

	// Addition of a scalar
	template <class T>
	vec<T> operator+(vec<T> const& a, T const& b)
	{
		if (a.is_zero())
			return inflated_deep_copy(a) + b;

		KS_ASSERT(a.size() != 0);
		vec<T> ret{ a.size() };

		for (int i = 0; i < a.size(); ++i)
			ret[i] = add(a[i], b);
		return ret;
	}

	// Subtraction of a scalar
	template <class T>
	vec<T> operator-(vec<T> const& a, T const& b)
	{
		KS_ASSERT(false); // Can't handle zero_t - const?
		if (a.is_zero())
			return inflated_deep_copy(a) + b;

		KS_ASSERT(a.size() != 0);
		vec<T> ret = vec<T>::create(a.size());

		for (int i = 0; i < a.size(); ++i)
			ret[i] = a[i] - b;
		return ret;
	}

	template <class T>
	T mul(double s, T const& t);

	// Scale a vec
	template <class T>
	vec<T> mul(double val, vec<T> const& v)
	{
		if (v.is_zero())
			return v;

		KS_ASSERT(v.size() != 0);
		int size = v.size();
		vec<T> ret = vec<T>::create(size);
		for (int i = 0; i < size; ++i)
			ret[i] = mul(val, v[i]);
		return ret;
	}

	// Scale a vec
	template <class T>
	vec<T> operator*(double val, vec<T> const& v)
	{
		return mul(val, v);
	}

	// Scale a vec
	template <class T>
	vec<T> operator*(vec<T> const& v, double val)
	{
		return mul(val, v);
	}

	// sum of elements
	template <class T>
	T sum(vec<T> const& v)
	{
		if (v.is_zero()) {
			std::cerr <<"okkk?";
			return zero(T{});
		}

		if (v.size() == 0) { return zero(T{}); }

		if (v.size() == 1) return v[0];
		T ret = add(v[0], v[1]);
		for (int i = 2; i < v.size(); ++i)
			ret = add(ret, v[i]);
		return ret;
	}

	template <class T>
	std::ostream &operator<<(std::ostream &s, ks::vec<T> const &v)
	{
		s << "[";
		if (v.is_zero()) return s << "ZERO(" << v.size() << ")]";
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

			To Apply(From f) const {
				return zero(To{});
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
			return To{ val * t };
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
				KS_ASSERT(n != 0);
				return Build{ n, f };
			}

			To Apply(From x) const
			{
				Functor/*std::function<L(int)>*/ f1 = f;
				return build<LTo>(n, [x, f1](int i) {
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
				if (n != x.size())
					std::cerr << "BuildT:" << n << " != " << x.size() << std::endl;
				ASSERT(n == x.size());        // TODO: copying arrays here -- should not need to..
				std::function<L(int)> f_local = f;  // TODO: use sumbuild
				return sumbuild<LFrom>(n, [f_local,x](int i) { return lmApply(f_local(i), x[i]); });
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
					s << (n - 1) << ":" << std::get<n - 1>(v);
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

	inline double sub(double t1, double t2) { return t1 - t2; }
	inline auto D$sub(double, double) 
	{
		typedef LM::Scale M1;
		typedef LM::Scale M2;
		return LM::HCat<M1, M2>::mk(M1::mk(1.0), M2::mk(-1.0));
	}
	
	inline int sub(int t1, int t2) { return t1 - t2; }

	template <class T>
	T mul(double s, T const& t)
	{
		return s * t;
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
		return LM::HCat<LM::Scale, LM::Scale>::mk(LM::Scale::mk(t2), LM::Scale::mk(t1));
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

	template <class T>
	inline bool eq(T t1, T t2)
	{
		return t1 == t2;
	}

	template <class T>
          inline bool ne(T t1, T t2)
	{
		return t1 != t2;
	}

        inline bool lt$aff(double t1, double t2)
	{
		return t1 < t2;
	}

        inline bool lt$aii(int t1, int t2)
	{
		return t1 < t2;
	}

	inline bool gt$aff(double t1, double t2)
	{
		return t1 > t2;
	}

	inline bool gt$aii(int t1, int t2)
	{
		return t1 > t2;
	}

	inline bool lte$aff(double t1, double t2)
	{
		return t1 <= t2;
	}

	inline bool lte$aii(int t1, int t2)
	{
		return t1 <= t2;
	}

	inline bool gte$aff(double t1, double t2)
	{
		return t1 >= t2;
	}

	inline bool gte$aii(int t1, int t2)
	{
		return t1 >= t2;
	}

	inline double add$aff(double t1, double t2)
	{
		return t1 + t2;
	}

	inline int add$aii(int t1, int t2)
	{
		return t1 + t2;
	}

	inline double sub$aff(double t1, double t2)
	{
		return t1 - t2;
	}

	inline int sub$aii(int t1, int t2)
	{
		return t1 - t2;
	}

	inline double div$aff(double t1, double t2)
	{
		return t1 / t2;
	}

	inline int div$aii(int t1, int t2)
	{
		return t1 / t2;
	}

	inline double mul$aff(double t1, double t2)
	{
		return t1 * t2;
	}

	inline int mul$aii(int t1, int t2)
	{
		return t1 * t2;
	}

	inline double neg$af(double t)
	{
		return -t;
	}

	inline int neg$ai(int t)
	{
		return -t;
	}

	inline bool or_(int b1, int b2)
	{
	  return b1 || b2;
	}

	inline bool and_(int b1, int b2)
	{
	  return b1 && b2;
	}

	inline double abs(double d) { return d > 0 ? d : -d; }
	inline auto D$abs(double d) { return LM::Scale::mk(d > 0 ? 1.0 : -1.0); }

	inline double max(double a, double b) { return a > b ? a : b; }
	inline auto D$max(double a, double b) {
		double s = a > b ? 1.0 : 0.0;
		return LM::HCat<LM::Scale, LM::Scale>::mk(LM::Scale::mk(s), LM::Scale::mk(1.0 - s));
	}

	inline int neg(int d) { return -d; }

	inline double neg(double d) { return -d; }
	inline auto D$neg(double d) { return LM::Scale::mk(-1.0); }

        inline tuple<> neg(tuple<> d) { return d; }

        template <class U0, class... Us>
        inline tuple<U0, Us...> neg(tuple<U0, Us...> t) { return prepend(neg(head(t)), neg(tail(t))); }

        template <class T>
        inline vec<T> neg(vec<T> v) { return build<T>(v.size(), [v](int i){ return neg(v[i]); }); }

	inline double to_float(int d) { return d; }
	inline auto D$to_float(int d) { return LM::Zero<int, double>(); }

	inline int to_size(int d) { return d; }
	inline auto D$to_size(int d) { return LM::Zero<int, int>(); }

	inline int to_integer(int d) { return d; }
	inline auto D$to_integer(int d) { return LM::Zero<int, int>(); }

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

	// ========================= Random primitives ============
        // ranhash functions from
        //
        //     https://mathoverflow.net/questions/104915/pseudo-random-algorithm-allowing-o1-computation-of-nth-element
        inline uint64_t $ranhash(uint64_t v) {
          v *= 3935559000370003845LL;
          v += 2691343689449507681LL;
          v ^= v >> 21; v ^= v << 37; v ^= v >> 4;
          v *= 4768777513237032717LL;
          v ^= v << 20; v ^= v >> 41; v ^= v << 5;
          return v;
        }

        inline double $ranhashdoub(int32_t v) {
          return 5.42101086242752217E-20 * $ranhash(v);
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

	template <class T>
	int print(T a)
	{
		std::cout << a;
		return 0;
	}

	template <class T, class... Ts>
	int print(T a, Ts... t)
	{
		print(a);
		return 1 + print(t...);
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
	double benchmark(std::function<void(int)> f);
#define BENCHMARK(CODE) ks::benchmark(ks::repeat([&]() { CODE; }))
	/* e.g:
		alloc_mark_t mark = mark_bump_allocator_if_present();
		BENCHMARK(
			reset_bump_allocator_if_present(mark);
			c$68 = gmm_knossos_gmm_objective(c$62, c$63, c$64, c$65, c$66, c$67)
		);
	*/

#define $BENCH(FUN) ks::benchmark(ks::repeat([&]() { \
																								$MRK(t); \
																								FUN(tuple<>{}); \
																								$REL(t); \
																						}))

	// ===============================  Dot ===========================================
  inline double dot(double t1, double t2) { return t1 * t2; }

	template <class T>
	inline double dot(T t1, tuple<> t2)
	{
		return 0.0;
	}

	template <class T>
	inline double dot(T t1, tuple<T> t2)
	{
		return dot(t1,std::get<0>(t2));
	}

	template <class T0, class... Ts, class U0, class... Us>
	inline double dot(tuple<T0, Ts...> t1, tuple<U0, Us...> t2)
	{
		return dot(head(t1), head(t2)) + dot(tail(t1), tail(t2));
	}

	template <class T1, class T2>
	inline double dot(vec<T1> t1, vec<T2> t2)
	{
		double ret = 0;

		KS_ASSERT(t1.size() == t2.size());

		for (int i = 0; i < t1.size(); i++)
		{
			ret += dot(t1[i], t2[i]);
		}

		return ret;
	}

	// ===============================  Derivative check  ================================
  //  Check derivatives:
  // 
	//    delta_f = f(x+dx) - f(x) ~= D$f * dx
	//
	//  And
	//    rev_f(x, df) = df' * D$f
	//  So
	//    dot(df, delta_f) =~ df*d$f*dx 
	//                     =~ dot(rev_f, dx)
  // 
  //  i.e. what should be small (when dx is) if our
  //  reverse mode generated code is correct.
	template <class Functor, class RevFunctor, class X, class X_, class Dx, class Df>
        double $check(Functor f, RevFunctor rev_f, X x, X_ x_, Dx dx, Df df)
	{
		auto f_x = std::apply(f, x);
		auto f_x_plus_dx = std::apply(f, add(x, dx));
		auto delta_f = f_x_plus_dx - f_x;
		double d1 = dot(delta_f, df);
		auto dfJ = std::apply(rev_f, std::make_tuple(x_, df));
		double d2 = dot(dfJ, dx);

		/*
		std::cout << "dfJ=" << dfJ << std::endl;
		std::cout << "DOT=" << dot(dfJ, dx) << std::endl;
		std::cout << " D1=" << d1 << std::endl;
		std::cout << " D2=" << d2 << std::endl;
		*/

		return abs(d1 - d2)/(abs(d1) + abs(d2));
	}
} // namespace ks

#include "knossos-prelude.h"
