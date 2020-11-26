// C++ "runtime" for Knossos
#pragma once

#include <type_traits>
#include <utility>
#include <variant>
#include <tuple>
#include <set>
#include <functional>
#include <sstream>
#include <iostream>
#include <random>
#include <cmath>
#include <cstring>
#include <string>
#include <chrono>

using std::tuple;

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

	// This needs to be declared before tuple_print in order to support printing of nested tuples
	// (gcc will accept the code even without this declaration, but clang will not).
	template <class... Ts>
	std::ostream& operator<<(std::ostream& s, tuple<Ts...> const& t);

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
	class allocator_base {
		size_t max_size_;
		unsigned char* buf_;
		size_t top_;
		size_t peak_;
	
	public:
		allocator_base(unsigned char * buf, size_t max_size, size_t peak = 0) :
			max_size_(max_size),
			buf_(buf),
			top_(0),
			peak_(peak)
		{}

		void* allocate(size_t size)
		{
			KS_ASSERT(size < 1000000);
			void* ret = buf_ + top_;
			top_ += padded_size(size);
			if (top_ > peak_) {
				peak_ = top_;
				KS_ASSERT(top_ < max_size_);
			}
			return ret;
		}

		static size_t padded_size(size_t size) { return ((size + 15) / 16) * 16; }

		size_t mark() const { return top_;  }

		void* top_ptr() const { return buf_ + top_; }

		void* ptr_at(size_t m) const { return buf_ + m; }

		void reset(size_t top = 0)
		{
			top_ = top;
		}

		size_t peak() const { return peak_; }
	};

	typedef size_t alloc_mark_t;

	class allocator : public allocator_base
	{
	public:
		allocator(size_t max_size) :
			allocator_base(new unsigned char[max_size], max_size)
		{}
		~allocator() {
			delete[] static_cast<unsigned char*>(ptr_at(0));
		}
	};

	class allocator_ref : public allocator_base
	{
	public:
		allocator_ref(unsigned char * buf, size_t size) :
			allocator_base(buf, size, size) // We don't need to track peak memory usage for allocator_ref, so can set peak=size to minimize overhead.
		{ }

		static allocator_ref create_from_allocation(allocator_base * alloc, size_t numBytes) {
			return allocator_ref((unsigned char*)alloc->allocate(numBytes), numBytes);
		}
	};

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
	T inflated_deep_copy(allocator_base *, T z)
	{
		return z;
	}

	template <class T, class... Ts>
	tuple<T, Ts...> inflated_deep_copy(allocator_base * alloc, tuple<T, Ts...> val)
	{
		return prepend(inflated_deep_copy(alloc, head(val)), inflated_deep_copy(alloc, tail(val)));
	}

	// ===============================  Addition ==================================
	// Adding is special because it needs to be defined before linear maps,
	// And needs to be defined before any primitives are used in linear maps.
	// ... At least for gcc, which seems to require more info at template decl time.

	template <class T1, class T2>
	T1 ts_add(allocator *, T1 t1, T2 t2) { return t1 + t2; }

	template <class T1>
	T1 ts_add(allocator *, T1 t1, tuple<> t2) { return t1; }

	template <>
	inline tuple<> ts_add(allocator *, tuple<> t1, tuple<> t2)
	{
		return tuple<>{};
	}

	template <class T0, class... Ts, class U0, class... Us>
	auto ts_add(allocator * alloc, tuple<T0, Ts...> t1, tuple<U0, Us...> t2)
	{
		return prepend(ts_add(alloc, head(t1), head(t2)),
			ts_add(alloc, tail(t1), tail(t2)));
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
		//TODO Full set of 5 ctors/assigners
		size_t size_;
		T* data_;
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

		vec(allocator_base * alloc, size_t  size) {
			allocate(alloc, size);
			z_ = T{};
		}

		void allocate(allocator_base * alloc, size_t size)
		{
			void *storage = alloc->allocate(bytes_required(size));
			this->size_ = size;
			this->data_ = (T*)storage;
			this->is_zero_ = false;
		}

		static size_t bytes_required(size_t size) {
			return sizeof(T) * size;
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
		vec(allocator_base * alloc, vec<U> const& that) : vec{ alloc, that.size() }
		{
			KS_ASSERT(size_ != 0);
			for (int i = 0; i < that.size(); ++i)
				this->data_[i] = that[i];
		}

                // We cannot efficiently construct from a std::vector.
                // When constructing from a std::vector we need to
                // allocate and copy because we have no guarantee
                // that the std::vector will not mutate or vanish
                // beneath our feet.
		vec(allocator_base * alloc, std::vector<T> const& that) : vec{ alloc, that.size() }
		{
			// Copying from std vector - allocate.
			for (size_t i = 0; i < that.size(); ++i)
				data_[i] = that[i];
		}

		int size() const { return int(size_); }

		T* data() { return data_; }
		const T* data() const { return data_; }

		T& operator[](int i) { if (is_zero_) return z_; else return data_[i]; }
		T const& operator[](int i) const {  if (is_zero_) return z_; else return data_[i]; }

		static vec<T> create(allocator_base * alloc, size_t size)
		{
			return vec<T>(alloc, size);
		}

		bool is_zero() const { return is_zero_; }

		T zero_element() const {
			if (is_zero())
				return z_;
			else {
				KS_ASSERT(size() > 0);
				return zero(data_[0]);
			}
		}

		bool operator == (vec const& other) const {
			if (size() != other.size()) {
				return false;
			}
			for (int i = 0; i != size(); ++i) {
				if ((*this)[i] != other[i]) {
					return false;
				}
			}
			return true;
		}

		bool operator != (vec const& other) const { return !(*this == other); }
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

	template <class T, class F>
	vec<T> build(allocator * alloc, int size, F f)
	{
		vec<T> ret = vec<T>::create(alloc, size);

		for (int i = 0; i < size; ++i)
			ret[i] = T{ f(alloc, i) };
		return ret;
	}

	template <class T, class F, class A>
        A fold(allocator * alloc, F f, A z, vec<T> v)
	{
          A acc = z;

          for (int i = 0; i < v.size(); i++) {
            acc = f(alloc, acc, v[i]);
          }

          return acc;
	}

        template <class T, class F, class F_, class A, class S, class dA, class dT>
          tuple<S, tuple<dA, vec<dT>>> RFold(allocator * alloc, const dT &dummy, S s_zero, F f, F_ f_, A acc, vec<T> v, dA dr) {
	  auto forward_pass = std::vector<A>(v.size());

	  for (int i = 0; i < v.size(); i++) {
	    forward_pass[i] = acc;
	    acc = f(alloc, acc, v[i]);
	  }

	  S dScope = s_zero;
	  auto dv = vec<dT>(alloc, v.size());

	  for (int i = v.size() - 1; i >= 0; i--) {
            tuple<S, tuple<dA, dT>> f_call = f_(alloc, tuple(forward_pass[i], v[i]), dr);

	    S  f_call_dScope = std::get<0>(f_call);
	    dA f_call_dacc   = std::get<0>(std::get<1>(f_call));
	    dT f_call_dT     = std::get<1>(std::get<1>(f_call));

	    dr = f_call_dacc;
	    dScope = ts_add(alloc, dScope, f_call_dScope);
	    dv[i] = f_call_dT;
	  }

          return tuple(dScope, tuple(dr, dv));
        }

        // Probably should implement this as a loop
        template <class T, class F, class F_, class A, class dA, class dT>
        dA FFold_recursive(allocator * alloc, int i, F f, A acc, vec<T> v, F_ f_, dA dacc, vec<dT> dv) {
          if (i == v.size()) {
            return dacc;
          } else {
            dA fwd_f = f_(alloc, tuple(acc, v[i]), tuple(dacc, dv[i]));
            return FFold_recursive(alloc, i + 1, f, f(alloc, acc, v[i]), v, f_, fwd_f, dv);
          }
        }

        template <class T, class F, class F_, class A, class dA, class dT>
        dA FFold(allocator * alloc, F f, A acc, vec<T> v, F_ f_, dA dacc, vec<dT> dv) {
          return FFold_recursive(alloc, 0, f, acc, v, f_, dacc, dv);
        }

	// specialize zero(vec<T>)
	template <class T>
	vec<T> zero(vec<T> const& val)
	{
		return vec<T> {zero_tag, val.zero_element(), (size_t)val.size()};
	}

	template <class T>
	vec<T> inflated_deep_copy(allocator_base * alloc, vec<T> t)
	{
		vec<T> ret = vec<T>::create(alloc, t.size());

		for (int i = 0; i < t.size(); ++i)
			ret[i] = inflated_deep_copy(alloc, t[i]);
		return ret;
	}

	// The number of bytes that would be required from the
	// allocator to store an inflated copy of the given object
	template<class T>
	size_t inflated_bytes(T const&) { return 0; }

	template<class TupleT, size_t... Indices>
	size_t inflated_bytes_tupleimpl(TupleT const& t, std::index_sequence<Indices...>) {
		return ((size_t)0 + ... + inflated_bytes(std::get<Indices>(t)));
	}
	template<class... Types>
	size_t inflated_bytes(tuple<Types...> const& t) {
		return inflated_bytes_tupleimpl(t, std::index_sequence_for<Types...>{});
	}

	template<class T>
	size_t inflated_bytes(vec<T> const& v) {
		int sz = v.size();
		size_t ret = allocator::padded_size(vec<T>::bytes_required(sz));
		for (int i = 0; i != sz; ++i) {
			ret += inflated_bytes(v[i]);
		}
		return ret;
	}

	// ===============================  Copydown  ==================================

	// Tests if any of the memory referred to by val overlaps the
	// range [start, end)
	template<class T>
	bool memory_overlaps(const void* /*start*/, const void* /*end*/, T const& /*val*/) {
		return false;
	}

	template<class TupleT, size_t... Indices>
	bool memory_overlaps_tupleimpl(const void* start, const void* end, TupleT const& t, std::index_sequence<Indices...>) {
		return (memory_overlaps(start, end, std::get<Indices>(t)) || ...);
	}
	template<class... Types>
	bool memory_overlaps(const void* start, const void* end, tuple<Types...> const& t) {
		return memory_overlaps_tupleimpl(start, end, t, std::index_sequence_for<Types...>{});
	}

	template<class T>
	bool memory_overlaps(const void* start, const void* end, vec<T> const& v) {
		if (v.is_zero()) {
			return v.size() != 0 && memory_overlaps(start, end, v[0]);
		}

		for (int i = 0; i != v.size(); ++i) {
			if (memory_overlaps(start, end, v[i])) {
				return true;
			}
		}
		return v.data() < end && v.data() + v.size() > start;
	}

	struct prepare_copydown_state
	{
		unsigned char * subobjectDestination;   // destination of the next vec subobject
		                                        // (updated whenever we encounter a vec during iteration over subobjects)
		unsigned char * const startOfDestination;   // destination of first vec in the iteration sequence
		allocator * alloc;
	};

	template<class T>
	void prepare_copydown_inplace(prepare_copydown_state *, T *) {
		/* There's nothing to do unless T has a vec subobject. */
	}

	template<class TupleT, size_t... Indices>
	void prepare_copydown_inplace_tupleimpl(prepare_copydown_state * dest, TupleT * t, std::index_sequence<Indices...>) {
		((prepare_copydown_inplace(dest, &std::get<Indices>(*t))), ...);
	}

	template<class... Types>
	void prepare_copydown_inplace(prepare_copydown_state * dest, tuple<Types...> * t) {
		prepare_copydown_inplace_tupleimpl(dest, t, std::index_sequence_for<Types...>{});
	}

	template<class T>
	void prepare_copydown_inplace(prepare_copydown_state * dest, vec<T> * v) {
		/* Note that this function modifies *v in-place. That's OK
		   provided that *v lives either
		   - on the stack; or
		   - in the allocator's buffer *after* dest->startOfDestination,
		   because in the latter case, all of this memory will be overwritten
		   by the copydown anyway, or freed when the allocator is reset.
		   We'll make sure that this function is never called with an argument
		   that lives in the allocator's buffer before dest->startOfDestination.
		   */
		if (v->is_zero()) {
			int sz = v->size();
			if (sz == 0) {
				return;   // no data to be copied
			}
			T zero_value = (*v)[0];
			/* When we do the copydown, we'll be making sz copies
			   of zero_value. We need to make sure that none of those
			   copies will overwrite data referred to by zero_value.
			   The strictest condition applies when the final copy
			   is made, so we can start by assuming that the first
			   (sz - 1) copies will be fine. */
			dest->subobjectDestination += allocator::padded_size(vec<T>::bytes_required(sz))
					+ (sz - 1) * inflated_bytes(zero_value);
			prepare_copydown_inplace(dest, &zero_value);
			*v = vec<T>(zero_tag, zero_value, sz);
			return;
		}
		
		void * sourceData = v->data();
		if (sourceData < dest->startOfDestination) {
			/* This data lives before the copydown location in the buffer.
			   We assume that this means it can't contain any pointers to
			   data after the copydown location, so we don't need to move
			   any of the data belonging to subobjects of *v.
			   That's fortunate, because we wouldn't be allowed to modify
			   objects before the copydown location even if we wanted to. */
			KS_ASSERT(!memory_overlaps(dest->startOfDestination, dest->subobjectDestination, *v));
			dest->subobjectDestination += inflated_bytes(*v);
		} else {
			int sz = v->size();
			if (sourceData < dest->subobjectDestination) {
				/* This source overlaps the desination of another subobject that comes
				   earlier in the iteration order. We need to move it out of the way. */
				if (dest->alloc->top_ptr() < dest->subobjectDestination) {
					/* Make sure we're not about to copy to a place which is still
					   in the way! */
					dest->alloc->allocate(dest->subobjectDestination - (unsigned char*)dest->alloc->top_ptr());
				}
				*v = vec<T>(dest->alloc, sz);
				std::memcpy(v->data(), sourceData, sz * (int)sizeof(T));
			}
			dest->subobjectDestination += allocator::padded_size(vec<T>::bytes_required(sz));
			for (int i = 0; i != sz; ++i) {
				prepare_copydown_inplace(dest, &((*v)[i]));
			}
		}
	}

	/* Copy some of the data referred to by val if necessary,
	   returning a modified version of val which meets the
	   precondition for copydown_by_memmove below.

	   This function works by calculating where the eventual
	   destination will be for the data of each vec subobject.
	   This involves replicating the sequence of allocations
	   that will take place, but without actually calling
	   an allocator.

	   Assumes that "mark" is not in the middle of an allocation
	   (see comment for copydown_by_memmove). */
	template<class T>
	T prepare_copydown(allocator * alloc, alloc_mark_t mark, T val) {
		unsigned char * start = static_cast<unsigned char*>(alloc->ptr_at(mark));
		prepare_copydown_state dest{ start, start, alloc };
		prepare_copydown_inplace(&dest, &val);
		return val;
	}

	template<class T>
	void copydown_by_memmove_inplace(allocator *, T *) { }

	template<class TupleType, size_t... Indices>
	void copydown_by_memmove_inplace_tuple(allocator * alloc, TupleType * t, std::index_sequence<Indices...>) {
		((copydown_by_memmove_inplace(alloc, &std::get<Indices>(*t))), ...);
	}
	template<class... Types>
	void copydown_by_memmove_inplace(allocator * alloc, tuple<Types...> * t) {
		copydown_by_memmove_inplace_tuple(alloc, t, std::index_sequence_for<Types...>{});
	}

	template<class T>
	void copydown_by_memmove_inplace(allocator * alloc, vec<T> * v) {
		int sz = v->size();
		if (v->is_zero()) {
			if (sz != 0) {
				T elem = (*v)[0];
				*v = vec<T>(alloc, sz);
				for (int i = 0; i != sz; ++i) {
					(*v)[i] = elem;
				}
			}
		} else {
			T* oldData = v->data();
			*v = vec<T>(alloc, sz);
			std::memmove(v->data(), oldData, static_cast<size_t>(sz) * sizeof(T));
		}
		for (int i = 0; i != sz; ++i) {
			copydown_by_memmove_inplace(alloc, &((*v)[i]));
		}
	}

	/* Perform a copydown by iterating over the subobjects of val;
	   for each subobject which is a vec, copy its data to the
	   desired position using memmove.

	   Precondition: for each vec<T> subobject v, there must be no
	   overlap between the intervals
	     [v.data(), v.data() + v.size()*sizeof(T)) and
	     [alloc->ptr_at(mark), newvdata)
	   where newvdata is the intended new value of v.data() after
	   copydown.
	   (If this condition was not satisfied, then v's data would
	   be overwritten before we got the chance to move it, because
	   the interval [alloc->ptr_at(mark), newvdata) contains
	   the destinations of subobjects which come before v in the
	   iteration order.)

	   If we assume that "mark" is always at the boundary of an
	   allocation, not in the middle of one, then the precondition
	   reduces to ensuring that v.data() is not in the interval
	   [alloc->ptr_at(mark), newvdata).
	   */
	template<class T>
	T copydown_by_memmove(allocator * alloc, alloc_mark_t mark, T val)
	{
		alloc->reset(mark);
		copydown_by_memmove_inplace(alloc, &val);
		return val;
	}

	/* Make a deep copy of the given object such that its allocations
	   start at the marked position, then reset the allocator to
	   the endpoint of the allocations for this object. That is, we
	   reclaim (and may overwrite) all of the memory of existing objects
	   which come after the marked position in the buffer.

	   Note that the original object val may itself refer to memory
	   which overlaps the copydown destination. */
	template<class T>
	T copydown(allocator * alloc, alloc_mark_t mark, T const& val)
	{
#ifdef CHECK_COPYDOWN_CORRECTNESS   // performs a (slow!) check that the result of a copydown is equal to the original
		alloc_mark_t originalTop = alloc->mark();
		alloc->allocate(inflated_bytes(val));  // ensure that safe_copy does not overlap any temporary allocations that might be made during copydown
		T safe_copy = inflated_deep_copy(alloc, val);
		alloc->reset(originalTop);
#endif
		T modified_val = prepare_copydown(alloc, mark, val);
		T ret = copydown_by_memmove(alloc, mark, modified_val);
#ifdef CHECK_COPYDOWN_CORRECTNESS
		if (ret != safe_copy) {
			std::cerr << "Detected an incorrect copydown" << std::endl;
			abort();
		}
#endif
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
				*t1 = t2; // TODO: memory aliasing here?   When we free, this will get lost.  We need another heap....
				return;
			}

			for (int i = 0; i < n; ++i)
				ks::inplace_add_t<T>::go(&(*t1)[i], t2[i]);
		}
	};

	template <class T, class F>
	T sumbuild(allocator * alloc, int size, F f)
	{
		if (size == 0)
		{
			std::cerr << "hope this is ok";
			return zero(T{});
		}

		if (size == 1)
			return T{f(alloc, 0)};

		T f0 = f(alloc, 0);
		T ret = inflated_deep_copy(alloc, f0);
		for (int i = 1; i < size; ++i)
		{
			auto mark = alloc->mark();
			T fi = f(alloc, i);
			inplace_add_t<T>::go(&ret, fi);
			alloc->reset(mark);
		}
		return ret;
	}

	template <class T>
	T delta(int i, int j, T val)
	{
		return (i == j) ? val : zero(val);
	}

	template <class T>
	vec<T> deltaVec(allocator * alloc, int n, int i, T val)
	{
		vec<T> ret(alloc, n);
		T z = zero(val);
		for(int j = 0; j < n; ++j)
			if (j != i)
			  ret[j] = z;
			else
			  ret[j] = val;
		return ret;
	}

	vec<double> constVec(allocator * alloc, int n, double val)
	{
          if (val == 0.0) {
            return vec<double>(zero_tag, 0.0, n);
          } else {
		vec<double> ret(alloc, n);
		for(int j = 0; j < n; ++j)
			ret[j] = val;
		return ret;
          }
	}

	template <class T>
	vec<T> constVec(allocator * alloc, int n, T val)
	{
		vec<T> ret(alloc, n);
		for(int j = 0; j < n; ++j)
			ret[j] = val;
		return ret;
	}

	template <class T, class dT>
        vec<dT> fwd$constVec(allocator * alloc, std::tuple<int, T> n_val, std::tuple<std::tuple<>, dT> unit_dval)
	{
                auto n = std::get<0>(n_val);
                auto dval = std::get<1>(unit_dval);
		return constVec(alloc, n, dval);
	}

	template <class F>
	auto diag(allocator * alloc, int rows, int cols, F f)
	{
		KS_ASSERT(rows == cols);
		typedef decltype(f(int{})) T;
		return build<vec<T>>(alloc, rows, [cols,f](allocator * alloc, int i) { 
					return deltaVec(alloc, cols, i, f(i)); 
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
	vec<T> ts_add(allocator * alloc, vec<T> const& a, vec<T> const& b)
	{
		if (a.is_zero())
			return b;

		if (b.is_zero())
			return a;

		KS_ASSERT(a.size() == b.size());
		vec<T> ret = vec<T>::create(alloc, a.size());

		for (int i = 0; i < a.size(); ++i)
			ret[i] = ts_add(alloc, a[i], b[i]);
		return ret;
	}

	template <class T>
	T ts_scale(allocator * alloc, double s, T const& t);

	// Scale a vec
	template <class T>
	vec<T> ts_scale(allocator * alloc, double val, vec<T> const& v)
	{
		if (v.is_zero())
			return v;

		KS_ASSERT(v.size() != 0);
		int size = v.size();
		vec<T> ret = vec<T>::create(alloc, size);
		for (int i = 0; i < size; ++i)
			ret[i] = ts_scale(alloc, val, v[i]);
		return ret;
	}

	// sum of elements
	template <class T>
	T sum(allocator * alloc, vec<T> const& v)
	{
		if (v.is_zero()) {
			std::cerr <<"okkk?";
			return zero(T{});
		}

		if (v.size() == 0) { return zero(T{}); }

		if (v.size() == 1) return v[0];
		T ret = ts_add(alloc, v[0], v[1]);
		for (int i = 2; i < v.size(); ++i)
			ret = ts_add(alloc, ret, v[i]);
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

	// ===============================  Primitives  ==================================

	tuple<> shape(allocator_base *, bool const&) { return {}; }
	tuple<> shape(allocator_base *, int const&) { return {}; }
	tuple<> shape(allocator_base *, double const&) { return {}; }
	tuple<> shape(allocator_base *, std::string const&) { return {}; }

	template<class T>
	auto shape(allocator_base * alloc, vec<T> const& v) {
		vec<decltype(shape(alloc, v[0]))> s(alloc, v.size());
		for (int ii = 0; ii != v.size(); ++ii) {
			s[ii] = shape(alloc, v[ii]);
		}
		return s;
	}

	template<class TupleType, size_t... Indices>
	auto shape_impl(allocator_base * alloc, TupleType const& t, std::index_sequence<Indices...>) {
		return std::make_tuple(shape(alloc, std::get<Indices>(t))...);
	}

	template<class... Types>
	auto shape(allocator_base * alloc, tuple<Types...> const& t) {
		return shape_impl(alloc, t, std::index_sequence_for<Types...>{});
	}

	auto shape(allocator_base *) { return tuple<>{}; }

	template<class T1, class T2, class... Ts>
	auto shape(allocator_base * alloc, T1 const& t1, T2 const& t2, Ts const& ...ts) {
		return std::make_tuple(shape(alloc, t1), shape(alloc, t2), shape(alloc, ts)...);
	}


	template <class T>
	T ts_scale(allocator *, double s, T const& t)
	{
		return s * t;
	}

	template <>
	inline tuple<> ts_scale(allocator *, double s, tuple<> const& t)
	{
		return t;
	}

	template <class U0, class... Us>
	auto ts_scale(allocator * alloc, double s, tuple<U0, Us...> const& t)
	{
		return prepend(ts_scale(alloc, s, head(t)),
			ts_scale(alloc, s, tail(t)));
	}

	inline
		int ts_scale(allocator *, int const& t1, int const& t2)
	{
		return t1 * t2;
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

        inline bool lt$aff(allocator *, double t1, double t2)
	{
		return t1 < t2;
	}

        inline bool lt$aii(allocator *, int t1, int t2)
	{
		return t1 < t2;
	}

	inline bool gt$aff(allocator *, double t1, double t2)
	{
		return t1 > t2;
	}

	inline bool gt$aii(allocator *, int t1, int t2)
	{
		return t1 > t2;
	}

	inline bool lte$aff(allocator *, double t1, double t2)
	{
		return t1 <= t2;
	}

	inline bool lte$aii(allocator *, int t1, int t2)
	{
		return t1 <= t2;
	}

	inline bool gte$aff(allocator *, double t1, double t2)
	{
		return t1 >= t2;
	}

	inline bool gte$aii(allocator *, int t1, int t2)
	{
		return t1 >= t2;
	}

	inline double add$aff(allocator *, double t1, double t2)
	{
		return t1 + t2;
	}

	inline int add$aii(allocator *, int t1, int t2)
	{
		return t1 + t2;
	}

	inline double mul$aff(allocator *, double t1, double t2)
	{
		return t1 * t2;
	}

	inline int mul$aii(allocator *, int t1, int t2)
	{
		return t1 * t2;
	}

	inline double abs$af(allocator *, double d) { return d > 0 ? d : -d; }

	inline double max$aff(allocator *, double a, double b) { return a > b ? a : b; }

	inline int ts_neg(allocator *, int d) { return -d; }

	inline double ts_neg(allocator *, double d) { return -d; }

        inline tuple<> ts_neg(allocator *, tuple<> d) { return d; }

        template <class U0, class... Us>
        inline tuple<U0, Us...> ts_neg(allocator * alloc, tuple<U0, Us...> t) { return prepend(ts_neg(alloc, head(t)), ts_neg(alloc, tail(t))); }

        template <class T>
        inline vec<T> ts_neg(allocator * alloc, vec<T> v) { return build<T>(alloc, v.size(), [v](allocator * alloc, int i){ return ts_neg(alloc, v[i]); }); }

	inline int to_integer(int d) { return d; }

	template<size_t I, typename TupleType>
	auto unzip_element(allocator * alloc, vec<TupleType> const& v)
	{
		vec<std::tuple_element_t<I, TupleType>> ret(alloc, v.size());
		for (int i = 0; i != v.size(); ++i)
		{
			ret[i] = std::get<I>(v[i]);
		}
		return ret;
	}
	template<typename TupleType, size_t... Indices>
	auto unzip_impl(allocator * alloc, vec<TupleType> const& v, std::index_sequence<Indices...>)
	{
		return std::make_tuple(unzip_element<Indices>(alloc, v)...);
	}
	template <class... Types>
	auto unzip(allocator * alloc, vec<tuple<Types...>> const& v)
	{
		return unzip_impl(alloc, v, std::index_sequence_for<Types...>{});
	}

	// ========================= Random primitives ============
        // ranhash functions from
        //
        //     https://mathoverflow.net/questions/104915/pseudo-random-algorithm-allowing-o1-computation-of-nth-element
        inline uint64_t $ranhash(allocator *, uint64_t v) {
          v *= 3935559000370003845LL;
          v += 2691343689449507681LL;
          v ^= v >> 21; v ^= v << 37; v ^= v >> 4;
          v *= 4768777513237032717LL;
          v ^= v << 20; v ^= v >> 41; v ^= v << 5;
          return v;
        }

        inline double $ranhashdoub$ai(allocator * alloc, int32_t v) {
          return 5.42101086242752217E-20 * $ranhash(alloc, v);
        }

	// ========================= Trace primitive ===============
	template <class T>
	T $trace(T const& a)
	{
		std::cout << "Trace[" << a << "]" << std::endl;
		return a;
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
		alloc_mark_t mark = $alloc->mark();
		BENCHMARK(
			$alloc->reset(mark);
			c$68 = gmm_knossos_gmm_objective($alloc, c$62, c$63, c$64, c$65, c$66, c$67)
		);
	*/

#define $BENCH$al$d$d$bf$b(alloc, FUN) ks::benchmark(ks::repeat([&]() { \
																		auto t = (alloc)->mark(); \
																		FUN(alloc); \
																		(alloc)->reset(t); \
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
	template<class Functor, class X>
	auto applyWithAllocator(allocator * alloc, Functor f, const X & x)
	{
		return std::apply(f, std::tuple_cat(std::make_tuple(alloc), x));
	}

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
        double $check(allocator * alloc, Functor f, RevFunctor rev_f, X x, X_ x_, Dx dx, Df df)
	{
		auto f_x = applyWithAllocator(alloc, f, x);
		auto f_x_plus_dx = applyWithAllocator(alloc, f, ts_add(alloc, x, dx));
		auto delta_f = f_x_plus_dx - f_x;
		double d1 = dot(delta_f, df);
		auto dfJ = applyWithAllocator(alloc, rev_f, std::make_tuple(x_, df));
		double d2 = dot(dfJ, dx);

		/*
		std::cout << "dfJ=" << dfJ << std::endl;
		std::cout << "DOT=" << dot(dfJ, dx) << std::endl;
		std::cout << " D1=" << d1 << std::endl;
		std::cout << " D2=" << d2 << std::endl;
		*/

		return std::abs(d1 - d2)/(std::abs(d1) + std::abs(d2));
	}
} // namespace ks

#include "knossos-lm.h"

#include "knossos-prelude.h"
