
namespace ks
{
	namespace LM
	{
		// ---------------- One  ------------------
		template <class T>
		struct One
		{
			typedef T To;
			typedef T From;

			static One mk(T) { return One{}; }

			To Apply(allocator *, From f) const { return f; }
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

			To Apply(allocator *, From f) const {
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
		struct Scale
		{
			typedef double scale_t;
			scale_t val;

			typedef double To;
			typedef double From;

			static Scale mk(scale_t val) { return Scale{ val }; }

			To Apply(allocator *, From f) const { return val * f; }
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

			To Apply(allocator * alloc, From f) const { return ts_add(alloc, lm1.Apply(alloc, f), lm2.Apply(alloc, f)); }
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

			To Apply(allocator * alloc, From const& f) const {
				static_assert(n > 1);
				typedef typename std::tuple_element<0, Tup>::type T0;
				To a0 = head(lms).Apply(alloc, typename T0::From{ head(f) });
				return Apply_aux<1>(alloc, a0, f);
			}

			template <size_t i>
			To Apply_aux(allocator * alloc, To accum, From const& f) const {
				typedef typename std::tuple_element<i, Tup>::type T0;
				To ai = std::get<i>(lms).Apply(alloc, typename T0::From{ std::get<i>(f) });
				if constexpr (i + 1 < n)
					return Apply_aux<i + 1>(alloc, ts_add(alloc, accum, ai), f);
				else
					return ts_add(alloc, accum, ai);
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

			To Apply(allocator * alloc, From const& f) const {
				static_assert(n > 1);
				To ret;
				Apply_aux<0>(alloc, f, &ret);
				return ret;
			}

			template <size_t i>
			void Apply_aux(allocator * alloc, From const& f, To* ret) const
			{
				typedef typename std::tuple_element<i, Tup>::type LM;
				typedef typename LM::To type;
				type ai = std::get<i>(lms).Apply(alloc, f);
				std::get<i>(*ret) = type{ ai };
				if constexpr (i + 1 < n) {
					Apply_aux<i + 1>(alloc, f, ret);
				}
			}
			/*
						template <size_t i>
						To Apply_aux(allocator * alloc, zero_t<To> accum, From const& f) const {
							if constexpr (i < n) {
								// Accumulator could be zero if first terms are zero,
								// just move on to case 1
								auto ai = std::get<i>(lms).Apply(alloc, std::get<i>(f));
								return Apply_aux<i + 1>(alloc, ai, f);
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

			To Apply(allocator * alloc, From f) const {
				auto g = ab.Apply(alloc, f);
				return bc.Apply(alloc, g);
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

			To Apply(allocator *, From f) const { return std::get(f, index); }
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

			To Apply(allocator * alloc, From x) const
			{
				Functor/*std::function<L(int)>*/ f1 = f;
				return build<LTo>(alloc, n, [x, f1](allocator * alloc, int i) {
					auto lm = f1(i);
					return lmApply(alloc, lm, x);
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

			To Apply(allocator * alloc, From x) const
			{
				if (n != x.size())
					std::cerr << "BuildT:" << n << " != " << x.size() << std::endl;
				ASSERT(n == x.size());        // TODO: copying arrays here -- should not need to..
				std::function<L(int)> f_local = f;  // TODO: use sumbuild
				return sumbuild<LFrom>(alloc, n, [f_local,x](allocator * alloc, int i) { return lmApply(alloc, f_local(i), x[i]); });
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

			To Apply(allocator * alloc, From f) const {
				if (v.index() == 0) return std::get<0>(v).Apply(alloc, f);
				if (v.index() == 1) return std::get<1>(v).Apply(alloc, f);
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
		typename LM::To lmApply(allocator * alloc, LM lm, typename LM::From a) {
			return typename LM::To{ lm.Apply(alloc, a) };
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


	template <class T1, class T2>
	auto D$ts_scale(T1 t1, T2 t2)
	{
		return LM::HCat<LM::Scale, LM::Scale>::mk(LM::Scale::mk(t2), LM::Scale::mk(t1));
	}

	inline auto D$abs$af(allocator *, double d) { return LM::Scale::mk(d > 0 ? 1.0 : -1.0); }

	inline auto D$max$aff(allocator *, double a, double b) {
		double s = a > b ? 1.0 : 0.0;
		return LM::HCat<LM::Scale, LM::Scale>::mk(LM::Scale::mk(s), LM::Scale::mk(1.0 - s));
	}

	inline auto D$ts_neg(allocator *, double d) { return LM::Scale::mk(-1.0); }

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

	template <class T>
	LM::One<T> D$$trace(T const& a)
	{
		std::cout << "Grad Trace[" << a << "]" << std::endl;
		return LM::One<T>::mk(a);
	}
} // namespace ks
