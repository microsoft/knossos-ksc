
namespace ks {

inline auto D$sub$aff(allocator *, double, double)
{
	typedef LM::Scale M1;
	typedef LM::Scale M2;
	return LM::HCat<M1, M2>::mk(M1::mk(1.0), M2::mk(-1.0));
}

inline auto D$div$aff(allocator *, double t1, double t2)
{
	return LM::HCat<LM::Scale, LM::Scale>::mk(LM::Scale::mk(1.0 / t2), LM::Scale::mk(-1.0 / (t1*t1)));
}

inline auto D$to_float$ai(allocator *, int d) { return LM::Zero<int, double>(); }
inline auto D$to_float(allocator *, int d) { return LM::Zero<int, double>(); }

}


