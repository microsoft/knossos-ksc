// C++ "runtime" for Knossos -- definitions corresponding to prelude.ks
#pragma once

#include <random>
#include <cmath>

#include "knossos.h"

namespace ks {
	inline double sub_TODO_unused(double t1, double t2) { return t1 - t2; }
	inline int sub_TODO_unused(int t1, int t2) { return t1 - t2; }

    inline double sub$aff(double t1, double t2)
	{
		return t1 - t2;
	}

	inline auto D$sub$aff(double, double) 
	{
		typedef LM::Scale M1;
		typedef LM::Scale M2;
		return LM::HCat<M1, M2>::mk(M1::mk(1.0), M2::mk(-1.0));
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

	template <class T1, class T2>
	T1 div_unusedTODO(T1 t1, T2 t2)
	{
		return t1 / t2;
	}

	inline
	auto D$div$aff(double t1, double t2)
	{
		return LM::HCat<LM::Scale, LM::Scale>::mk(LM::Scale::mk(1.0 / t2), LM::Scale::mk(-1.0 / (t1*t1)));
	}

	inline double log$af(double d) { return log(d); }
	inline double exp$af(double d) { return exp(d); }
	inline double sin$af(double d) { return sin(d); }
	inline double cos$af(double d) { return cos(d); }
	inline double tanh$af(double d) { return tanh(d); }

	inline double to_float$ai(int d) { return d; }
	inline auto D$to_float$ai(int d) { return LM::Zero<int, double>(); }
}
