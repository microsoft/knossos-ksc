#include <stdio.h>
#include "knossos.h"
namespace ks {

	double f(vec<double> s$x, vec<double> s$y) {
		auto c$1 = lt(2, 3);

		double c$0;
		if (c$1) {  
			auto c$2 = index(1, s$x);
			;
			c$0 = c$2;
		}
		else {
			;
			c$0 = 7.0;
		}
		return c$0;
	}

	auto D$f() {
		LM::Variant<LM::BuildT<LM::One<double>>, LM::Zero<vec<double>, double>> ret;

		auto c$3 = [](int s$ii) {
			return LM::One<double>::mk(double{});
		};
		auto x = LM::BuildT<LM::One<double>>::mk(7, c$3);

		if (1)
			ret = x;
		else
			ret = LM::Zero<vec<double>, double>::mk(vec<double>{}, double{});
		return ret;
	}


	auto main() {
		return D$f();
	}

}
int main() {
	ks::main();
	return 0;
}