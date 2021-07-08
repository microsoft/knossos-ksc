#include <stdio.h>
#include "knossos.h"
namespace ks {

	Float f(vec<Float> s$x, vec<Float> s$y) {
		auto c$1 = lt(2, 3);

		Float c$0;
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
		LM::Variant<LM::BuildT<LM::One<Float>>, LM::Zero<vec<Float>, Float>> ret;

		auto c$3 = [](int s$ii) {
			return LM::One<Float>::mk(Float{});
		};
		auto x = LM::BuildT<LM::One<Float>>::mk(7, c$3);

		if (1)
			ret.v = x;
		else
			ret.v = LM::Zero<vec<Float>, Float>::mk(vec<Float>{}, Float{});
		return ret;
	}


	auto main() {

		return D$f();
	}

}
int main() {
	ks::test_type_to_string();
	ks::main();
	return 0;
}