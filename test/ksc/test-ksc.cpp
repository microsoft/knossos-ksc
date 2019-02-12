
#define main ks_main
#include "obj/test/ksc/gmm.cpp"
#undef main

void test_ctor();
int main() {
	test_ctor();
	ks_main();
	return 17;
}
