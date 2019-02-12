
// Test that inclusion of knossos.h in multiple files doesn't cause doubly-defined symbols
#include "knossos.h"

void test_ctor()
{
	std::vector<double> v{ 0.1, 0.2, 0.3 };
	ks::vec<double> vv{ v };
}
