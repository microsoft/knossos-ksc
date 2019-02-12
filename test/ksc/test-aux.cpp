
// Test that inclusion of knossos.h in multiple files doesn't cause doubly-defined symbols
#include "knossos.h"

static void test_ctor()
{
	ks::vec<double> vv{ 3 };
}
