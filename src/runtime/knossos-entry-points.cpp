#include "knossos-entry-points.h"

namespace ks {
namespace entry_points {

#ifdef KS_ALLOCATOR

ks::allocator g_alloc{ 16'000'000'000 };

void reset_allocator() { g_alloc.reset(); }
size_t allocator_top() { return g_alloc.mark(); }
size_t allocator_peak() { return g_alloc.peak(); }

#else

void reset_allocator() { }
size_t allocator_top() { return 0u; }
size_t allocator_peak() { return 0u; }

#endif

}
}
