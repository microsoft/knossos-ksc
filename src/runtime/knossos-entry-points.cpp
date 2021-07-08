#include "knossos-entry-points.h"

namespace ks {
namespace entry_points {

ks::allocator g_alloc{ 1'000'000'000 };

void reset_allocator() { g_alloc.reset(); }
size_t allocator_top() { return g_alloc.mark(); }
size_t allocator_peak() { return g_alloc.peak(); }

bool g_logging = false;

bool logging(bool enable) {
    bool prev = g_logging;
    g_logging = enable;
    return prev;
}

}
}
