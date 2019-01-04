
#include "knossos.h"

#ifdef BUMPY
ks::allocator g_alloc{ 10000000 };
#endif

int ks::log_indent = 8;
bool ks::do_log = false;
std::set<void*> ks::objects;
