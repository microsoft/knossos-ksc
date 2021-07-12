#pragma once

#include "knossos.h"

#include <iostream>

namespace ks {
namespace entry_points {

extern ks::allocator g_alloc;
void reset_allocator();
size_t allocator_top();
size_t allocator_peak();

extern bool g_logging;
bool logging(bool enable);

// Convert functor to one which takes a first argument g_alloc,
// and optionally logs inputs and outputs to cerr
template<typename RetType, typename... ParamTypes>
auto with_ks_allocator(const char * tracingMessage, RetType(*f)(ks::allocator*, ParamTypes...)) {
  return [f, tracingMessage](ParamTypes... params) {
    if (g_logging) {
        std::cerr << tracingMessage << "(";
        (std::cerr << ... << params);
        std::cerr << ") =" << std::endl;
        auto ret = f(&g_alloc, params...);
        std::cerr << ret << std::endl;
        return ret;
    } else {
        return f(&g_alloc, params...);
    }
  };
}

}
}

