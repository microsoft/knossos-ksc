
#include <cassert>

#include "Parser/Assert.h"

void ks_backtrace();

asserter::asserter(char const* expr, char const* file, int line)
{
    s = &std::cerr;
    *s << "\n" << "KSC: "<< file << ":" << line << ": ASSERT FAIL[" << expr << "]";
}

asserter::~asserter()
{
    std::cerr << std::endl;
    ks_backtrace();
    exit(1);
}

#if 1
void ks_backtrace()
{
}

#else
#include <stdio.h>
#include <execinfo.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>

// Print a backtrace
void ks_backtrace()
{
    void *array[10];

    // get void*'s for all entries on the stack
    size_t size = backtrace(array, sizeof array / sizeof array[0]);

    // print out all the frames to stderr
    backtrace_symbols_fd(array, size, STDERR_FILENO);
}
#endif