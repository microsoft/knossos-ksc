/* Copyright Microsoft Corp. 2020 */
#ifndef _KSC_ASSERT_H_
#define _KSC_ASSERT_H_

#include <iostream>

/* 

More flexible ASSERT.

Usage:

  ASSERT(condition) << msg << value << ...;

Note:

  If the assert passes, there is no cost, otherwise an ostream is returned which can print anything an ostream can.

*/

#define ASSERT(p) if (p) ; else asserter() += asserter::startAssertMessage(#p, __FILE__, __LINE__)

struct asserter {
  static std::ostream & startAssertMessage(char const * expr, char const* file, int line);
  void operator += [[noreturn]] (std::ostream & message);  // could pick any operator with a lower precedence than <<
};

#endif /// _KSC_ASSERT_H_
