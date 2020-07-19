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

#define ASSERT(p) if (p) ; else asserter(#p, __FILE__, __LINE__)

struct asserter {
  std::ostream* s;
 
  asserter(char const* expr, char const* file, int line);
  ~asserter();
 
  template <class T>
  std::ostream& operator<<(const T& t) { return *s << t; }
};

#endif _KSC_ASSERT_H_
