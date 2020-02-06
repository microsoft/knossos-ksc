#include <iostream>
#include "Parser.h"

using namespace Knossos::Parser;
using namespace std;

static size_t tab = 0;
void dumpToken(const Token *tok) {
  if (tok->isValue()) {
    cout << string(tab, ' ') << "tok(" << tok->getValue() << ")\n";
    return;
  }

  cout << string(tab, ' ') << "enter\n";
  tab += 2;
  for (size_t i=0; i<tok->numChildren(); i++)
    dumpToken(tok->getChild(i));
  tab -= 2;
  cout << string(tab, ' ') << "exit\n";
}

int test_simple() {
  string code = "(def f1 Integer ((x : Integer) (y : Integer)) (add@ii x y))";
  Lexer l(code);
  const Token* root = l.lex();
  cout << " == test_simple\n";
  dumpToken(root);

  // Root can have many exprs, here only one
  assert(root->isValue() == false);
  const Token* tok = root->getChild(0);
  // Op is not a value and has 5 sub-exprs
  assert(tok->isValue() == false);
  assert(tok->numChildren() == 5);
  // Which are:
  assert(tok->getChild(0)->getValue() == "def");
  assert(tok->getChild(1)->getValue() == "f1");
  assert(tok->getChild(2)->getValue() == "Integer");
  const Token* args = tok->getChild(3);
  const Token* arg0 = args->getChild(0);
  assert(arg0->getChild(0)->getValue() == "x");
  assert(arg0->getChild(1)->getValue() == ":");
  assert(arg0->getChild(2)->getValue() == "Integer");
  const Token* arg1 = args->getChild(1);
  assert(arg1->getChild(0)->getValue() == "y");
  assert(arg1->getChild(1)->getValue() == ":");
  assert(arg1->getChild(2)->getValue() == "Integer");
  const Token* impl = tok->getChild(4);
  assert(impl->getChild(0)->getValue() == "add@ii");
  assert(impl->getChild(1)->getValue() == "x");
  assert(impl->getChild(2)->getValue() == "y");

  return 0;
}
int main() {
  int error = 0;
  error |= test_simple();

  return error;
}
