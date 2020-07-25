/* Copyright Microsoft Corp. 2020 */
#include <iostream>

#include "Parser/AST.h"

using namespace std;
using namespace Knossos::AST;

string Type2Str(Type::ValidType type) {
  switch (type) {
  case Type::None:
    return "none";
  case Type::String:
    return "String";
  case Type::Bool:
    return "Bool";
  case Type::Integer:
    return "Integer";
  case Type::Float:
    return "Float";
  case Type::Tuple:
    return "Tuple";
  case Type::Vector:
    return "Vec";
  case Type::Lambda:
    return "Lambda";
  case Type::LM:
    return "LM";
  }
}

std::ostream& Type::dump(std::ostream& s) const {
  if (type == Vector) {
    s << "(Vector ";
    subTypes[0].dump(s);
    return s << " )";
  } 
  if (type == Tuple) {
    s << "Tuple{ ";
    for (auto &t: subTypes)
      t.dump(s) << " ";
    return s << "}";
  } 
  
  return s << Type2Str(type);
}

std::ostream&  Expr::dump(std::ostream& s, size_t tab) const {
  s << string(tab, ' ') << "type [";
  type.dump(s);
  return s << "]" << endl;
}

std::ostream&  Block::dump(std::ostream& s, size_t tab) const {
  s << string(tab, ' ') << "Block:" << endl;
  for (auto &op : operands)
    op->dump(s, tab + 2);
}

std::ostream& Literal::dump(std::ostream& s, size_t tab) const {
  s << string(tab, ' ') << "Literal:" << endl;
  s << string(tab + 2, ' ') << "value [" << value << "]" << endl;
  return Expr::dump(s, tab + 2);
}

std::ostream& Variable::dump(std::ostream& s, size_t tab) const {
  s << string(tab, ' ') << "Variable:" << endl;
  s << string(tab + 2, ' ') << "name [" << name << "]" << endl;
  Expr::dump(s, tab + 2);
  if (init)
    init->dump(s, tab + 2);
  return s;
}

std::ostream&  Let::dump(std::ostream& s, size_t tab) const {
  s << string(tab, ' ') << "Let:" << endl;
  Expr::dump(s, tab + 2);
  for (auto &v: vars)
    v->dump(s, tab + 2);
  if (expr)
    expr->dump(s, tab + 2);
  return s;
}

std::ostream& Call::dump(std::ostream& s, size_t tab) const {
  s << string(tab, ' ') << "Call:" << endl;
  s << string(tab + 2, ' ') << "name [" << name << "]" << endl;
  Expr::dump(s, tab + 2);
  for (auto &op : operands)
    op->dump(s, tab + 2);
  return s;
}

std::ostream& Declaration::dump(std::ostream& s, size_t tab) const {
  s << string(tab, ' ') << "Declaration:" << endl;
  s << string(tab + 2, ' ') << "name [" << name << "]" << endl;
  Expr::dump(s, tab + 2);
  s << string(tab + 2, ' ') << "Types: [ ";
  for (auto ty : argTypes)
    ty.dump(s) << " ";
  return s << "]" << endl;
}

std::ostream& Definition::dump(std::ostream& s, size_t tab) const {
  s << string(tab, ' ') << "Definition:" << endl;
  s << string(tab + 2, ' ') << "name [" << decl->getName().data() << "]"
       << endl;
  Expr::dump(s, tab + 2);
  s << string(tab + 2, ' ') << "Arguments:" << endl;
  for (auto &op : arguments)
    op->dump(s, tab + 4);
  s << string(tab + 2, ' ') << "Implementation:" << endl;
  return impl->dump(s, tab + 4);
}

std::ostream& Condition::dump(std::ostream& s, size_t tab) const {
  s << string(tab, ' ') << "Condition:" << endl;
  cond->dump(s, tab + 2);
  s << string(tab + 2, ' ') << "True branch:" << endl;
  ifBlock->dump(s, tab + 4);
  s << string(tab + 2, ' ') << "False branch:" << endl;
  elseBlock->dump(s, tab + 4);
  return s;
}

std::ostream& Build::dump(std::ostream& s, size_t tab) const {
  s << string(tab, ' ') << "Build:" << endl;
  Expr::dump(s, tab + 2);
  s << string(tab + 2, ' ') << "Range:" << endl;
  range->dump(s, tab + 4);
  s << string(tab + 2, ' ') << "Induction:" << endl;
  var->dump(s, tab + 4);
  s << string(tab + 2, ' ') << "Body:" << endl;
  return expr->dump(s, tab + 4);
}

std::ostream& Tuple::dump(std::ostream& s, size_t tab) const {
  s << string(tab, ' ') << "Tuple:" << endl;
  Expr::dump(s, tab + 2);
  s << string(tab + 2, ' ') << "Values:" << endl;
  for (auto &el: elements)
    el->dump(s, tab + 4);
  return s;
}

std::ostream& Get::dump(std::ostream& s, size_t tab) const {
  s << string(tab, ' ') << "Get:" << endl;
  Expr::dump(s, tab + 2);
  s << string(tab + 2, ' ') << "index [" << index << "]" << endl;
  s << string(tab + 2, ' ') << "From:" << endl;
  expr->dump(s, tab + 4);
  return s;
}

std::ostream& Fold::dump(std::ostream& s, size_t tab) const {
  s << string(tab, ' ') << "Fold:" << endl;
  Expr::dump(s, tab + 2);
  s << string(tab + 2, ' ') << "Lambda:" << endl;
  body->dump(s, tab + 4);
  s << string(tab + 2, ' ') << "Accumulator:" << endl;
  acc->dump(s, tab + 4);
  s << string(tab + 2, ' ') << "Vector:" << endl;
  vector->dump(s, tab + 4);
  return s;
}

std::ostream&  Rule::dump(std::ostream& s, size_t tab) const {
  s << string(tab, ' ') << "Rule:" << endl;
  s << string(tab + 2, ' ') << "name [" << name << "]"
       << endl;
  Expr::dump(s, tab + 2);
  s << string(tab + 2, ' ') << "Variable:" << endl;
  variable->dump(s, tab + 4);
  s << string(tab + 2, ' ') << "Pattern:" << endl;
  pattern->dump(s, tab + 4);
  s << string(tab + 2, ' ') << "Result:" << endl;
  result->dump(s, tab + 4);
  return s;
}
