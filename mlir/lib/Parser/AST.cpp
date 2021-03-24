/* Copyright Microsoft Corp. 2020 */
#include <iostream>

#include "Parser/AST.h"

using namespace std;

namespace Knossos { namespace AST {

char const* ValidType2Str(Type::ValidType type) {
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
  
  return s << ValidType2Str(type);
}

std::ostream& operator<<(std::ostream& s, Signature const& t)
{
  s << t.name << "(";
  bool first = true;
  for(auto ty : t.argTypes) {
    if (!first)
      s << ",";
    s << ty;
    first = false;
  }
  return s << ")";
}

std::ostream& operator<<(std::ostream& s, StructuredName const& t)
{
  for (auto& derivation : t.derivations) {
    s << '[' << derivation << ' ';
  }
  if (t.hasType()) {
    s << '[' << t.baseFunctionName << ' ' << t.baseFunctionArgType << ']';
  } else {
    s << t.baseFunctionName;
  }
  for (auto& derivation : t.derivations) {
    s << ']';
  }
  return s;
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
  return s;
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
  Expr::dump(s, tab + 2);
  decl->dump(s, tab + 2);
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
  s << string(tab + 2, ' ') << "name [" << decl->getName() << "]"
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
  s << string(tab + 2, ' ') << "Accumulator:" << endl;
  lambdaParameter->dump(s, tab + 4);
  s << string(tab + 2, ' ') << "Lambda:" << endl;
  body->dump(s, tab + 4);
  s << string(tab + 2, ' ') << "Init: " << endl;
  init->dump(s, tab + 4);
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

// Mangle to be consistent with Cgen.hs:mangleType
std::string mangleType(Type const& t)
{
  switch (t.getValidType()) {
    case Type::Bool: return "b";
    case Type::Integer: return "i";
    case Type::Float: return "f";
    case Type::String: return "s";
  }

  if (t.isTuple()) {
    std::string ret = "<";
    for(auto &ty: t.getSubTypes())
      ret += mangleType(ty);
    return ret + ">";
  }

  if (t.isVector())
    return "v" + mangleType(t.getSubType());

  ASSERT(0) << "Cannot mangle type " << t;
  return "*FAIL*";
}

std::string translate(char c)
{
  switch (c) {
    case '@': return "$a";
    case ',': return "$_";
    case '[': return "$6";
    case ']': return "$9";
    case '<': return "$d";
    case '>': return "$b";
    case '*': return "$x";
  }
  return std::string(1, c);
}

std::string encodeName(std::string const& s)
{
    std::string ret;
    for(auto c: s)
      ret += translate(c);
    return ret;
}

std::string Declaration::getMangledName() const {
  if (name.hasType())
    return name.getMangledName();
  
  /* If the function name is plain identifier rather than
     a structured name, we mangle the argument types
     of the function in order to support overloading.
     Note that this is different from the bejaviour of
     ksc Cgen, which generates unmangled names for primitive
     functions (because they can be implemented as
     C++ template functions). */
  if (argTypes.size() == 0)
    return name.baseFunctionName;
  std::string ret = name.baseFunctionName + "@";
  for(auto &ty: argTypes)
      ret += mangleType(ty);
  return encodeName(ret);
}

std::string StructuredName::getMangledName() const {
  std::string ret;
  for (auto& derivation : derivations) {
    ret += derivation;
    ret += '$';
  }
  ret += baseFunctionName;
  if (hasType()) {
    if (baseFunctionArgType.isTuple()) {
      if (!baseFunctionArgType.getSubTypes().empty()) {
        ret += '@';
        for (auto &ty : baseFunctionArgType.getSubTypes()) {
          ret += mangleType(ty);
        }
      }
    } else {
      ret += '@';
      ret += mangleType(baseFunctionArgType);
    }
  }
  return encodeName(ret);
}

}}
