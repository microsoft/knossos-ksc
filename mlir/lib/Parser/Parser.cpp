/* Copyright Microsoft Corp. 2020 */
#include <iostream>

#include "Parser/Parser.h"

using namespace std;
using namespace Knossos::AST;

#define PARSE_ASSERT2(p, loctok) ASSERT(p) << "\nat " << tok << "\n" << loctok->getLocation() << ": error: "
#define PARSE_ASSERT(p) PARSE_ASSERT2(p, tok)

//================================================ Helpers

static Type::ValidType Str2Type(llvm::StringRef ty) {
  if (ty == "String")
    return Type::String;
  if (ty == "Bool")
    return Type::Bool;
  if (ty == "Integer")
    return Type::Integer;
  if (ty == "Float")
    return Type::Float;
  if (ty == "Tuple")
    return Type::Tuple;
  if (ty == "Vec")
    return Type::Vector;
  if (ty == "Lambda")
    return Type::Lambda;
  if (ty == "LM")
    return Type::LM;

  return Type::None;
}

static size_t toIndex(llvm::StringRef str) {
  size_t idx = 0;
  bool failed = str.getAsInteger(0, idx);
  assert(!failed && "Bad index conversion");
  return idx;
}

static Type::ValidType LiteralType(llvm::StringRef str) {
  // String
  if (str[0] == '"' && str[str.size() - 1] == '"')
    return Type::String;
  // Bool
  if (str == "true" || str == "false")
    return Type::Bool;
  // Number
  int i;
  if (!str.getAsInteger(0, i))
    return Type::Integer;
  double d;
  if (!str.getAsDouble(d))
    return Type::Float;

  // No literals for Tuple, Vec, Lambda, LM
  return Type::None;
}

static llvm::StringRef unquote(llvm::StringRef original) {
  size_t start = 0, len = original.size();
  if (original.front() == '"') { start++; len--; }
  if (original.back() == '"') len--;
  return original.substr(start, len);
}

static Literal::Ptr getZero(Type type) {
  switch (type.getValidType()) {
  case Type::Integer:
    return make_unique<Literal>("0", Type::Integer);
  case Type::Float:
    return make_unique<Literal>("0.0", Type::Float);
  case Type::Bool:
    return make_unique<Literal>("false", Type::Bool);
  case Type::String:
    return make_unique<Literal>("", Type::String);
  default:
    ASSERT(0) << "Invalid zero type [" << type << "]";
  }
}

//
Declaration *Parser::addExtraDecl(std::string name, std::vector<Type> argTypes, Type returnType) {
  Signature sig {name, argTypes};
  auto& decl = function_decls[sig];
  if (decl) {
    std::cerr << "[addExtraDecl: already present: " << sig << "]";
    return decl;
  }

  // Not present, cons one up
  decl = new Declaration(name, returnType, argTypes);
  extraDecls->addOperand(Expr::Ptr(decl));
  
  return decl;
}

//================================================ Parse Tokens into Exprs
struct ParseEnter {
  static int verbose;
  static int indent;

  ParseEnter(char const* function, const Token* tok) { 
    ++indent;
    if (verbose > 0) 
      std::cerr 
        << std::string(indent, ' ') 
        << "Enter [" << function << "]:" << tok; 
  }
  ~ParseEnter() {
    --indent;
  }
};
int ParseEnter::indent = 0;
int ParseEnter::verbose = 0;
#define PARSE_ENTER ParseEnter parse_enter { __FUNCTION__, tok }

// Creates an Expr for each token, validating
Expr::Ptr Parser::parseToken(const Token *tok) {
  PARSE_ENTER;

  // Values are all strings (numbers will be converted later)
  // They could be variable use, type names, literals
  if (tok->isValue) {
    return parseValue(tok);
  }

  // Empty block
  if (tok->size() == 0) {
    std::cerr << "EMPTY BLOCK!\n";
    return unique_ptr<Expr>(new Block());
  }

  // If the first expr is not a value, this is a block of blocks
  auto head = tok->getHead();
  if (!head->isValue) {
    // This is an actual block
    return parseBlock(tok);
  }

  // First child is a value, can be all sorts of block types
  // Check first value for type
  auto value = tok->getHead()->getValue();

  // Constructs: let, edef, def, if, fold, etc.
  switch (isReservedWord(value)) {
  // "Builtins": Core language constructs
  case Parser::Keyword::EDEF:
    return parseDecl(tok);
  case Parser::Keyword::DEF:
    return parseDef(tok);
  case Parser::Keyword::RULE:
    return parseRule(tok);
  case Parser::Keyword::LET:
    return parseLet(tok);
  case Parser::Keyword::IF:
    return parseCond(tok);
  case Parser::Keyword::TUPLE:
    return parseTuple(tok);

  // "Prims": Polymorphic functions, rather than builtins
  case Parser::Keyword::GET:
    return parseGet(tok);
  case Parser::Keyword::BUILD:
    return parseBuild(tok);
  case Parser::Keyword::FOLD:
    return parseFold(tok);
  case Parser::Keyword::NA:
    // It's not a reserved keyword, try other constructs
    break;
  }

  // Everything else is a function call: (fun 10.0 42 "Hello")
  return parseCall(tok);
}

// Values (variable names, literals, type names)
Block::Ptr Parser::parseBlock(const Token *tok) {
  PARSE_ENTER;
  Block *b = new Block();
  for (auto &c : tok->getChildren())
    b->addOperand(parseToken(c.get()));
  return unique_ptr<Block>(b);
}

// Parses type declarations (vector, tuples)
Type Parser::parseType(const Token *tok) {
  PARSE_ENTER;
  // Scalar type
  if (tok->isValue) {
    Type::ValidType ty = Str2Type(tok->getValue());
    PARSE_ASSERT(ty <= Type::LAST_SCALAR) << "Non-scalar types must be parenthesised";
    return Type(ty);
  }

  // Empty type
  PARSE_ASSERT(tok->size() != 0) << "Empty Type decl";
  size_t nargs = tok->size() - 1;

  auto type = Str2Type(tok->getChild(0)->getValue());

  if (type == AST::Type::Vector) {
    PARSE_ASSERT(nargs == 1) << "Vector needs one type argument";
    return Type::makeVector(parseType(tok->getChild(1)));
  }

  if (type == AST::Type::Tuple) {
    std::vector<Type> types;
    for (auto &c: tok->getTail())
      types.push_back(parseType(c.get()));
    return Type(type, move(types));
  }

  if (type == AST::Type::Lambda || type == AST::Type::LM) {
    PARSE_ASSERT(nargs == 2) << "Lambda/LM need two type arguments";
    return Type(type, {parseType(tok->getChild(1)), parseType(tok->getChild(2))});
  }

  PARSE_ASSERT(0) << "Unknown type";
}

// Parses relaxed type declarations (vector, tuples)
Type Parser::parseRelaxedType(vector<const Token *> toks) {
  assert(toks[0]->isValue && "Invalid relaxed type syntax");
  auto ty = toks[0]->getValue();
  if (ty == "Vec") {
    assert(toks.size() == 2 && "Invalid relaxed vector syntax");
    return Type(Type::Vector, {parseType(toks[1])});
  } else if (ty == "Tuple") {
    vector<Type> tys;
    for (size_t i=1, e=toks.size(); i<e; i++)
      tys.push_back(parseType(toks[i]));
    return Type(Type::Tuple, tys);
  }
  assert(0 && "Invalid relaxed type syntax");
}

// Values (variable names, literals, type names)
Expr::Ptr Parser::parseValue(const Token *tok) {
  PARSE_ENTER;

  PARSE_ASSERT(tok->isValue);
  string value = tok->getValue();

  // Literals: 10.0 42 "Hello" (not hello, that's a variable use)
  Type::ValidType ty = LiteralType(value);
    
  if (ty != Type::None) {
    // Trim quotes "" from strings before creating constant
    if (ty == Type::String)
      value = value.substr(1, value.length()-2);
    return unique_ptr<Expr>(new Literal(value, ty));
  }

  // Variable use: name (without quotes)
  PARSE_ASSERT(variables.exists(value)) << "Variable not declared [" << value << "]";
  auto val = variables.get(value);
  // Create new node, referencing existing variable
  PARSE_ASSERT(Variable::classof(val));
  return unique_ptr<Expr>(new Variable(value, val->getType()));
}

// Calls (fun arg1 arg2 ...)
// Checks types agains symbol table
Call::Ptr Parser::parseCall(const Token *tok) {
  PARSE_ENTER;

  string name = tok->getHead()->getValue();
  int arity = tok->size() - 1;

  Declaration* decl = nullptr;
  
  // Collect operands
  std::vector<Expr::Ptr> operands;
  operands.reserve(arity);
  for (auto &c : tok->getTail())
    operands.push_back(parseToken(c.get()));

  // Extract types
  std::vector<Type> types;
  types.reserve(arity);
  for (auto &arg : operands)
    types.push_back(arg->getType());

  // Look up this function
  Signature sig {name, types};
  auto decl_iter = function_decls.find(sig);
  if (decl_iter != function_decls.end())
    // Non need to typecheck, the lookup succeeded.
    return make_unique<Call>(decl_iter->second, move(operands));

  // Function wasn't found - it may be a Prim that we handle here.

  // Helper to construct the call
  auto mkCall = [&name, &types, &operands, this](Type const& type) {
    Declaration* decl = addExtraDecl(name, types, type);
    return make_unique<Call>(decl, move(operands));
  };

  // TODO: should these be in Type?
  const auto Bool = Type(Type::Bool);
  const auto Float = Type(Type::Float);
  const auto Integer = Type(Type::Integer);

  // print(T1, ..., Tn) -> Integer
  if (name == "print")
    // Cons up a new decl for this combination of print and Type
    return mkCall(Integer);

#define MATCH_1(NAME, ARGTYPE_0) \
  (arity == 1 && name == NAME && \
   types[0] == Type::ARGTYPE_0)

#define MATCH_2(NAME, ARGTYPE_0, ARGTYPE_1) \
  (arity == 2 && name == NAME &&            \
   types[0] == Type::ARGTYPE_0 &&           \
   types[1] == Type::ARGTYPE_1)

  if (MATCH_1("abs", Float))     return mkCall(Float);
  if (MATCH_1("neg", Float))     return mkCall(Float);
  if (MATCH_1("exp", Float))     return mkCall(Float);
  if (MATCH_1("log", Float))     return mkCall(Float);

  if (MATCH_1("to_float", Integer))   return mkCall(Float);
  if (MATCH_1("to_int", Float))       return mkCall(Float);

  if (MATCH_2("add", Integer, Integer))   return mkCall(Integer);
  if (MATCH_2("add", Float, Float))       return mkCall(Float);
  if (MATCH_2("sub", Integer, Integer))   return mkCall(Integer);
  if (MATCH_2("sub", Float, Float))       return mkCall(Float);
  if (MATCH_2("mul", Integer, Integer))   return mkCall(Integer);
  if (MATCH_2("mul", Float, Float))       return mkCall(Float);
  if (MATCH_2("div", Integer, Integer))   return mkCall(Integer);
  if (MATCH_2("div", Float, Float))       return mkCall(Float);
  if (MATCH_2("and", Bool, Bool))         return mkCall(Bool);
  if (MATCH_2("or", Bool, Bool))          return mkCall(Bool);

  // Comparison
  if (MATCH_2("eq", Integer, Integer))      return mkCall(Bool);
  if (MATCH_2("eq", Float, Float))          return mkCall(Bool);
  if (MATCH_2("ne", Integer, Integer))      return mkCall(Bool);
  if (MATCH_2("ne", Float, Float))          return mkCall(Bool);
  if (MATCH_2("lte", Integer, Integer))     return mkCall(Bool);
  if (MATCH_2("lte", Float, Float))         return mkCall(Bool);
  if (MATCH_2("gte", Integer, Integer))     return mkCall(Bool);
  if (MATCH_2("gte", Float, Float))         return mkCall(Bool);
  if (MATCH_2("gt", Integer, Integer))      return mkCall(Bool);
  if (MATCH_2("gt", Float, Float))          return mkCall(Bool);
  if (MATCH_2("lt", Integer, Integer))      return mkCall(Bool);
  if (MATCH_2("lt", Float, Float))          return mkCall(Bool);

  // Prims
  if (MATCH_1("size", Vector))             return mkCall(Integer);
  if (MATCH_2("index", Integer, Vector))   return mkCall(types[1].getSubType());
  if (MATCH_1("sum", Vector))              return mkCall(types[0].getSubType());

  // ts_add(T, dT)
  if (name == "ts_add" && arity == 2) {
    Type ty0 = types[0];
    Type ty1 = types[1];
    PARSE_ASSERT(ty1 == ty0.tangentType()) << "ts_add defined between tangentType and type";
    return mkCall(ty0);
  }

#undef MATCH_1
#undef MATCH_2
#undef mkCall

  // Nothing matched...
  PARSE_ASSERT2(0, tok->getHead()) << "Unrecognized function: " << sig;
}


// Variables can be declarations or definitions, depending on the arguments
// (def f Type ((v : Type)) body)  ; declaration
//              ^^^^^^^^^^
// ... (let ((v value)) body) ...  ; definition
//           ^^^^^^^^^
Variable::Ptr Parser::parseVariable(const Token *tok) {
  PARSE_ENTER;

  PARSE_ASSERT(tok->size() > 1);
  llvm::ArrayRef<Token::Ptr> children = tok->getChildren();
  string value = children[0]->getValue();

  // Variable declaration: (name : Type) : add name to SymbolTable
  // Relaxed type syntax (ex: (x : Vec Float) instead of (x : (Vec Float))
  if (tok->size() > 2 && children[1]->getValue() == ":") {
    Type type(Type::None);
    if (tok->size() == 3) {
      type = parseType(children[2].get());
    } else {
      // Re-build the type from children[2:]
      vector<const Token *> toks;
      for (size_t i=2, e=tok->size(); i<e; i++)
        toks.push_back(tok->getChild(i));
      type = parseRelaxedType(toks);
    }
    PARSE_ASSERT(!type.isNone());
    auto var = unique_ptr<Variable>(new Variable(value, type));
    variables.set(value, var.get());
    return var;
  }

  // Variable definition: (name value) : check name on SymbolTable
  if (tok->size() == 2) {
    // Add to map first, to allow recursion
    auto var = unique_ptr<Variable>(new Variable(value));
    Expr::Ptr expr = parseToken(children[1].get());
    var->setInit(move(expr));
    variables.set(value, var.get());
    return var;
  }

  PARSE_ASSERT(0) << "Invalid variable declaration (v : Type) / definition (v value)";
}

// Variable declaration: (let (x 10) (add x 10))
Let::Ptr Parser::parseLet(const Token *tok) {
  PARSE_ENTER;

  PARSE_ASSERT(tok->size() == 3);
  const Token *bond = tok->getChild(1);
  PARSE_ASSERT2(!bond->isValue, bond);
  vector<Expr::Ptr> vars;
  // Single variable binding
  if (bond->getChild(0)->isValue) {
    PARSE_ASSERT2(bond->size() == 2, bond) << "Binding (v val), not [" << bond << "]";
    vars.push_back(parseVariable(bond));
    // Multiple variables
  } else {
    for (auto &c: bond->getChildren())
      vars.push_back(parseVariable(c.get()));
  }

  if (tok->size() == 2) {
    return make_unique<Let>(move(vars));
  } else {
    auto body = parseToken(tok->getChild(2));
    return make_unique<Let>(move(vars), move(body));
  }
}

// Declares a function (and add it to symbol table)
Declaration::Ptr Parser::parseDecl(const Token *tok) {
  PARSE_ENTER;

  PARSE_ASSERT(tok->size() == 4) << "Decl should be (edef name type args)";
  const Token *name = tok->getChild(1);
  const Token *ty = tok->getChild(2);
  const Token *args = tok->getChild(3);

  PARSE_ASSERT2(name->isValue, name) << "Decl should be (edef name type args)";

  auto type = parseType(ty);
  PARSE_ASSERT2(!type.isNone(), ty) << "Parsing decl [" << name << "]";

  PARSE_ASSERT2(!args->isValue, args) << "Parsing decl [" << name << "]";

  auto decl = make_unique<Declaration>(name->getValue(), type);
  PARSE_ASSERT(decl);

  // Vector and Tuples can be declared bare
  if (args->getChild(0)->isValue &&
      !Type::isScalar(Str2Type(args->getChild(0)->getValue())))
    decl->addArgType(parseType(args));
  else
    for (auto &c : args->getChildren())
      decl->addArgType(parseType(c.get()));

  Signature sig{name->getValue(), decl->getArgTypes()};
  function_decls[sig] = decl.get();
  return decl;
}

// Defines a function (checks from|adds to symbol table)
// (def name type args expr)
Definition::Ptr Parser::parseDef(const Token *tok) {
  PARSE_ENTER;

  PARSE_ASSERT(tok->size() == 5) << "Expect def to have 4 parts (def name Type args body)";
  const Token *name = tok->getChild(1);
  const Token *tok_type = tok->getChild(2);
  const Token *args = tok->getChild(3);
  const Token *tok_body = tok->getChild(4);
  
  PARSE_ASSERT2(name->isValue, name) << "Def name should be a value, not [" << name << "]";

  auto type = parseType(tok_type);
  PARSE_ASSERT2(!type.isNone(), tok_type) << "Unknown return type [" << tok_type << "]";

  vector<Variable::Ptr> arguments;
  // Single var: (v 2.3)
  if (args->size() && args->getChild(0)->isValue)
    arguments.push_back(parseVariable(args));
  // Many vars: ((a 1) (b 2))
  else
    for (auto &a : args->getChildren())
      arguments.push_back(parseVariable(a.get()));

  // Create node early, to allow recursion
  auto node = make_unique<Definition>(name->getValue(), type);
  std::vector<Type> argTypes;
  for (auto &a : arguments) {
    PARSE_ASSERT(a->kind == Expr::Kind::Variable);
    argTypes.push_back(a->getType());
    node->addArgument(move(a));
  }
  Signature sig {name->getValue(), argTypes};
  function_decls[sig] = node->getDeclaration();
  
  // Function body is a block, create one if single expr
  auto body = parseToken(tok_body);
  PARSE_ASSERT2(type == body->getType(), tok_body) << "Return type declared as [" << type << "], but body has type [" << body->getType() << "]";

  node->setImpl(move(body));

  return node;
}

// Conditional: (if (cond) (true block) (false block))
Condition::Ptr Parser::parseCond(const Token *tok) {
  PARSE_ENTER;

  PARSE_ASSERT(tok->size() == 4);
  auto c = parseToken(tok->getChild(1));
  auto i = parseToken(tok->getChild(2));
  auto e = parseToken(tok->getChild(3));
  return make_unique<Condition>(move(c), move(i), move(e));
}

// Loops, ex: (build N (lam (i : Integer) (add i i)))
Build::Ptr Parser::parseBuild(const Token *tok) {
  PARSE_ENTER;

  PARSE_ASSERT(tok->size() == 3);

  // Range (can be dynamic)
  auto range = parseToken(tok->getChild(1));

  // Lambda (induction variable and loop body)
  const Token *lam = tok->getChild(2);
  PARSE_ASSERT(!lam->isValue);
  PARSE_ASSERT(lam->getChild(0)->isValue && lam->getChild(0)->getValue() == "lam");
  const Token *bond = lam->getChild(1);
  const Token *expr = lam->getChild(2);
  auto var = parseVariable(bond);
  PARSE_ASSERT(var->kind == Expr::Kind::Variable);
  PARSE_ASSERT(var->getType() == AST::Type::Integer);
  llvm::dyn_cast<Variable>(var.get())->setInit(getZero(Type(Type::Integer)));
  auto body = parseToken(expr);
  return make_unique<Build>(move(range), move(var), move(body));
}

// Tuple, ex: (tuple 10.0 42 (add 1.0 2.0))
Tuple::Ptr Parser::parseTuple(const Token *tok) {
  PARSE_ASSERT(tok->size() > 0);
  PARSE_ASSERT(tok->getChild(0)->isValue && tok->getChild(0)->getValue() == "tuple");
  std::vector<Expr::Ptr> elements;
  for (auto &c: tok->getTail())
    elements.push_back(parseToken(c.get()));
  return make_unique<Tuple>(move(elements));
}

// Index, ex: (get$7$9 tuple)
Get::Ptr Parser::parseGet(const Token *tok) {
  PARSE_ASSERT(tok->size() == 2);
  PARSE_ASSERT(tok->getChild(0)->isValue);
  llvm::StringRef get = tok->getChild(0)->getValue();
  size_t dollar1 = get.find('$');
  size_t dollar2 = get.find('$', dollar1 + 1);
  llvm::StringRef indexStr = get.substr(dollar1 + 1, dollar2 - dollar1 - 1);
  llvm::StringRef maxStr = get.substr(dollar2 + 1);
  size_t idx = toIndex(indexStr);
  size_t max = toIndex(maxStr);
  auto var = parseToken(tok->getChild(1));
  return make_unique<Get>(idx, max, move(var));
}

// Fold, ex: (fold (lambda) init vector)
Fold::Ptr Parser::parseFold(const Token *tok) {
  PARSE_ENTER;

  PARSE_ASSERT(tok->size() == 4);
  // Lambda format: (lam (acc_x : (Tuple AccTy ElmTy)) (expr))
  const Token *lam = tok->getChild(1);
  PARSE_ASSERT(!lam->isValue);
  PARSE_ASSERT(lam->getChild(0)->isValue && lam->getChild(0)->getValue() == "lam");
  auto var = parseVariable(lam->getChild(1));
  PARSE_ASSERT(var->kind == Expr::Kind::Variable);
  PARSE_ASSERT(var->getType() == Type::Tuple);
  auto accTy = var->getType().getSubType(0);
  auto elmTy = var->getType().getSubType(1);
  // Variable initialiser is (init, zero)
  vector<Expr::Ptr> initArgs;
  initArgs.push_back(parseToken(tok->getChild(2)));
  initArgs.push_back(getZero(elmTy));
  auto init = make_unique<Tuple>(move(initArgs));
  llvm::dyn_cast<Variable>(var.get())->setInit(move(init));
  // Lambda body has same as accTy
  auto body = parseToken(lam->getChild(2));
  PARSE_ASSERT(body->getType() == accTy);
  auto vector = parseToken(tok->getChild(3));
  PARSE_ASSERT(vector->getType() == Type::Vector);
  PARSE_ASSERT(vector->getType().getSubType() == elmTy);
  return make_unique<Fold>(accTy, move(body), move(var), move(vector));
}

// Rule: (rule "mul2" (v : Float) (mul v 2.0) (add v v))
Rule::Ptr Parser::parseRule(const Token *tok) {
  PARSE_ENTER;

  PARSE_ASSERT(tok->size() == 5);
  const Token *name = tok->getChild(1);
  PARSE_ASSERT(name->isValue);
  llvm::StringRef unquoted = unquote(name->getValue());
  auto var = parseVariable(tok->getChild(2));
  auto pat = parseToken(tok->getChild(3));
  auto res = parseToken(tok->getChild(4));
  auto rule =
      make_unique<Rule>(unquoted, move(var), move(pat), move(res));
  rules.set(unquoted.str(), rule.get());
  return rule;
}
