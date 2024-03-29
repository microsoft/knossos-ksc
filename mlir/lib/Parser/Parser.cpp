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

//
Declaration *Parser::addExtraDecl(StructuredName const& name, Type argType, Type returnType) {
  Signature sig {name, argType};
  auto& decl = function_decls[sig];
  if (decl) {
    std::cerr << "[addExtraDecl: already present: " << sig << "]";
    return decl;
  }

  // Not present, cons one up
  decl = new Declaration(sig, returnType);
  extraDecls->addOperand(Expr::Ptr(decl));
  
  return decl;
}

//================================================ Parser stack for debugging
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


//================================================ Parse Tokens into Exprs
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
    if (head->isSquareBracket()) {
      // First expr is a structured name: this must be a call
      return parseCall(tok);
    } else {
      // This is an actual block
      return parseBlock(tok);
    }
  }

  // First child is a value, can be all sorts of block types
  // Check first value for type
  auto value = tok->getHead()->getValue();

  // Constructs: let, edef, def, if, fold, etc.
  switch (isReservedWord(value)) {
  // "Builtins": Core language constructs
  case Parser::Keyword::EDEF:
    return parseEDef(tok);
  case Parser::Keyword::DEF:
    return parseDef(tok);
  case Parser::Keyword::GDEF:
    PARSE_ASSERT(0) << "gdef not supported: definitions must be generated by ksc";
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

StructuredName Parser::parseStructuredName(const Token *tok) {
  if (tok->isValue)
    return StructuredName(tok->getValue());
  else
    return parseStructuredNameWithType(tok);
}

StructuredName Parser::parseStructuredNameWithType(const Token * tok) {
  PARSE_ASSERT(tok->size() == 2 && tok->getChild(0)->isValue) << "Structured name is either [<id> <type>] or [<derivation> <sname>], not " << tok;
  std::string id = tok->getChild(0)->getValue();
  Token const* snd = tok->getChild(1);
  if (!snd->isValue && snd->isSquareBracket())
    return StructuredName(id, parseStructuredNameWithType(snd));
  else
    return StructuredName(id, parseType(snd));
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

Type oneArgify(std::vector<Type> const& argTypes) {
  if (argTypes.size() == 1)
    return argTypes[0];
  else
    return Type(Type::Tuple, argTypes);
}

struct FakePrimitiveFunction
{
  const char * name;
  Type argType;
  Type resultType;
};
/* These are functions which aren't actually treated as primitives
   by ksc, but have previously been treated as primitives as
   ksc-mlir (ie. they are not written using structured names).
   To keep the tests working, we continue to hard-code
   their result types for now.
   TODO: update the tests to use the prelude, then remove this completely.
   */
std::vector<FakePrimitiveFunction> getFakePrims() {
  const auto Bool = Type(Type::Bool);
  const auto Float = Type(Type::Float);
  const auto Integer = Type(Type::Integer);
  return {
    { "abs", Float, Float },
    { "neg", Float, Float },
    { "exp", Float, Float },
    { "log", Float, Float },
    { "to_float", Integer, Float },
    { "add", Type::makeTuple({Integer, Integer}), Integer },
    { "add", Type::makeTuple({Float, Float}), Float },
    { "sub", Type::makeTuple({Integer, Integer}), Integer },
    { "sub", Type::makeTuple({Float, Float}), Float },
    { "mul", Type::makeTuple({Integer, Integer}), Integer },
    { "mul", Type::makeTuple({Float, Float}), Float },
    { "div", Type::makeTuple({Integer, Integer}), Integer },
    { "div", Type::makeTuple({Float, Float}), Float },
    { "and", Type::makeTuple({Bool, Bool}), Bool },
    { "or", Type::makeTuple({Bool, Bool}), Bool },
    { "lte", Type::makeTuple({Integer, Integer}), Bool },
    { "lte", Type::makeTuple({Float, Float}), Bool },
    { "gte", Type::makeTuple({Integer, Integer}), Bool },
    { "gte", Type::makeTuple({Float, Float}), Bool },
    { "lt", Type::makeTuple({Integer, Integer}), Bool },
    { "lt", Type::makeTuple({Float, Float}), Bool },
    { "gt", Type::makeTuple({Integer, Integer}), Bool },
    { "gt", Type::makeTuple({Float, Float}), Bool },
  };
}

Type primCallResultType(const Token *tok, std::string const& primName, Type argType) {

  // print(T1, ..., Tn) -> Integer
  if (primName == "print")
    return Type(Type::Integer);

  if (primName == "eq" || primName == "ne") {
    PARSE_ASSERT(argType.isTuple(2));
    PARSE_ASSERT(argType.getSubType(0) == argType.getSubType(1));
    return Type(Type::Bool);
  }

  if (primName == "size") {
    PARSE_ASSERT(argType.isVector());
    return Type(Type::Integer);
  }

  if (primName == "index") {
    PARSE_ASSERT(argType.isTuple(2));
    PARSE_ASSERT(argType.getSubType(0) == Type::Integer);
    PARSE_ASSERT(argType.getSubType(1).isVector());
    return argType.getSubType(1).getSubType();
  }

  if (primName == "sum") {
    PARSE_ASSERT(argType.isVector());
    return argType.getSubType();
  }

  if (primName == "ts_add") {
    PARSE_ASSERT(argType.isTuple(2));
    Type ty0 = argType.getSubType(0);
    Type ty1 = argType.getSubType(1);
    PARSE_ASSERT(ty1 == ty0.tangentType()) << "ts_add defined between tangentType and type";
    return ty0;
  }

  static const auto fakePrims = getFakePrims();
  for (auto & prim : fakePrims) {
    if (primName == prim.name && argType == prim.argType)
      return prim.resultType;
  }

  PARSE_ASSERT2(0, tok->getHead()) << "Unrecognized function: " << Signature{ StructuredName(primName), argType };
}

// Calls (fun arg1 arg2 ...)
// Checks types agains symbol table
Call::Ptr Parser::parseCall(const Token *tok) {
  PARSE_ENTER;

  StructuredName name = parseStructuredName(tok->getHead());
  int arity = tok->size() - 1;

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
  Type argType = oneArgify(types);

  // Look up this function
  Signature sig {name, argType};
  auto decl_iter = function_decls.find(sig);
  if (decl_iter != function_decls.end())
    // Non need to typecheck, the lookup succeeded.
    return make_unique<Call>(decl_iter->second, move(operands));

  // Function wasn't found - it may be a Prim that we handle here.
  PARSE_ASSERT(!name.isDerivation()) << "No declaration found for derived function: " << sig;   // Prims cannot be derived functions
  PARSE_ASSERT(!name.hasType()) << "No declaration found for function: " << sig;  // Prims do not have their types specified

  Type returnType = primCallResultType(tok, name.baseFunctionName, argType);  
  Declaration* decl = addExtraDecl(name, argType, returnType);
  return make_unique<Call>(decl, move(operands));
}

// Parse a variable which is declared with its type, e.g.
// (def f Type ((v : Type)) body)  ; declaration
//              ^^^^^^^^^^
Variable::Ptr Parser::parseVariableWithType(const Token *tok) {
  PARSE_ENTER;

  PARSE_ASSERT(tok->size() >= 3);
  PARSE_ASSERT(tok->getChild(1)->isValue && tok->getChild(1)->getValue() == ":");

  llvm::ArrayRef<Token::Ptr> children = tok->getChildren();
  string value = children[0]->getValue();

  // Relaxed type syntax (ex: (x : Vec Float) instead of (x : (Vec Float))
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

Binding Parser::parseBinding(const Token *tok) {
  PARSE_ENTER;

  PARSE_ASSERT(tok->size() == 2) << "Binding (v val), not [" << tok << "]";

  Expr::Ptr expr = parseToken(tok->getChild(1));

  if (tok->getChild(0)->isValue) {
    std::string value = tok->getChild(0)->getValue();
    Variable::Ptr var = std::make_unique<Variable>(value, expr->getType());
    variables.set(value, var.get());
    return Binding(std::move(var), std::move(expr));
  } else {
    // Tuple-unpacking let
    PARSE_ASSERT(expr->getType().isTuple());
    size_t tupleSize = expr->getType().getSubTypes().size();
    PARSE_ASSERT(tupleSize == tok->getChild(0)->getChildren().size()) << "Initializer is a " << tupleSize << "-tuple";
    std::vector<Variable::Ptr> unpackingVars(tupleSize);
    for (size_t ii = 0; ii != tupleSize; ++ii) {
      const Token* v = tok->getChild(0)->getChild(ii);
      PARSE_ASSERT(v->isValue);
      std::string value = v->getValue();
      unpackingVars[ii] = std::make_unique<Variable>(value, expr->getType().getSubType(ii));
      variables.set(value, unpackingVars[ii].get());
    }
    return Binding(std::move(unpackingVars), std::move(expr));
  }
}

// Variable declaration: (let (x 10) (add x 10))
Let::Ptr Parser::parseLet(const Token *tok) {
  PARSE_ENTER;

  PARSE_ASSERT(tok->size() == 3);
  auto binding = parseBinding(tok->getChild(1));
  auto body = parseToken(tok->getChild(2));
  return make_unique<Let>(move(binding), move(body));
}

// Declares a function (and add it to symbol table)
Declaration::Ptr Parser::parseEDef(const Token *tok) {
  PARSE_ENTER;

  PARSE_ASSERT(tok->size() == 4) << "edef should be (edef name resultType (argType))";
  const Token *name = tok->getChild(1);
  const Token *ty = tok->getChild(2);
  const Token *args = tok->getChild(3);

  auto type = parseType(ty);
  PARSE_ASSERT2(!type.isNone(), ty) << "Parsing edef [" << name << "]";

  PARSE_ASSERT2(!args->isValue, args) << "Parsing edef [" << name << "]";

  Type argType;
  // Vector and Tuples can be declared bare
  if (args->getChild(0)->isValue &&
      !Type::isScalar(Str2Type(args->getChild(0)->getValue())))
    argType = parseType(args);
  else {
    std::vector<Type> argTypes;
    for (auto &c : args->getChildren())
      argTypes.push_back(parseType(c.get()));
    argType = oneArgify(argTypes);
  }
  Signature sig{ parseStructuredName(name), std::move(argType) };

  auto decl = make_unique<Declaration>(sig, type);
  function_decls[sig] = decl.get();
  return decl;
}

// Defines a function (checks from|adds to symbol table)
// (def name type args expr)
Definition::Ptr Parser::parseDef(const Token *tok) {
  PARSE_ENTER;

  PARSE_ASSERT(tok->size() == 5) << "Expect def to have 4 parts (def name Type args body)";
  const Token *tok_name = tok->getChild(1);
  const Token *tok_type = tok->getChild(2);
  const Token *tok_args = tok->getChild(3);
  const Token *tok_body = tok->getChild(4);

  auto returnType = parseType(tok_type);
  PARSE_ASSERT2(!returnType.isNone(), tok_type) << "Unknown return type [" << tok_type << "]";

  vector<Variable::Ptr> arguments;
  // Single var: (v 2.3)
  if (tok_args->size() && tok_args->getChild(0)->isValue)
    arguments.push_back(parseVariableWithType(tok_args));
  // Many vars: ((a 1) (b 2))
  else
    for (auto &a : tok_args->getChildren())
      arguments.push_back(parseVariableWithType(a.get()));

  // Create declaration early, to allow recursion
  StructuredName name = parseStructuredName(tok_name);
  std::vector<Type> argTypes;
  for (auto &a : arguments) {
    argTypes.push_back(a->getType());
  }
  Signature sig{ name, oneArgify(argTypes) };
  auto declaration = make_unique<Declaration>(sig, returnType);
  function_decls[sig] = declaration.get();

  // Function body is a block, create one if single expr
  auto body = parseToken(tok_body);
  PARSE_ASSERT2(returnType == body->getType(), tok_body) << "Return type declared as [" << returnType << "], but body has type [" << body->getType() << "]";

  return make_unique<Definition>(std::move(declaration), std::move(arguments), std::move(body));
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

Lambda::Ptr Parser::parseLambda(const Token *tok) {
  PARSE_ASSERT(!tok->isValue);
  PARSE_ASSERT(tok->size() == 3);
  PARSE_ASSERT(tok->getChild(0)->isValue && tok->getChild(0)->getValue() == "lam");

  auto var = parseVariableWithType(tok->getChild(1));
  auto body = parseToken(tok->getChild(2));
  return std::make_unique<Lambda>(std::move(var), std::move(body));
}

// Loops, ex: (build N (lam (i : Integer) (add i i)))
Build::Ptr Parser::parseBuild(const Token *tok) {
  PARSE_ENTER;

  PARSE_ASSERT(tok->size() == 3);

  // Range (can be dynamic)
  auto range = parseToken(tok->getChild(1));

  // Lambda (induction variable and loop body)
  auto lam = parseLambda(tok->getChild(2));
  PARSE_ASSERT(lam->getVariable()->getType() == AST::Type::Integer);

  return make_unique<Build>(move(range), move(lam));
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
  auto lam = parseLambda(tok->getChild(1));
  auto varTy = lam->getVariable()->getType();
  PARSE_ASSERT(varTy == Type::Tuple);
  auto accTy = varTy.getSubType(0);
  auto elmTy = varTy.getSubType(1);
  PARSE_ASSERT(lam->getBody()->getType() == accTy);

  auto init = parseToken(tok->getChild(2));
  PARSE_ASSERT(init->getType() == accTy);

  auto vector = parseToken(tok->getChild(3));
  PARSE_ASSERT(vector->getType() == Type::Vector);
  PARSE_ASSERT(vector->getType().getSubType() == elmTy);

  return make_unique<Fold>(accTy, move(lam), move(init), move(vector));
}

// Rule: (rule "mul2" (v : Float) (mul v 2.0) (add v v))
Rule::Ptr Parser::parseRule(const Token *tok) {
  PARSE_ENTER;

  PARSE_ASSERT(tok->size() == 5);
  const Token *name = tok->getChild(1);
  PARSE_ASSERT(name->isValue);
  llvm::StringRef unquoted = unquote(name->getValue());
  auto var = parseVariableWithType(tok->getChild(2));
  auto pat = parseToken(tok->getChild(3));
  auto res = parseToken(tok->getChild(4));
  auto rule =
      make_unique<Rule>(unquoted, move(var), move(pat), move(res));
  rules.set(unquoted.str(), rule.get());
  return rule;
}

