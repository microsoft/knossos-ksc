/* Copyright Microsoft Corp. 2020 */
#include <iostream>

#include "Parser/Parser.h"

using namespace std;
using namespace Knossos::AST;

#define PARSE_ASSERT(p) ASSERT(p) << "\nAT: " << tok << std::endl << "FAILED: "

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


static const string Type2Str(Type::ValidType type) {
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

static size_t toIndex(llvm::StringRef str) {
  size_t idx = 0;
  bool failed = str.getAsInteger(0, idx);
  assert(!failed && "Bad index conversion");
  return idx;
}

static Type LiteralType(llvm::StringRef str) {
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

static bool isLiteralOrType(llvm::StringRef str) {
  return Str2Type(str) != Type::None ||
         LiteralType(str) != Type::None;
}

static llvm::StringRef unquote(llvm::StringRef original) {
  size_t start = 0, len = original.size();
  if (original.front() == '"') { start++; len--; }
  if (original.back() == '"') len--;
  return original.substr(start, len);
}

static Literal::Ptr getZero(Type type) {
  switch(type) {
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

//================================================ Lex source into Tokens

// Lex a token out, recurse if another entry point is found
size_t Lexer::lexToken(Token *tok, size_t pos) {
  const char TAB = 0x09;
  const char SPC = 0x20;
  size_t tokenStart = pos;
  bool isInString = false;
  while (pos < len) {
    switch (code[pos]) {
      case ';':
        // Comment, to the end of the line
        while (code[pos] != '\n')
          tokenStart = ++pos;
        break;
      case '#':
        if (code[pos+1] != '|')
          break;
        PARSE_ASSERT(multiLineComments == 0);
        pos += 2; // consume #|
        multiLineComments = 1;
        // Multi-line comment
        while (multiLineComments) {
          switch (code[pos]) {
            case '|':
              if (code[pos+1] == '#') {
                multiLineComments--;
                pos++;
              }
              pos++;
              break;
            case '#':
              if (code[pos+1] == '|') {
                multiLineComments++;
                pos++;
              }
              pos++;
              break;
            default:
              pos++;
          }
        }
        tokenStart = pos;
        break;
      case TAB:
      case SPC:
      case '\n':
      case '\r':
        // "Whitespace" is allowed inside strings
        if (isInString) {
          pos++;
          break;
        }
        // Maybe end of a value
        if (tokenStart != pos) {
          tok->addChild(
              make_unique<Token>(code.substr(tokenStart, pos - tokenStart)));
        }
        // Or end of a token, which we ignore
        tokenStart = ++pos;
        break;
      case ')':
        // Maybe end of a value
        if (tokenStart != pos) {
          tok->addChild(
              make_unique<Token>(code.substr(tokenStart, pos - tokenStart)));
        }
        // Finished parsing this token
        return ++pos;
      case '(': {
        // Recurse into sub-tokens
        auto t = make_unique<Token>();
        tokenStart = pos = lexToken(t.get(), pos + 1);
        tok->addChild(move(t));
        break;
      }
      case '"':
        if (isInString) {
          // Strings need to capture the quotes, too
          size_t start = tokenStart - 1;
          size_t length = (pos - start + 1);
          tok->addChild(
              make_unique<Token>(code.substr(start, length)));
        }
        tokenStart = ++pos;
        isInString = !isInString;
        break;
      default:
        // These are text, so we keep reading
        pos++;
    }
  }
  return pos;
}

//================================================ Parse Tokens into Exprs

// Creates an Expr for each token, validating
Expr::Ptr Parser::parseToken(const Token *tok) {
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
    std::cerr << "NON-VALUE HEAD!\n" << tok; 
    // Simple pass-through
    if (tok->size() == 1)
      return parseToken(head);
    // This is an actual block
    return parseBlock(tok);
  }

  // First child is a value, can be all sorts of block types
  // Check first value for type
  string value = tok->getHead()->getValue().str();

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
    case Parser::Keyword::SUM:
      return parseSum(tok);
    case Parser::Keyword::NA:
      // It's not a reserved keyword, try other constructs
      break;
  }

  // Everything else is a function call: (fun 10.0 42 "Hello")
  return parseCall(tok);
}

// Values (variable names, literals, type names)
Expr::Ptr Parser::parseBlock(const Token *tok) {
  Block *b = new Block();
  for (auto &c : tok->getChildren())
    b->addOperand(parseToken(c.get()));
  return unique_ptr<Expr>(b);
}

// Parses type declarations (vector, tuples)
Type Parser::parseType(const Token *tok) {
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
  PARSE_ASSERT(tok->isValue);
  string value = tok->getValue().str();

  // Literals: 10.0 42 "Hello" (not hello, that's a variable use)
  Type ty = LiteralType(value);
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
Expr::Ptr Parser::parseCall(const Token *tok) {
  string name = tok->getHead()->getValue().str();

  // Construct with no type, no operands
  Call *o = new Call(name, Type::None);

  // Add operands
  for (auto &c : tok->getTail())
    o->addOperand(parseToken(c.get()));

#define MATCH_1(NAME, ARGTYPE_0)\
     (o->size() == 1 && name == NAME &&\
      o->getOperand(0)->getType() == Type::ARGTYPE_0)

#define MATCH_2(NAME, ARGTYPE_0, ARGTYPE_1)\
     (o->size() == 2 && name == NAME &&\
      o->getOperand(0)->getType() == Type::ARGTYPE_0 &&\
      o->getOperand(1)->getType() == Type::ARGTYPE_1)

#define RETURN(TYPE) { o->setType(TYPE); return unique_ptr<Expr>(o); }

  // TODO: Dedup this with Generator::buildCall
  // TODO: Move all to prelude once polymorphic
  // TODO: expose the "if", just have the conditional
  if (MATCH_1("abs", Float))  RETURN(Type::Float);
  if (MATCH_1("neg", Float))  RETURN(Type::Float);
  if (MATCH_1("exp", Float))  RETURN(Type::Float);
  if (MATCH_1("log", Float))  RETURN(Type::Float);

  if (MATCH_1("to_float", Integer)) RETURN(Type::Float);
  if (MATCH_1("to_int", Float))     RETURN(Type::Float);

  if (MATCH_2("add", Integer, Integer))   RETURN(Type::Integer);
  if (MATCH_2("add", Float, Float))       RETURN(Type::Float);
  if (MATCH_2("sub", Integer, Integer))   RETURN(Type::Integer);
  if (MATCH_2("sub", Float, Float))       RETURN(Type::Float);
  if (MATCH_2("mul", Integer, Integer))   RETURN(Type::Integer);
  if (MATCH_2("mul", Float, Float))       RETURN(Type::Float);
  if (MATCH_2("div", Integer, Integer))   RETURN(Type::Integer);
  if (MATCH_2("div", Float, Float))       RETURN(Type::Float);

  if (MATCH_2("and", Bool, Bool))   RETURN(Type::Bool);
  if (MATCH_2("or", Bool, Bool))    RETURN(Type::Bool);

  // Comparison
  if (MATCH_2("eq", Integer, Integer))   RETURN(Type::Bool);
  if (MATCH_2("eq", Float, Float))       RETURN(Type::Bool);
  if (MATCH_2("ne", Integer, Integer))   RETURN(Type::Bool);
  if (MATCH_2("ne", Float, Float))       RETURN(Type::Bool);
  if (MATCH_2("lte", Integer, Integer))  RETURN(Type::Bool);
  if (MATCH_2("lte", Float, Float))      RETURN(Type::Bool);
  if (MATCH_2("gte", Integer, Integer))  RETURN(Type::Bool);
  if (MATCH_2("gte", Float, Float))      RETURN(Type::Bool);
  if (MATCH_2("gt", Integer, Integer))   RETURN(Type::Bool);
  if (MATCH_2("gt", Float, Float))       RETURN(Type::Bool);
  if (MATCH_2("lt", Integer, Integer))   RETURN(Type::Bool);
  if (MATCH_2("lt", Float, Float))       RETURN(Type::Bool);

  // Prims
  if (MATCH_1("size", Vector))               RETURN(Type::Integer);
  if (MATCH_2("index", Integer, Vector))     RETURN(o->getOperand(1)->getType().getSubType());

  if (name == "print") RETURN(Type::Integer); // Any number of args, any type...

#undef MATCH_1
#undef MATCH_2
#undef RETURN

  // Fall through to non-prims
  PARSE_ASSERT(functions.exists(name)) << "Unrecognized function: " << name;
  
  Declaration* decl = llvm::dyn_cast<Declaration>(functions.get(name));
  PARSE_ASSERT(decl) << "Function [" << name << "] found but not declared?";

  o->setType(decl->getType());

  // Validate types
  for (const auto &it : llvm::zip(o->getOperands(), decl->getArgTypes()))
    PARSE_ASSERT(get<0>(it)->getType() == get<1>(it)) 
                << "Type mismatch at " << tok << "\n" 
                << get<0>(it)->getType() << " != " << get<1>(it);

  return unique_ptr<Expr>(o);
}


// Variables can be declarations or definitions, depending on the arguments
// (def f Type ((v : Type)) body)  ; declaration
//              ^^^^^^^^^^
// ... (let ((v value)) body) ...  ; definition
//           ^^^^^^^^^
Expr::Ptr Parser::parseVariable(const Token *tok) {
  PARSE_ASSERT(tok->size() > 1);
  llvm::ArrayRef<Token::Ptr> children = tok->getChildren();
  string value = children[0]->getValue().str();

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
    PARSE_ASSERT(type != Type::None);
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
Expr::Ptr Parser::parseLet(const Token *tok) {
  PARSE_ASSERT(tok->size() == 3);
  const Token *bond = tok->getChild(1);
  PARSE_ASSERT(!bond->isValue);
  vector<Expr::Ptr> vars;
  // Single variable binding
  if (bond->getChild(0)->isValue) {
    PARSE_ASSERT(bond->size() == 2);
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
Expr::Ptr Parser::parseDecl(const Token *tok) {
  PARSE_ASSERT(tok->size() == 4) << "Decl should be (edef name type args)";
  const Token *name = tok->getChild(1);
  const Token *ty = tok->getChild(2);
  const Token *args = tok->getChild(3);
  PARSE_ASSERT(name->isValue) << "Decl should be (edef name type args)";
  auto type = parseType(ty);
  PARSE_ASSERT(type != Type::None) << "Parsing decl [" << name << "]";
  PARSE_ASSERT(!args->isValue) << "Parsing decl [" << name << "]";

  auto decl =
      make_unique<Declaration>(name->getValue(), type);
  PARSE_ASSERT(decl);

  // Vector and Tuples can be declared bare
  if (args->getChild(0)->isValue &&
      !Type::isScalar(Str2Type(args->getChild(0)->getValue())))
    decl->addArgType(parseType(args));
  else
    for (auto &c : args->getChildren())
      decl->addArgType(parseType(c.get()));

  functions.set(name->getValue().str(), decl.get());
  return decl;
}

// Defines a function (checks from|adds to symbol table)
// (def name type args expr)
Expr::Ptr Parser::parseDef(const Token *tok) {
  PARSE_ASSERT(tok->size() == 5);
  const Token *name = tok->getChild(1);
  const Token *type = tok->getChild(2);
  const Token *args = tok->getChild(3);
  const Token *expr = tok->getChild(4);
  PARSE_ASSERT(name->isValue && !args->isValue);
  vector<Expr::Ptr> arguments;
  // If there is only one child
  if (args->size() && args->getChild(0)->isValue)
    arguments.push_back(parseVariable(args));
  // Or if there are many
  else
    for (auto &a : args->getChildren())
      arguments.push_back(parseVariable(a.get()));

  // Create node early, to allow recursion
  auto node = make_unique<Definition>(name->getValue(),
                                      parseType(type));
  for (auto &a : arguments) {
    PARSE_ASSERT(a->kind == Expr::Kind::Variable);
    node->addArgument(move(a));
  }
  functions.set(name->getValue().str(), node->getDeclaration());

  // Function body is a block, create one if single expr
  auto body = parseToken(expr);
  node->setImpl(move(body));

  return node;
}

// Conditional: (if (cond) (true block) (false block))
Expr::Ptr Parser::parseCond(const Token *tok) {
  PARSE_ASSERT(tok->size() == 4);
  auto c = parseToken(tok->getChild(1));
  auto i = parseToken(tok->getChild(2));
  auto e = parseToken(tok->getChild(3));
  return make_unique<Condition>(move(c), move(i), move(e));
}

// Loops, ex: (build N (lam (i : Integer) (add i i)))
Expr::Ptr Parser::parseBuild(const Token *tok) {
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
  llvm::dyn_cast<Variable>(var.get())->setInit(getZero(Type::Integer));
  auto body = parseToken(expr);
  return make_unique<Build>(move(range), move(var), move(body));
}

// Tuple, ex: (tuple 10.0 42 (add 1.0 2.0))
Expr::Ptr Parser::parseTuple(const Token *tok) {
  PARSE_ASSERT(tok->size() > 2);
  PARSE_ASSERT(tok->getChild(0)->isValue);
  std::vector<Expr::Ptr> elements;
  for (auto &c: tok->getTail())
    elements.push_back(parseToken(c.get()));
  return make_unique<Tuple>(move(elements));
}

// Index, ex: (get$7$9 tuple)
Expr::Ptr Parser::parseGet(const Token *tok) {
  PARSE_ASSERT(tok->size() == 2);
  PARSE_ASSERT(tok->getChild(0)->isValue);
  llvm::StringRef get = tok->getChild(0)->getValue();
  size_t dollar1 = get.find('$');
  size_t dollar2 = get.find('$', dollar1+1);
  llvm::StringRef indexStr = get.substr(dollar1+1, dollar2-dollar1-1);
  llvm::StringRef maxStr = get.substr(dollar2+1);
  size_t idx = toIndex(indexStr);
  size_t max = toIndex(maxStr);
  auto var = parseToken(tok->getChild(1));
  return make_unique<Get>(idx, max, move(var));
}

// Fold, ex: (fold (lambda) init vector)
Expr::Ptr Parser::parseFold(const Token *tok) {
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
Expr::Ptr Parser::parseRule(const Token *tok) {
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

//================================================ Standard library

// This is a hack, given that some polymorphic methods are not in prelude.ks
// but C++ headers (knossos.h) and we can't replicate it here. So we lower those
// special cases here as simple as we can, and hope one day they'll move out of
// the headers.

// Vector Reduction Sum, ex: (fold (add) 0.0 vector)
Expr::Ptr Parser::parseSum(const Token *tok) {
  PARSE_ASSERT(tok->size() == 2);
  // Vector
  auto vec = parseToken(tok->getChild(1));
  PARSE_ASSERT(vec->getType() == Type::Vector);
  auto elmTy = vec->getType().getSubType();
  // Accumulator & element (same type)
  auto type = Type(Type::Tuple, vector<Type>{ elmTy, elmTy });
  auto var = unique_ptr<Variable>(new Variable("acc_x", type));
  vector<Expr::Ptr> initArgs;
  initArgs.push_back(getZero(elmTy));
  initArgs.push_back(getZero(elmTy));
  auto init = make_unique<Tuple>(move(initArgs));
  llvm::dyn_cast<Variable>(var.get())->setInit(move(init));
  // Add Lambda: (add (get$1$2 acc_x) (get$2$2 acc_x))
  auto add = make_unique<Call>("add", elmTy);
  add->addOperand(make_unique<Get>(1, 2, make_unique<Variable>("acc_x", type)));
  add->addOperand(make_unique<Get>(2, 2, make_unique<Variable>("acc_x", type)));
  // Return fold
  return make_unique<Fold>(elmTy, move(add), move(var), move(vec));
}

//================================================ Dumps tokens, nodes to stdout

Token::ppresult Token::pprint(Token const* tok, int indent, int width)
{
  const int tab = 2;

  if (tok->isValue)
    return {tok->getValue().data(), tok->getValue().size()};

  int mywidth = 1; // for parens
  int maxwidth = 0;
  bool first = true;
  std::vector<std::string> strs;
  for (auto& t : tok->children) {
    ppresult p = pprint(t.get(), indent+tab, width);
    if (p.width > maxwidth)
      maxwidth = p.width;
    mywidth += (first ? 0 : 1) + p.width;
    strs.push_back(p.s);
    first = false;
  }

  int available_width = width - indent;
  ppresult ret;
  std::string sep;
  if (mywidth < available_width) {
    sep = " ";
    ret.width = mywidth;
  } else {
    sep = "\n" + std::string(indent, ' ');
    ret.width = maxwidth;
  }

  first = true;
  ret.s = "(";
  for (auto &s : strs) {
    if (!first) 
      ret.s += sep;
    ret.s += s;
    first = false;
  }
  ret.s += ")";

  return ret;  
}

std::ostream& Token::dump(std::ostream& s) const {
  return s << pprint(this, 0, 80).s << std::endl;
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
