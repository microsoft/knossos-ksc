/* Copyright Microsoft Corp. 2020 */
#include <iostream>

#include "Parser/Parser.h"

using namespace std;
using namespace Knossos::AST;

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
      assert(0 && "Invalid zero type");
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
        assert(multiLineComments == 0);
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
  if (tok->isValue)
    return parseValue(tok);

  // Empty block
  if (tok->size() == 0)
    return unique_ptr<Expr>(new Block());

  // If the first expr is not a value, this is a block of blocks
  auto head = tok->getHead();
  if (!head->isValue) {
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
    case Parser::Keyword::LET:
      return parseLet(tok);
    case Parser::Keyword::EDEF:
      return parseDecl(tok);
    case Parser::Keyword::DEF:
      return parseDef(tok);
    case Parser::Keyword::IF:
      return parseCond(tok);
    case Parser::Keyword::BUILD:
      return parseBuild(tok);
    case Parser::Keyword::INDEX:
      return parseIndex(tok);
    case Parser::Keyword::SIZE:
      return parseSize(tok);
    case Parser::Keyword::TUPLE:
      return parseTuple(tok);
    case Parser::Keyword::GET:
      return parseGet(tok);
    case Parser::Keyword::FOLD:
      return parseFold(tok);
    case Parser::Keyword::PRINT:
      return parsePrint(tok);
    case Parser::Keyword::RULE:
      return parseRule(tok);
    case Parser::Keyword::SUM:
      return parseSum(tok);
    case Parser::Keyword::NA:
      // It's not a reserved keyword, try other constructs
      break;
  }

  // Function call: (fun 10.0 42 "Hello")
  // FIXME: Check signature, too
  if (functions.exists(value))
    return parseCall(tok);

  // Operations: reserved keywords like add, mul, etc.
  Operation::Opcode op = isReservedOp(value);
  if (op != Operation::Opcode::MAYBE_CALL)
    return parseOperation(tok, op);

  // Variable declaration/definition
  if (!isLiteralOrType(value))
    return parseVariable(tok);

  // Nothing recognised, recurse
  return parseBlock(tok);
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
  if (tok->isValue)
    return Type(Str2Type(tok->getValue()));

  // Empty type
  if (tok->size() == 0)
    return Type::None;

  // Vector or Tuple (recursive)
  auto type = Str2Type(tok->getChild(0)->getValue());
  if (type == AST::Type::Vector) {
    auto element = tok->getChild(1);
    if (element->isValue) // Vector of scalar
      return Type(type, Str2Type(element->getValue()));
    else                  // Vector of vector
      return Type(type, parseType(element));
  }
  if (type == AST::Type::Tuple) {
    std::vector<Type> types;
    for (auto &c: tok->getTail()) {
      if (c->isValue)     // Scalar
        types.push_back(Str2Type(c->getValue()));
      else                // Tuple or Vector
        types.push_back(parseType(c.get()));
    }
    return Type(type, move(types));
  }
  assert(0 && "Invalid type");
}

// Parses relaxed type declarations (vector, tuples)
Type Parser::parseRelaxedType(vector<const Token *> toks) {
  assert(toks[0]->isValue && "Invalid relaxed type syntax");
  auto ty = toks[0]->getValue();
  if (ty == "Vec") {
    assert(toks.size() == 2 && "Invalid relaxed vector syntax");
    return Type(Type::Vector, parseType(toks[1]));
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
  assert(tok->isValue);
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
  assert(variables.exists(value) && "Variable not declared");
  auto val = variables.get(value);
  // Create new node, referencing existing variable
  assert(Variable::classof(val));
  return unique_ptr<Expr>(new Variable(value, val->getType()));
}

// Calls (fun arg1 arg2 ...)
// Checks types agains symbol table
Expr::Ptr Parser::parseCall(const Token *tok) {
  string name = tok->getHead()->getValue().str();
  assert(functions.exists(name));
  Declaration* decl = llvm::dyn_cast<Declaration>(functions.get(name));
  assert(decl);

  vector<Expr> operands;
  Type type = decl->getType();
  Operation *o = new Operation(name, Operation::Opcode::MAYBE_CALL, type);

  // Function without argument
  if (tok->size() == 1)
    return unique_ptr<Expr>(o);

  // Argument handling
  for (auto &c : tok->getTail())
    o->addOperand(parseToken(c.get()));
  // Validate types
  for (const auto &it : llvm::zip(o->getOperands(), decl->getArgTypes()))
    assert(get<0>(it)->getType() == get<1>(it));
  assert(o->getType() == decl->getType());
  return unique_ptr<Expr>(o);
}

// Operations (op arg1 arg2 ...), retTy = argnTy
Expr::Ptr Parser::parseOperation(const Token *tok, Operation::Opcode op) {
  assert(!tok->isValue && tok->size() > 1);
  assert(tok->getHead()->isValue);
  llvm::StringRef name = tok->getHead()->getValue();

  Operation *o = new Operation(name, op);
  for (auto &c : tok->getTail())
    o->addOperand(parseToken(c.get()));
  // Infer types
  o->inferTypes();
  return unique_ptr<Expr>(o);
}

// Variables can be declarations, definitions or use, depending on the arguments
Expr::Ptr Parser::parseVariable(const Token *tok) {
  assert(tok->size() > 1);
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
    assert(type != Type::None);
    auto var = unique_ptr<Variable>(new Variable(value, type));
    variables.set(value, var.get());
    return var;
  }

  // Variable definition: (name value) : check name on SymbolTable
  if (tok->size() == 2) {
    // Add to map fist, to allow recursion
    auto var = unique_ptr<Variable>(new Variable(value));
    Expr::Ptr expr = parseToken(children[1].get());
    var->setInit(move(expr));
    variables.set(value, var.get());
    return var;
  }

  assert(0 && "Invalid variable declaration/definition");
}

// Variable declaration: (let (x 10) (add x 10))
Expr::Ptr Parser::parseLet(const Token *tok) {
  assert(tok->size() == 3);
  const Token *bond = tok->getChild(1);
  assert(!bond->isValue);
  vector<Expr::Ptr> vars;
  // Single variable binding
  if (bond->getChild(0)->isValue) {
    assert(bond->size() == 2);
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
  assert(tok->size() == 4);
  const Token *name = tok->getChild(1);
  const Token *ty = tok->getChild(2);
  const Token *args = tok->getChild(3);
  assert(name->isValue);
  auto type = parseType(ty);
  assert(!args->isValue);

  auto decl =
      make_unique<Declaration>(name->getValue(), type);
  assert(decl);
  assert(!args->isValue);
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
Expr::Ptr Parser::parseDef(const Token *tok) {
  assert(tok->size() == 5);
  const Token *name = tok->getChild(1);
  const Token *type = tok->getChild(2);
  const Token *args = tok->getChild(3);
  const Token *expr = tok->getChild(4);
  assert(name->isValue && !args->isValue);
  vector<Expr::Ptr> arguments;
  // If there is only one child
  if (args->size() && args->getChild(0)->isValue)
    arguments.push_back(parseToken(args));
  // Or if there are many
  else
    for (auto &a : args->getChildren())
      arguments.push_back(parseToken(a.get()));

  // Creeate node early, to allow recursion
  auto node = make_unique<Definition>(name->getValue(),
                                      parseType(type));
  for (auto &a : arguments) {
    assert(a->kind == Expr::Kind::Variable);
    node->addArgument(move(a));
  }
  functions.set(name->getValue().str(), node->getProto());

  // Function body is a block, create one if single expr
  auto body = parseToken(expr);
  node->setImpl(move(body));

  return node;
}

// Conditional: (if (cond) (true block) (false block))
Expr::Ptr Parser::parseCond(const Token *tok) {
  assert(tok->size() == 4);
  auto c = parseToken(tok->getChild(1));
  auto i = parseToken(tok->getChild(2));
  auto e = parseToken(tok->getChild(3));
  return make_unique<Condition>(move(c), move(i), move(e));
}

// Loops, ex: (build N (lam (i : Integer) (add@ii i i)))
Expr::Ptr Parser::parseBuild(const Token *tok) {
  assert(tok->size() == 3);

  // Range (can be dynamic)
  auto range = parseToken(tok->getChild(1));

  // Lambda (induction variable and loop body)
  const Token *lam = tok->getChild(2);
  assert(!lam->isValue);
  assert(lam->getChild(0)->isValue && lam->getChild(0)->getValue() == "lam");
  const Token *bond = lam->getChild(1);
  const Token *expr = lam->getChild(2);
  auto var = parseVariable(bond);
  assert(var->kind == Expr::Kind::Variable);
  assert(var->getType() == AST::Type::Integer);
  llvm::dyn_cast<Variable>(var.get())->setInit(getZero(Type::Integer));
  auto body = parseToken(expr);
  return make_unique<Build>(move(range), move(var), move(body));
}

// Index, ex: (index N vector)
Expr::Ptr Parser::parseIndex(const Token *tok) {
  assert(tok->size() == 3);
  auto index = parseToken(tok->getChild(1));
  auto vector = parseToken(tok->getChild(2));
  return make_unique<Index>(move(index), move(vector));
}

// Size, ex: (size vector)
Expr::Ptr Parser::parseSize(const Token *tok) {
  assert(tok->size() == 2);
  auto vector = parseToken(tok->getChild(1));
  return make_unique<Size>(move(vector));
}

// Tuple, ex: (tuple 10.0 42 (add@ff 1.0 2.0))
Expr::Ptr Parser::parseTuple(const Token *tok) {
  assert(tok->size() > 2);
  assert(tok->getChild(0)->isValue);
  std::vector<Expr::Ptr> elements;
  for (auto &c: tok->getTail())
    elements.push_back(parseToken(c.get()));
  return make_unique<Tuple>(move(elements));
}

// Index, ex: (get$7$9 tuple)
Expr::Ptr Parser::parseGet(const Token *tok) {
  assert(tok->size() == 2);
  assert(tok->getChild(0)->isValue);
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
  assert(tok->size() == 4);
  // Lambda format: (lam (acc_x : (Tuple AccTy ElmTy)) (expr))
  const Token *lam = tok->getChild(1);
  assert(!lam->isValue);
  assert(lam->getChild(0)->isValue && lam->getChild(0)->getValue() == "lam");
  auto var = parseVariable(lam->getChild(1));
  assert(var->kind == Expr::Kind::Variable);
  assert(var->getType() == Type::Tuple);
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
  assert(body->getType() == accTy);
  auto vector = parseToken(tok->getChild(3));
  assert(vector->getType() == Type::Vector);
  assert(vector->getType().getSubType() == elmTy);
  return make_unique<Fold>(accTy, move(body), move(var), move(vector));
}

// Print: ex (print foo bar baz)
Expr::Ptr Parser::parsePrint(const Token *tok) {
  // Empty print returns nothing
  if (tok->size() == 0)
    return make_unique<Block>();
  auto print = make_unique<Print>();
  for (auto &t: tok->getTail())
    print->addExpr(parseToken(t.get()));
  return print;
}

// Rule: (rule "mul2" (v : Float) (mul@ff v 2.0) (add v v))
Expr::Ptr Parser::parseRule(const Token *tok) {
  assert(tok->size() == 5);
  const Token *name = tok->getChild(1);
  assert(name->isValue);
  llvm::StringRef unquoted = unquote(name->getValue());
  auto var = parseToken(tok->getChild(2));
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
  assert(tok->size() == 2);
  // Vector
  auto vec = parseToken(tok->getChild(1));
  assert(vec->getType() == Type::Vector);
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
  auto add = make_unique<Operation>("add", Operation::Opcode::ADD);
  add->addOperand(make_unique<Get>(1, 2, make_unique<Variable>("acc_x", type)));
  add->addOperand(make_unique<Get>(2, 2, make_unique<Variable>("acc_x", type)));
  add->inferTypes();
  // Return fold
  return make_unique<Fold>(elmTy, move(add), move(var), move(vec));
}

//================================================ Dumps tokens, nodes to stdout

void Token::dump(size_t tab) const {
  if (isValue) {
    cout << string(tab, ' ') << "tok(" << getValue().data() << ")\n";
    return;
  }

  cout << string(tab, ' ') << "enter\n";
  tab += 2;
  for (auto &t : children)
    t->dump(tab);
  tab -= 2;
  cout << string(tab, ' ') << "exit\n";
}

void Type::dump() const {
  if (type == Vector) {
    cout << "Vector( ";
    subTypes[0].dump();
    cout << " )";
  } else if (type == Tuple) {
    cout << "Tuple{ ";
    for (auto &t: subTypes) {
      t.dump();
      cout << " ";
    }
    cout << "}";
  } else {
    cout << Type2Str(type);
  }
}

void Expr::dump(size_t tab) const {
  cout << string(tab, ' ') << "type [";
  type.dump();
  cout << "]" << endl;
}

void Block::dump(size_t tab) const {
  cout << string(tab, ' ') << "Block:" << endl;
  for (auto &op : operands)
    op->dump(tab + 2);
}

void Literal::dump(size_t tab) const {
  cout << string(tab, ' ') << "Literal:" << endl;
  cout << string(tab + 2, ' ') << "value [" << value << "]" << endl;
  Expr::dump(tab + 2);
}

void Variable::dump(size_t tab) const {
  cout << string(tab, ' ') << "Variable:" << endl;
  cout << string(tab + 2, ' ') << "name [" << name << "]" << endl;
  Expr::dump(tab + 2);
  if (init)
    init->dump(tab + 2);
}

void Let::dump(size_t tab) const {
  cout << string(tab, ' ') << "Let:" << endl;
  Expr::dump(tab + 2);
  for (auto &v: vars)
    v->dump(tab + 2);
  if (expr)
    expr->dump(tab + 2);
}

void Operation::dump(size_t tab) const {
  cout << string(tab, ' ') << "Operation:" << endl;
  cout << string(tab + 2, ' ') << "name [" << name << "]" << endl;
  Expr::dump(tab + 2);
  for (auto &op : operands)
    op->dump(tab + 2);
}

void Declaration::dump(size_t tab) const {
  cout << string(tab, ' ') << "Declaration:" << endl;
  cout << string(tab + 2, ' ') << "name [" << name << "]" << endl;
  Expr::dump(tab + 2);
  cout << string(tab + 2, ' ') << "Types: [ ";
  for (auto ty : argTypes) {
    ty.dump();
    cout << " ";
  }
  cout << "]" << endl;
}

void Definition::dump(size_t tab) const {
  cout << string(tab, ' ') << "Definition:" << endl;
  cout << string(tab + 2, ' ') << "name [" << proto->getName().data() << "]"
       << endl;
  Expr::dump(tab + 2);
  cout << string(tab + 2, ' ') << "Arguments:" << endl;
  for (auto &op : arguments)
    op->dump(tab + 4);
  cout << string(tab + 2, ' ') << "Implementation:" << endl;
  impl->dump(tab + 4);
}

void Condition::dump(size_t tab) const {
  cout << string(tab, ' ') << "Condition:" << endl;
  cond->dump(tab + 2);
  cout << string(tab + 2, ' ') << "True branch:" << endl;
  ifBlock->dump(tab + 4);
  cout << string(tab + 2, ' ') << "False branch:" << endl;
  elseBlock->dump(tab + 4);
}

void Build::dump(size_t tab) const {
  cout << string(tab, ' ') << "Build:" << endl;
  Expr::dump(tab + 2);
  cout << string(tab + 2, ' ') << "Range:" << endl;
  range->dump(tab + 4);
  cout << string(tab + 2, ' ') << "Induction:" << endl;
  var->dump(tab + 4);
  cout << string(tab + 2, ' ') << "Body:" << endl;
  expr->dump(tab + 4);
}

void Tuple::dump(size_t tab) const {
  cout << string(tab, ' ') << "Tuple:" << endl;
  Expr::dump(tab + 2);
  cout << string(tab + 2, ' ') << "Values:" << endl;
  for (auto &el: elements)
    el->dump(tab + 4);
}

void Index::dump(size_t tab) const {
  cout << string(tab, ' ') << "Index:" << endl;
  Expr::dump(tab + 2);
  cout << string(tab + 2, ' ') << "Value:" << endl;
  index->dump(tab + 2);
  cout << string(tab + 2, ' ') << "Vector:" << endl;
  var->dump(tab + 2);
}

void Size::dump(size_t tab) const {
  cout << string(tab, ' ') << "Size:" << endl;
  Expr::dump(tab + 2);
  cout << string(tab + 2, ' ') << "Vector:" << endl;
  var->dump(tab + 2);
}

void Get::dump(size_t tab) const {
  cout << string(tab, ' ') << "Get:" << endl;
  Expr::dump(tab + 2);
  cout << string(tab + 2, ' ') << "index [" << index << "]" << endl;
  cout << string(tab + 2, ' ') << "From:" << endl;
  expr->dump(tab + 4);
}

void Fold::dump(size_t tab) const {
  cout << string(tab, ' ') << "Fold:" << endl;
  Expr::dump(tab + 2);
  cout << string(tab + 2, ' ') << "Lambda:" << endl;
  body->dump(tab + 4);
  cout << string(tab + 2, ' ') << "Accumulator:" << endl;
  acc->dump(tab + 4);
  cout << string(tab + 2, ' ') << "Vector:" << endl;
  vector->dump(tab + 4);
}

void Print::dump(size_t tab) const {
  cout << string(tab, ' ') << "Print:" << endl;
  Expr::dump(tab + 2);
  for (auto &el: exprs)
    el->dump(tab + 4);
}

void Rule::dump(size_t tab) const {
  cout << string(tab, ' ') << "Rule:" << endl;
  cout << string(tab + 2, ' ') << "name [" << name << "]"
       << endl;
  Expr::dump(tab + 2);
  cout << string(tab + 2, ' ') << "Variable:" << endl;
  variable->dump(tab + 4);
  cout << string(tab + 2, ' ') << "Pattern:" << endl;
  pattern->dump(tab + 4);
  cout << string(tab + 2, ' ') << "Result:" << endl;
  result->dump(tab + 4);
}
