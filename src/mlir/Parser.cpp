#include <iostream>
#include <map>
#include <set>
#include "Parser.h"

using namespace std;
using namespace Knossos::Parser;

//================================================ Symbol Table

// Symbol in the table (function, variable), not reserved
struct Symbol {
  enum class Type {
    Variable,
    Function,
    LAST = Function
  };
  Symbol(const string name, Type type) : name(name), type(type) { }
  void setExpr(Node::Expr e) {
    assert(!expr);
    expr = e;
  }
  const Node::Expr getExpr() const {
    assert(expr);
    return expr;
  }
  Node::Type getType() const {
    assert(expr);
    return expr->getType();
  }
  bool isVariable() const { return type == Type::Variable; }
  bool isFunction() const { return type == Type::Function; }

  bool operator< (const Symbol &o) const {
    return name < o.name;
  }

private:
  const string name;
  Type type;
  Node::Expr expr;
};

// Symbol Table: holds symbols for operations, functions and variables
//
// Overall structure:
//  * List of reserved words: language constructs, operations
//  * List of functions and variables declared (names can't clash)
//
// TODO:
//  - Add lexical context to variables (now they're all globals)
//  - Perform recursive search on parent contexts
class SymbolTable {
  const vector<const char*> ReservedOps {
    "add", "sub", "mul", "div" // TODO: add all
  };
  const vector<const char*> ReservedWords {
    "let", "edef", "def", "if" // TODO: add all
  };
  set<string> reservedOps;
  set<string> reservedWords;
  using ST = map<const string, Symbol>;
  ST declared;

  bool addSymbol(const string &name, Symbol::Type type, Node::Expr expr=nullptr) {
    if (reservedOps.find(name) != reservedOps.end())
      return false;
    if (reservedWords.find(name) != reservedWords.end())
      return false;
    const auto [it, success] = declared.insert({name, Symbol(name, type)});
    if (!success)
      return false;
    if (expr)
      it->second.setExpr(expr);
    return true;
  }

public:
  SymbolTable() {
    for (auto w: ReservedOps)
      reservedOps.insert(w);
    for (auto w: ReservedWords)
      reservedWords.insert(w);
  }
  void clear() {
    declared.clear();
  }

  void setExpr(const string &name, Node::Expr expr) {
    assert(expr != nullptr);
    auto it = declared.find(name);
    assert(it != declared.end());
    auto symbol = it->second;
    symbol.setExpr(expr);
  }

  bool addFunction(const string &name, Node::Expr expr=nullptr) {
    return addSymbol(name, Symbol::Type::Function, expr);
  }
  bool existsFunction(const string &name) const {
    auto fun = declared.find(name);
    return fun != declared.end() && fun->second.isFunction();
  }
  const Node::Expr getFunction(const string &name) const {
    auto fun = declared.find(name);
    if (fun == declared.end())
      return nullptr;
    assert(fun->second.isFunction());
    return fun->second.getExpr();
  }

  bool addVariable(const string &name, Node::Expr expr=nullptr) {
    return addSymbol(name, Symbol::Type::Variable, expr);
  }
  bool existsVariable(const string &name) const {
    auto fun = declared.find(name);
    return fun != declared.end() && fun->second.isVariable();
  }
  const Node::Expr getVariable(const string &name) const {
    auto var = declared.find(name);
    if (var == declared.end())
      return nullptr;
    assert(var->second.isVariable());
    return var->second.getExpr();
  }

  bool isReservedOp(const string &name) const {
    return reservedOps.find(name) != reservedOps.end();
  }
  bool isReservedWord(const string &name) const {
    return reservedWords.find(name) != reservedWords.end();
  }
  bool isReserved(const string &name) const {
    return isReservedOp(name) || isReservedWord(name);
  }
};
// FIXME: This should move inside a Context variable in the parser
SymbolTable symbols;

//================================================ Helpers

Node::Type Str2Type(const string &ty) {
  if (ty == "String")
    return Node::Type::String;
  if (ty == "Bool")
    return Node::Type::Bool;
  if (ty == "Integer")
    return Node::Type::Integer;
  if (ty == "Float")
    return Node::Type::Float;
  if (ty == "Tuple")
    return Node::Type::Tuple;
  if (ty == "Vec")
    return Node::Type::Vec;
  if (ty == "Lambda")
    return Node::Type::Lambda;
  if (ty == "LM")
    return Node::Type::LM;
  return Node::Type::None;
}

const string Type2Str(Node::Type type) {
  switch (type) {
    case Node::Type::None:
      return "none";
    case Node::Type::String:
      return "String";
    case Node::Type::Bool:
      return "Bool";
    case Node::Type::Integer:
      return "Integer";
    case Node::Type::Float:
      return "Float";
    case Node::Type::Tuple:
      return "Tuple";
    case Node::Type::Vec:
      return "Vec";
    case Node::Type::Lambda:
      return "Lambda";
    case Node::Type::LM:
      return "LM";
    default:
      assert(0 && "Invalid type");
  }
}

Node::Type LiteralType(const std::string &str) {
  // String
  if (str[0] == '"' && str[str.size()-1] == '"')
    return Node::Type::String;
  // Bool
  if (str == "true" || str == "false")
    return Node::Type::Bool;
  // Number
  bool isNumber = true;
  bool isFloat = false;
  for (auto c: str) {
    if (c == '.')
      isFloat = true;
    else if (!::isdigit(c)) {
      isNumber = false;
      break;
    }
  }
  if (isNumber) {
    if (isFloat)
      return Node::Type::Float;
    else
      return Node::Type::Integer;
  }

  // TODO: detect Tuple, Vec, Lambda, LM
  return Node::Type::None;
}

//================================================ Lex source into Tokens

// Lex a token out, recurse if another entry point is found
size_t Lexer::lexToken(Token *tok, size_t index) {
  size_t tokenStart = index;
  while (index < len) {
    switch (code[index]) {
    case ' ':
      // Maybe end of a value
      if (tokenStart != index) {
        tok->addChild(new Token(code.substr(tokenStart, index - tokenStart)));
      }
      // Or end of a token, which we ignore
      tokenStart = ++index;
      break;
    case ')':
      // Maybe end of a value
      if (tokenStart != index) {
        tok->addChild(new Token(code.substr(tokenStart, index - tokenStart)));
      }
      // Finished parsing this token
      return ++index;
    case '(': {
      // Recurse into sub-tokens
      Token *t = new Token();
      tokenStart = index = lexToken(t, index + 1);
      tok->addChild(t);
      break;
    }
    default: {
      // These are text, so we keep reading
      index++;
      break;
    }
    }
  }
  return index;
}

//================================================ Parse Tokens into Nodes

// Entry point, returns the root node
const Node::Expr Parser::parse() {
  // This is a global for now, so we clear every time
  symbols.clear();
  if (!root)
    root = parseToken(tok);
  return root;
}

// Creates an AST::Node for each token, validating
Node::Expr Parser::parseToken(const Token *tok) {
  // Values are all strings (numbers will be converted later)
  // They could be variable use, type names, literals
  if (tok->isValue())
    return parseValue(tok);

  // Empty block
  if (tok->numChildren() == 0)
    return shared_ptr<Node>(new Block());

  // First child is a value, can be all sorts of block types
  if (tok->getChild(0)->isValue()) {

    // Check first value for type
    const string &value = tok->getChild(0)->getValue();

    // Constructs: let, edef, def, if, fold, etc.
    if (symbols.isReservedWord(value)) {
      if (value == "let") {
        assert(tok->numChildren() == 3);
        return parseLet(tok->getChild(1), tok->getChild(2));
      } else if (value == "edef") {
        assert(tok->numChildren() == 4);
        return parseDecl(tok->getChild(1), tok->getChild(2),
                         tok->getChild(3));
      } else if (value == "def") {
        assert(tok->numChildren() == 5);
        return parseDef(tok->getChild(1), tok->getChild(2),
                        tok->getChild(3), tok->getChild(4));
      } else if (value == "if") {
        assert(tok->numChildren() == 4);
        return parseCond(tok->getChild(1), tok->getChild(2),
                        tok->getChild(3));
      }
      // TODO: implement fold, lambda, tuple, apply
    }

    // Operations: reserved keywords like add, mul, etc.
    if (symbols.isReservedOp(value))
      return parseOperation(tok);

    // Function call: (fun 10.0 42 "Hello")
    if (symbols.existsFunction(value))
      return parseOperation(tok);

    // Variable declaration: (name : Type) : add name to SymbolTable
    if (tok->numChildren() == 3 && tok->getChild(1)->getValue() == ":") {
      Node::Type type = Str2Type(tok->getChild(2)->getValue());
      assert(type != Node::Type::None);
      symbols.addVariable(value, make_shared<Node>(Type(tok->getChild(2)->getValue(), type)));
      return shared_ptr<Node>(new Variable(value, type));

    // Variable definition: (name value) : check name on SymbolTable
    } else if (tok->numChildren() == 2) {
      Node::Expr expr = parseToken(tok->getChild(1));
      if (symbols.existsVariable(value)) {
        assert(expr->getType() == symbols.getVariable(value)->getType());
        symbols.setExpr(value, expr);
      } else {
        symbols.addVariable(value, expr);
      }
      return shared_ptr<Node>(new Variable(value, expr));
    }

    // Fall through to a block if we don't recognise it
  }

  // If the first expr is not a value (is a block) or if none of the values
  // matched above, we treat them as "just a block" of expressions.
  Block* b = new Block();
  for (size_t i=0, e=tok->numChildren(); i<e; i++)
    b->addOp(parseToken(tok->getChild(i)));
  return shared_ptr<Node>(b);
}

// Values (variable names, literals, type names)
Node::Expr Parser::parseValue(const Token* tok) {
  assert(tok->isValue());
  const string &value = tok->getValue();

  // Type names: Integer, Float, Bool, String...
  Node::Type ty = Str2Type(value);
  if (ty != Node::Type::None) {
    return shared_ptr<Node>(new Type(value, ty));
  }
  // Literals: 10.0 42 "Hello" (not hello, that's a variable use)
  ty = LiteralType(value);
  if (ty != Node::Type::None) {
    return shared_ptr<Node>(new Literal(value, ty));
  }
  // Variable use: name (without quotes)
  assert(symbols.existsVariable(value) && "Variable not declared");
  ty = symbols.getVariable(value)->getType();
  return shared_ptr<Node>(new Variable(value, ty));
}

// Calls (fun arg1 arg2 ...)
// Checks types agains symbol table
Node::Expr Parser::parseCall(const Token* tok) {
  const string &value = tok->getChild(0)->getValue();
  assert(symbols.existsFunction(value));
  Declaration::Ptr decl =
      dynamic_pointer_cast<Declaration>(symbols.getFunction(value));
  assert(decl);

  vector<Node::Expr> operands;
  Node::Type type = decl->getType();
  Operation* o = new Operation(value, type);
  for (size_t i=1, e=tok->numChildren(); i<e; i++) {
    o->addOperand(parseToken(tok->getChild(i)));
    assert(o->getOperand(i-1)->getType() == decl->getOperandType(i-1));
  }
  return shared_ptr<Node>(o);
}

// Operations (op arg1 arg2 ...), retTy = argnTy
Node::Expr Parser::parseOperation(const Token* tok) {
  assert(!tok->isValue() && tok->numChildren() > 1);
  assert(tok->getChild(0)->isValue());
  const string &value = tok->getChild(0)->getValue();

  // Get all operands
  vector<Node::Expr> operands;
  Node::Type type = Node::Type::None;
  for (size_t i=1, e=tok->numChildren(); i<e; i++) {
    operands.push_back(parseToken(tok->getChild(i)));
    // Check types are all the same
    if (type == Node::Type::None)
      type = operands[i-1]->getType();
    else
      assert(type == operands[i-1]->getType());
  }
  // Create the op and return
  Operation* o = new Operation(value, type);
  for (auto op: operands)
    o->addOperand(op);
  return shared_ptr<Node>(o);
}

// Sub-expr with contextual variables: (let (x 10) (add x 10))
Node::Expr Parser::parseLet(const Token* bond, const Token* expr) {
  assert(!bond->isValue() && !expr->isValue());
  assert(bond->numChildren() == 2);
  // TODO: can we bind more than one variable?
  return make_shared<Let>(parseToken(bond), parseToken(expr));
}

// Declares a function (and add it to symbol table)
Node::Expr Parser::parseDecl(const Token* name, const Token* type, const Token* args) {
  assert(name->isValue() && type->isValue());
  assert(!args->isValue());
  auto node =
      make_shared<Declaration>(name->getValue(), Str2Type(type->getValue()));
  assert(node);
  assert(!args->isValue());
  for (size_t i = 0, e = args->numChildren(); i < e; i++)
    node->addOperandType(Str2Type(args->getChild(i)->getValue()));
  symbols.addFunction(name->getValue(), node);
  return node;
}

// Defines a function (checks from|adds to symbol table)
Node::Expr Parser::parseDef(const Token* name, const Token* type,
                            const Token* args, const Token* expr) {
  assert(name->isValue() && type->isValue());
  assert(!args->isValue() && !expr->isValue());
  vector<Node::Expr> arguments;
  for (size_t i=0, e=args->numChildren(); i<e; i++) {
    arguments.push_back(parseToken(args->getChild(i)));
    assert(arguments[i]->opTy == Node::OpTy::Variable);
  }

  // If declaration exists, compare types, else, adds to it
  bool hasDecl = symbols.existsFunction(name->getValue());
  if (hasDecl) {
    Declaration::Expr decl =
      dynamic_pointer_cast<Declaration>(symbols.getFunction(name->getValue()));
    for (auto a : arguments)
      assert(Str2Type(type->getValue()) == decl->getType());
  }

  auto node = make_shared<Definition>(
      name->getValue(), Str2Type(type->getValue()), parseToken(expr));
  assert(node);
  assert(!args->isValue());
  for (auto arg: arguments)
    node->addOperand(arg);

  // If missing decl, create one and add to the symbol table
  if (!hasDecl)
    symbols.addFunction(name->getValue(), node);
  return node;
}

// Conditional: (if (cond) (true block) (false block))
Node::Expr Parser::parseCond(const Token *cond, const Token *ifBlock,
                             const Token *elseBlock) {
  assert(!cond->isValue() && !ifBlock->isValue() && !elseBlock->isValue());
  return make_shared<Condition>(parseToken(cond), parseToken(ifBlock),
                                parseToken(elseBlock));
}

//================================================ Dumps tokens, nodes to stdout

void Token::dump(size_t tab) const {
  if (isValue()) {
    cout << string(tab, ' ') << "tok(" << getValue() << ")\n";
    return;
  }

  cout << string(tab, ' ') << "enter\n";
  tab += 2;
  for (auto t: children)
    t->dump(tab);
  tab -= 2;
  cout << string(tab, ' ') << "exit\n";
}

void Node::dump(size_t tab) const {
  cout << string(tab, ' ')
       << "name [" << name << "] "
       << "type [" << Type2Str(type) << "]"
       << endl;
}

void Block::dump(size_t tab) const {
  cout << string(tab, ' ') << "Block:" << endl;
  for (auto op: ops)
    op->dump(tab+2);
}

void Type::dump(size_t tab) const {
  cout << string(tab, ' ') << "Type:" << endl;
  Node::dump(tab+2);
}

void Literal::dump(size_t tab) const {
  cout << string(tab, ' ') << "Literal:" << endl;
  Node::dump(tab+2);
}

void Variable::dump(size_t tab) const {
  cout << string(tab, ' ') << "Variable:" << endl;
  Node::dump(tab+2);
  if (init)
    init->dump(tab+2);
}

void Let::dump(size_t tab) const {
  cout << string(tab, ' ') << "Let:" << endl;
  Node::dump(tab+2);
  var->dump(tab+2);
  expr->dump(tab+2);
}

void Operation::dump(size_t tab) const {
  cout << string(tab, ' ') << "Operation:" << endl;
  Node::dump(tab+2);
  for (auto op: ops)
    op->dump(tab+2);
}

void Declaration::dump(size_t tab) const {
  cout << string(tab, ' ') << "Declaration:" << endl;
  Node::dump(tab+2);
  cout << string(tab+2, ' ') << "Types: [ ";
  for (auto ty: operandTypes)
    cout << Type2Str(ty) << " ";
  cout << "]" << endl;
}

void Definition::dump(size_t tab) const {
  cout << string(tab, ' ') << "Definition:" << endl;
  Node::dump(tab+2);
  cout << string(tab+2, ' ') << "Arguments:" << endl;
  for (auto op: operands)
    op->dump(tab+4);
  cout << string(tab+2, ' ') << "Implementation:" << endl;
  impl->dump(tab+4);
}

void Condition::dump(size_t tab) const {
  cout << string(tab, ' ') << "Condition:" << endl;
  cond->dump(tab+2);
  cout << string(tab+2, ' ') << "True branch:" << endl;
  ifBlock->dump(tab+4);
  cout << string(tab+2, ' ') << "False branch:" << endl;
  elseBlock->dump(tab+4);
}
