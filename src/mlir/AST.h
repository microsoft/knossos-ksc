/* Copyright Microsoft Corp. 2020 */
#ifndef _AST_H_
#define _AST_H_

#include <vector>
#include <string>
#include <memory>
#include <cassert>

namespace Knossos {
namespace Parser {

// A node in the AST.
//
// These decisions were taken before implementation, so could be completely
// wrong. All restricting choices (const, immutable, etc) have been taken to
// simplify the parser.
struct Node {
  // Basic infrastructure
  using Expr = std::shared_ptr<Node>;
  virtual ~Node() { }
  Expr parent;

  // Node type, for quick checking in switch/cases
  enum class OpTy {
    Invalid,
    Block,
    Type,
    Literal,
    Variable,
    Let,
    Declaration,
    Definition,
    Condition,
    Operation,
    // Unused below (TODO: Implement those)
    Fold,
    Lambda,
    Tuple,
    Apply,
    Assert
  };

  // Type checking occurs from bottom up. Each node is responsible for its own
  // checks and, if valid, their return type can be used for the parents.
  // Function and variable types are checked by the symbol table
  enum class Type {
    None,
    String,
    Bool,
    Integer,
    Float,
    LAST = Float,
    // Unused below (TODO: Implement those)
    Tuple,
    Vec,
    Lambda,
    LM
  };

  // valid types, for safety checks
  bool isValidType() const {
    return type > Type::None && type <= Type::LAST;
  }
  // base types are "scalar"
  bool isScalar() const {
    switch (type) {
      case Type::String:
      case Type::Bool:
      case Type::Integer:
      case Type::Float:
        return true;
      default:
        return false;
    }
  }
  // tuples, vectors are "composite"
  bool isComposite() const {
    switch (type) {
      case Type::Tuple:
      case Type::Vec:
        return true;
      default:
        return false;
    }
  }
  const std::string &getName() const {
    return name;
  }
  Type getType() const {
    return type;
  }
  virtual void dump(size_t tab=0) const;

  // Type of the node, for quick access
  const OpTy opTy;

protected:
  Node() = delete;
  Node(const char* name, Type type, OpTy opTy)
    : name(name), type(type), opTy(opTy) { }
  Node(const std::string &name, Type type, OpTy opTy)
    : name(name), type(type), opTy(opTy) { }

  // Name of the variable, function, operation
  // FIXME: We may need to deduplicate the names like SSA does, with an auto
  // increment map of existing names, as well as using a context to prepend
  // the scope name (function, block, etc).
  std::string name;
  // Type it returns, for type checking
  Type type;
};

// Block node has nothing but children
// Use the current state
struct Block : public Node {
  using Ptr = std::shared_ptr<Block>;
  Block() : Node("block", Node::Type::None, Node::OpTy::Block) { }

  void addOp(Node::Expr node) {
    ops.push_back(node);
  }
  const Node::Expr getOp(size_t i) const {
    assert(i < ops.size() && "Offset error");
    return ops[i];
  }

  void dump(size_t tab) const override;

private:
  std::vector<Expr> ops;
};

// Types, ex: Float, String, Bool
//
// These are constant immutable objects. If no type match,
// the type remains None and this is not a type.
//
// Simplify parser a lot.
struct Type : public Node {
  using Ptr = std::shared_ptr<Type>;
  Type(const std::string &name, Node::Type type)
    : Node(name, type, Node::OpTy::Type) { }

  void dump(size_t tab) const override;
};

// Literals, ex: "Hello", 10.0, 123, false
//
// These are constant immutable objects.
// Type is determined by the parser.
struct Literal : public Node {
  using Ptr = std::shared_ptr<Literal>;
  Literal(const std::string &value, Node::Type type)
    : Node(value, type, Node::OpTy::Literal) {
    assert(isValidType() && "Invalid type");
  }

  const std::string &getValue() const {
    return name;
  }

  void dump(size_t tab) const override;
};

// Named variables, ex: (let (str "Hello") (print str)), (def a (x : Float) x)
//
// Variables have a contextual name (scope::name) and an initialisation
// expression. If nested, all types must match. Cannot be reassigned.
struct Variable : public Node {
  using Ptr = std::shared_ptr<Variable>;
  // Definition: (x 10) in ( let (x 10) (expr) )
  Variable(const std::string &name, Node::Expr init)
      : Node(name, init->getType(), Node::OpTy::Variable), init(init) { }
  // Declaration: (x : Integer) in ( def name Type (x : Integer) (expr) )
  Variable(const std::string &name, Node::Type type)
      : Node(name, type, Node::OpTy::Variable) { }
  // Use: x in ( add x 10 )
  Variable(const std::string &name)
      : Node(name, Node::Type::None, Node::OpTy::Variable) { }

  void addValue(Node::Expr node) {
    assert(!init && "Variables can only have one initialiser");
    assert(type == node->getType() && "pe mismatch");
    init = node;
  }
  const Node::Expr getValue() const {
    assert(init && "Variables without initialiser");
    return init;
  }

  void dump(size_t tab) const override;

private:
  Node::Expr init;
};

// Lexical bloc, ex: (let (x 10) (add x 10))
//
// Defines a variable to be used insode the scope
struct Let : public Node {
  using Ptr = std::shared_ptr<Let>;
  Let(Node::Expr var, Node::Expr expr)
      : Node("let", expr->getType(), Node::OpTy::Let), var(var), expr(expr) {
    assert(std::dynamic_pointer_cast<Variable>(var));
  }

  const Node::Expr getVariable() const {
    return var;
  }
  const Node::Expr getExpr() const {
    return expr;
  }

  void dump(size_t tab) const override;

private:
  Node::Expr var;
  Node::Expr expr;
};

// Operation, ex: (add x 3), (neg (mul@ff (sin x) d_dcos)))
// Call, ex: (fwd$to_float 10 dx)
//
// Represent native operations (add, mul) and calls.
//
// For native, all types must match and that's the return type.
// For calls, return type and operand types must match declaration.
struct Operation : public Node {
  using Ptr = std::shared_ptr<Operation>;
  Operation(const std::string &name, Node::Type type)
    : Node(name, type, Node::OpTy::Operation) { }

  void addOperand(Node::Expr op) {
    ops.push_back(op);
  }
  const Node::Expr getOperand(size_t i) const {
    assert(i < ops.size() && "Offset error");
    return ops[i];
  }

  void dump(size_t tab) const override;

private:
  std::vector<Expr> ops;
};

// Definition, ex: (def fwd$to_float Float ((x : Integer) (dx : (Tuple))) 0.0)
//
// Implementation of a function. Signature needs to match with its declaration,
// if any. The operand names and types must match with the implementation and
// the return type will be used to check types further up.
struct Definition : public Node {
  using Ptr = std::shared_ptr<Definition>;
  Definition(const std::string &name, Node::Type type, Node::Expr impl)
      : Node(name, type, Node::OpTy::Definition), impl(impl) { }

  // Operands and return type (for name and type validation)
  void addOperand(Node::Expr node) {
    operands.push_back(node);
  }
  const Node::Expr getOperand(size_t i) const {
    assert(i < operands.size() && "Offset error");
    return operands[i];
  }
  size_t numOperands() const {
    return operands.size();
  }
  const Node::Expr getImpl() const {
    return impl;
  }

  void dump(size_t tab) const override;

private:
  Node::Expr impl;
  std::vector<Node::Expr> operands;
};

// Declaration, ex: (edef max Float (Float Float))
//
// Declares an external or subsequent function. Will be used for lookup during
// the declaration (to match signature) and also used to emit declarations in
// the final IR.
struct Declaration : public Node {
  using Ptr = std::shared_ptr<Declaration>;
  // Declaration from code
  Declaration(const std::string &name, Node::Type type)
    : Node(name, type, Node::OpTy::Declaration) { }
  // Declaration from existing definition without declaration
  Declaration(const Definition::Ptr def)
    : Node(def->getName(), def->getType(), Node::OpTy::Declaration) {
    for (size_t i=0, e=def->numOperands(); i<e; i++)
      addOperandType(def->getOperand(i)->getType());
  }

  void addOperandType(Node::Type opt) {
    operandTypes.push_back(opt);
  }
  Node::Type getOperandType(size_t i) const {
    assert(i < operandTypes.size() && "Offset error");
    return operandTypes[i];
  }

  void dump(size_t tab) const override;

private:
  std::vector<Node::Type> operandTypes;
};

// Condition, ex: (if (or x y) (add x y) 0)
struct Condition : public Node {
  using Ptr = std::shared_ptr<Condition>;
  Condition(Node::Expr cond, Node::Expr ifBlock, Node::Expr elseBlock)
      : Node("cond", Node::Type::None, Node::OpTy::Condition),
        cond(cond),
        ifBlock(ifBlock),
        elseBlock(elseBlock) { }

  const Node::Expr getIfBlock() const {
    return ifBlock;
  }
  const Node::Expr getElseBlock() const {
    return elseBlock;
  }
  const Node::Expr getCond() const {
    return cond;
  }

  void dump(size_t tab) const override;

private:
  Node::Expr cond;
  Node::Expr ifBlock;
  Node::Expr elseBlock;
};

} // namespace Parser
} // namespace Knossos
#endif // _AST_H_
