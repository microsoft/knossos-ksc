/* Copyright Microsoft Corp. 2020 */
#ifndef _AST_H_
#define _AST_H_

#include <vector>
#include <string>
#include <memory>
#include <cassert>

namespace Knossos {
namespace AST {

// A node in the AST.
//
// These decisions were taken before implementation, so could be completely
// wrong. All restricting choices (const, immutable, etc) have been taken to
// simplify the parser.
struct Node {
  // Type checking occurs from bottom up. Each node is responsible for its own
  // checks and, if valid, their return type can be used for the parents.
  enum Type {
    None,
    String,
    Bool,
    Integer,
    Float,
    Tuple,
    Vec,
    LAST_TYPE = Vec
  };

  // Basic infrastructure
  using Expr = std::shared_ptr<Node>;
  virtual ~Node() { }
  Expr parent;

  // valid types, for safety checks
  bool isValidType() const {
    return type > Type::None && type < LAST_TYPE;
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
  Type getType() const {
    return type;
  }

protected:
  Node() = delete;
  Node(const char* name, Type type) : name(name), type(type) { }
  Node(std::string &name, Type type) : name(name), type(type) { }

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
struct Block : virtual Node {
  Block() : Node("block", Node::Type::None) { }

  void addOp(Node::Expr node) {
    ops.push_back(node);
  }
  const Node::Expr getOp(size_t i) const {
    assert(i < ops.size() && "Offset error");
    return ops[i];
  }

private:
  std::vector<Expr> ops;
};

// Literals, ex: "Hello", 10.0, 123, false
//
// These are constant immutable objects.
// Type is determined by the parser.
struct Literal : virtual Node {
  Literal(Node::Type type, std::string &value)
    : Node(value, type) {
    assert(isValidType() && "Invalid type");
  }

  const std::string &getValue() const {
    return name;
  }
};

// Named variables, ex: (let (str "Hello") (print str)), (def a (x : Float) x)
//
// Variables have a contextual name (scope::name) and an initialisation
// expression. If nested, all types must match. Cannot be reassigned.
struct Variable : virtual Node {
  // Used for (let) expressions, that initialise directly
  Variable(std::string &name, Node::Expr init)
      : Node(name, init->getType()), init(init) { }
  // Used for (def) expressions, that declare arguments
  Variable(std::string &name, Node::Type type)
      : Node(name, type) { }

  void addValue(Node::Expr node) {
    assert(!init && "Variables can only have one initialiser");
    assert(type == node->getType() && "Type mismatch");
    init = node;
  }
  const Node::Expr getValue() const {
    assert(init && "Variables without initialiser");
    return init;
  }

private:
  Node::Expr init;
};

// Operation, ex: (add x 3), (neg (mul@ff (sin x) d_dcos)))
// Call, ex: (fwd$to_float 10 dx)
//
// Represent native operations (add, mul) and calls.
//
// For native, all types must match and that's the return type.
// For calls, return type and operand types must match declaration.
struct Operation : virtual Node {
  Operation(std::string &name, Node::Type type) : Node(name, type) { }

  void addOperand(Node::Expr op) {
    ops.push_back(op);
  }
  const Node::Expr getOperand(size_t i) const {
    assert(i < ops.size() && "Offset error");
    return ops[i];
  }

private:
  std::vector<Expr> ops;
};

// Declaration, ex: (edef max Float (Float Float))
//
// Declares an external or subsequent function. Will be used for lookup during
// the declaration (to match signature) and also used to emit declarations in
// the final IR.
struct Declaration : virtual Node {
  Declaration(std::string &name, Node::Type type) : Node(name, type) { }

  void addOperandType(Node::Type opt) {
    operandTypes.push_back(opt);
  }
  Node::Type getOperandType(size_t i) const {
    assert(i < operandTypes.size() && "Offset error");
    return operandTypes[i];
  }

private:
  std::vector<Node::Type> operandTypes;
};

// Definition, ex: (def fwd$to_float Float ((x : Integer) (dx : (Tuple))) 0.0)
//
// Implementation of a function. Signature needs to match with its declaration,
// if any. The operand names and types must match with the implementation and
// the return type will be used to check types further up.
struct Definition : virtual Node {
  Definition(std::string &name, Node::Type type, Node::Expr impl)
      : Node(name, type), impl(impl) { }

  // Operands and return type (for name and type validation)
  void addOperand(Node::Expr node) {
    operands.push_back(node);
  }
  const Node::Expr getOperand(size_t i) const {
    assert(i < operands.size() && "Offset error");
    return operands[i];
  }

private:
  Node::Expr impl;
  std::vector<Node::Expr> operands;
};

// Condition, ex: (if (or x y) (add x y) 0)
struct Condition : virtual Node {
  Condition(Node::Expr cond, Node::Expr ifBlock, Node::Expr elseBlock)
      : Node("cond", Node::Type::None),
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

private:
  Node::Expr cond;
  Node::Expr ifBlock;
  Node::Expr elseBlock;
};

// Fold, ex: (fold f s0 v)
struct Fold : virtual Node {
  Fold(Node::Expr map, Node::Expr init, Node::Expr list)
    : Node("fold", Node::Type::None),
      map(map), init(init), list(list) { }

  const Node::Expr getMap() const {
    return map;
  }
  const Node::Expr getInit() const {
    return init;
  }
  const Node::Expr getList() const {
    return list;
  }

private:
  Node::Expr map;
  Node::Expr init;
  Node::Expr list;
};

class AST {
  Node::Expr root;
  Node::Expr current;

public:
  AST() {
    root = std::make_shared<Block>();
    current = root;
  }
};

} // namespace AST
} // namespace Knossos
#endif // _AST_H_
