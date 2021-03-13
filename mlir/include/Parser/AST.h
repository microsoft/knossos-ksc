/* Copyright Microsoft Corp. 2020 */
#ifndef _AST_H_
#define _AST_H_

#include <cassert>
#include <memory>
#include <string>
#include <vector>

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Casting.h"

#include "Assert.h"

namespace Knossos {
namespace AST {

/// Type checking occurs from bottom up. Each node is responsible for its own
/// checks and, if valid, their return type can be used for the parents.
/// Function and variable types are checked by the symbol table
struct Type {
  /// Not enum class because we're already inside Type
  ///// so we can already use AST::Type::Integer
  enum ValidType {
    None,
    String,
    Bool,
    Integer,
    Float,
    LAST_SCALAR=Float,
    Tuple,
    Vector,
    Lambda,
    /// This is not a valid IR type, but can be parsed
    LM
  };

  Type() : type(None) {}

  /// Scalar constructor
  explicit Type(ValidType type) : type(type) {
    ASSERT(isScalar()) << "Type: Scalar constructor called for non-Scalar type: " << type;
  }

  /// Compound type constructor
  Type(ValidType type, std::vector<Type> const& subTys) : type(type) {
    ASSERT(!isScalar()) << "Compound ctor called for Scalar type";
    subTypes = subTys;
  }

  /// Utilities
  static bool isScalar(ValidType type) {
    return type >= None && type <= LAST_SCALAR;
  }
  bool isNone() const {
    return type == None;
  }
  bool isString() const {
    return type == String;
  }
  bool isScalar() const {
    return isScalar(type);
  }
  bool isVector() const {
    return type == Vector;
  }
  bool isTuple() const {
    return type == Tuple;
  }

  ValidType getValidType() const { 
    return type; 
  }

  bool operator ==(ValidType oTy) const {
    return type == oTy;
  }
  
  std::ostream& dump(std::ostream& s) const;

  // Vector accessor
  const Type &getSubType() const {
    assert(type == Vector);
    return subTypes[0];
  }
  // Tuple accessor
  const Type &getSubType(size_t idx) const {
    return subTypes[idx];
  }
  llvm::ArrayRef<Type> getSubTypes() const {
    return subTypes;
  }

  static Type makeVector(Type type) {
    return Type(Vector, {type});
  }

  static Type makeTuple(std::vector<Type> const& types) {
    return Type(Tuple, types);
  }

  static Type makeLambda(Type s, Type t) {
    return Type(Lambda, {s, t});
  }

  static Type makeLM(Type s, Type t) {
    return Type(LM, {s, t});
  }

  Type tangentType() const;

  bool operator<(const Type& that) const {
    return type < that.type || (type == that.type && subTypes < that.subTypes);
  }

  bool operator==(const Type& that) const {
    return type == that.type && subTypes == that.subTypes;
  }

protected:
  ValidType type;
  std::vector<Type> subTypes;
};

inline std::ostream& operator<<(std::ostream& s, Type const& t)
{
  return t.dump(s);
}

char const* ValidType2Str(Type::ValidType type);

/// Tangent type is an AD concept, but very built-in
inline Type Type::tangentType() const
{
  switch (type) {
    case Float:
      return Type(Float);
    case Vector:
      return makeVector(getSubType().tangentType());
    case Tuple: {
      std::vector<Type> newsubTypes {subTypes.size()};
      std::transform(subTypes.begin(), subTypes.end(), newsubTypes.begin(), [](Type const& t) { return t.tangentType(); });
      return makeTuple(newsubTypes);
    }
  }
  return Type(None);
}

// SName: StructuredName
// Either:
//     sin                                String
//     [pow (Tuple Float Integer)]        String Type
//     [fwd [sin Float]]                  String SName
struct SName {
  using Ptr = std::unique_ptr<SName>;

  SName(llvm::StringRef name, Type type=Type(Type::None)):baseFunctionName(name), baseFunctionArgType(type) {}
  SName(llvm::StringRef derivation, SName base) : SName(std::move(base)) { derivations.insert(derivations.begin(), std::string(derivation)); }

  std::vector<std::string> derivations;  // Derivations applied to the base function, in order from outermost to innermost
  std::string baseFunctionName;
  Type baseFunctionArgType;

  bool operator<(SName const& that) const {
    return std::tie(derivations, baseFunctionName, baseFunctionArgType)
      < std::tie(that.derivations, that.baseFunctionName, that.baseFunctionArgType);
  }
  bool operator==(SName const& that) const {
    return derivations == that.derivations
      && baseFunctionName == that.baseFunctionName
      && baseFunctionArgType == that.baseFunctionArgType;
  }
  bool operator!=(SName const& that) const { return !(*this == that); }

  bool isDerivation() const { return !derivations.empty(); }

  bool hasType() const { return !baseFunctionArgType.isNone(); }
};
std::ostream& operator<<(std::ostream& s, SName const& t);

struct Signature {
  std::string name;
  std::vector<Type> argTypes;   

  bool operator<(Signature const& that) const {
    return name < that.name ||
          (name == that.name && argTypes < that.argTypes);
  }
  bool operator==(Signature const& that) const {
    return name == that.name && argTypes == that.argTypes;
  }
};

std::ostream& operator<<(std::ostream& s, Signature const& t);

/// A node in the AST.
struct Expr {
  using Ptr = std::unique_ptr<Expr>;

  /// Expr type, for quick checking in switch/cases
  enum class Kind {
    Invalid,
    Block,
    Type,
    Literal,
    Variable,
    Let,
    Condition,
    Call,
    // TODO: Lambda,
    Declaration,
    Definition,
    Rule,
    // Tuple prims
    Tuple,
    Get,
    // Prims
    Build,
    Fold,
    Assert
  };

  /// valid types, for safety checks
  Type getType() const { return type; }

  /// Set type
  void setType(Type type) { this->type = type; }

  /// Type of the node, for quick access
  const Kind kind;

  virtual ~Expr() = default;
  virtual std::ostream& dump(std::ostream& s, size_t tab = 0) const;

protected:
  Expr(Kind kind) : kind(kind), type(Type::None) {}
  Expr(Type type, Kind kind) : kind(kind), type(type) {}

  /// Type it returns, for type checking
  Type type;
};

inline std::ostream& operator<<(std::ostream& s, Expr const& t)
{
  return t.dump(s);
}


/// Block node has nothing but children
///
/// This should only be used for the program scope. Redundant parenthesis
/// may create single blocks, which will then just pass-through the evaluation
/// of the contained token (ie. multi-token blocks inside code are illegal).
struct Block : public Expr {
  using Ptr = std::unique_ptr<Block>;
  Block() : Expr(Kind::Block){}
  Block(Expr::Ptr op) : Expr(Kind::Block) {
    operands.push_back(std::move(op));
  }

  void addOperand(Expr::Ptr node) {
    type = node->getType();
    operands.push_back(std::move(node));
  }
  llvm::ArrayRef<Expr::Ptr> getOperands() const { return operands; }
  Expr *getOperand(size_t idx) const {
    assert(idx < operands.size() && "Offset error");
    return operands[idx].get();
  }
  size_t size() const { return operands.size(); }

  std::ostream& dump(std::ostream& s, size_t tab = 0) const override;

  /// LLVM RTTI
  static bool classof(const Expr *c) { return c->kind == Kind::Block; }

private:
  std::vector<Expr::Ptr> operands;
};

/// Literals, ex: "Hello", 10.0, 123, false
///
/// These are constant immutable objects.
/// Type is determined by the parser.
struct Literal : public Expr {
  using Ptr = std::unique_ptr<Literal>;
  Literal(llvm::StringRef value, Type::ValidType type)
      : Expr(Type(type), Kind::Literal), value(value) {}

  llvm::StringRef getValue() const { return value; }

  std::ostream& dump(std::ostream& s, size_t tab = 0) const override;

  /// LLVM RTTI
  static bool classof(const Expr *c) { return c->kind == Kind::Literal; }

private:
  std::string value;
};

/// Named variables, ex:
///   str in (let (str "Hello") (print str))
///     x in (def a (x : Float) x)
///
/// Variables have a contextual name (scope::name) and an optional
/// initialisation expression.
struct Variable : public Expr {
  using Ptr = std::unique_ptr<Variable>;
  /// Definition: (x 10) in ( let (x 10) (expr) )
  /// Declaration: (x : Integer) in ( def name Type (x : Integer) (expr) )
  /// We need to bind first, then assign to allow nested lets
  Variable(llvm::StringRef name, Type type=Type(Type::None))
      : Expr(type, Kind::Variable), name(name), init(nullptr) {}

  void setInit(Expr::Ptr &&expr) {
    assert(!init);
    init = std::move(expr);
    if (!type.isNone())
      assert(type == init->getType());
    else
      type = init->getType();
  }
  /// No value == nullptr
  Expr *getInit() const { return init.get(); }
  std::string const& getName() const { return name; }

  std::ostream& dump(std::ostream& s, size_t tab = 0) const override;

  /// LLVM RTTI
  static bool classof(const Expr *c) { return c->kind == Kind::Variable; }

private:
  std::string name;
  Expr::Ptr init;
};

/// Let, ex: (let (x 10) (add x 10))
///
/// Defines a variable.
struct Let : public Expr {
  using Ptr = std::unique_ptr<Let>;
  Let(std::vector<Expr::Ptr> &&vars)
      : Expr(Kind::Let), vars(std::move(vars)),
      expr(nullptr) {}
  Let(std::vector<Expr::Ptr> &&vars, Expr::Ptr expr)
      : Expr(expr->getType(), Kind::Let), vars(std::move(vars)),
        expr(std::move(expr)) {}

  llvm::ArrayRef<Expr::Ptr> getVariables() const { return vars; }
  Expr *getVariable(size_t idx) const {
    assert(idx < vars.size() && "Offset error");
    return vars[idx].get();
  }
  Expr *getExpr() const { return expr.get(); }
  size_t size() const { return vars.size(); }

  std::ostream& dump(std::ostream& s, size_t tab = 0) const override;

  /// LLVM RTTI
  static bool classof(const Expr *c) { return c->kind == Kind::Let; }

private:
  std::vector<Expr::Ptr> vars;  // TODO: make a vector of vars
  Expr::Ptr expr;
};

/// Declaration, ex: (edef max Float (Float Float))
///
/// Declares a function (external or posterior). Will be used for lookup during
/// the declaration (to match signature) and also used to emit declarations in
/// the final IR.
struct Declaration : public Expr {
  using Ptr = std::unique_ptr<Declaration>;
  Declaration(llvm::StringRef name, Type type)
      : Expr(type, Kind::Declaration), name(name) {}

  Declaration(llvm::StringRef name, Type type, std::vector<Type> argTypes)
      : Expr(type, Kind::Declaration), name(name), argTypes(move(argTypes)) {}

  void addArgType(Type opt) { argTypes.push_back(opt); }
  llvm::ArrayRef<Type> getArgTypes() const { return argTypes; }
  Type getArgType(size_t idx) const {
    assert(idx < argTypes.size() && "Offset error");
    return argTypes[idx];
  }
  llvm::StringRef getName() const { return name; }

  std::string getMangledName() const;

  size_t size() const { return argTypes.size(); }

  std::ostream& dump(std::ostream& s, size_t tab = 0) const override;

  /// LLVM RTTI
  static bool classof(const Expr *c) {
    return c->kind == Kind::Declaration;
  }

private:
  // TODO: use Signature here
  std::string name;
  std::vector<Type> argTypes;   
};

/// Call, ex: (add x 3), (neg (mul (sin x) d_dcos)))
/// Call, ex: (fwd$to_float 10 dx)
///
/// Represent native operations (add, mul) and calls.
///
/// Return type and operand types must match declaration.
struct Call : public Expr {
  using Ptr = std::unique_ptr<Call>;
  Call(Declaration* decl, std::vector<Expr::Ptr> operands)
      : Expr(decl->getType(), Kind::Call), decl(decl), operands(std::move(operands)) { } 
  
  void setDeclaration(Declaration* decl) { this->decl = decl; }
  Declaration const* getDeclaration() const { return decl; } 
  Declaration* getDeclaration() { return decl; } 
  
  size_t size() const { return operands.size(); }
  void addOperand(Expr::Ptr op) { operands.push_back(std::move(op)); }
  llvm::ArrayRef<Expr::Ptr> getOperands() const { return operands; }
  Expr *getOperand(size_t idx) const {
    assert(idx < operands.size() && "Offset error");
    return operands[idx].get();
  }
  
  std::ostream& dump(std::ostream& s, size_t tab = 0) const override;

  /// LLVM RTTI
  static bool classof(const Expr *c) {
    return c->kind == Kind::Call;
  }

private:
  // A pointer to the corresponding decl, owned elsewhere 
  Declaration* decl;
  std::vector<Expr::Ptr> operands;
};

/// Definition, ex: (def fwd$to_float Float ((x : Integer) (dx : (Tuple))) 0.0)
///
/// Implementation of a function. Declarations populate the prototype,
/// definitions complete the variable list and implementation, while also
/// validating the arguments and return types.
struct Definition : public Expr {
  using Ptr = std::unique_ptr<Definition>;
  Definition(llvm::StringRef name, Type type)
      : Expr(type, Kind::Definition) {
    decl = std::make_unique<Declaration>(name, type);
  }

  /// Arguments and return type (for name and type validation)
  void addArgument(Variable::Ptr node) {
    decl->addArgType(node->getType());
    arguments.push_back(std::move(node));
  }
  llvm::ArrayRef<Variable::Ptr> getArguments() const { return arguments; }
  Variable *getArgument(size_t idx) {
    assert(idx < arguments.size() && "Offset error");
    return arguments[idx].get();
  }
  void setImpl(Expr::Ptr expr) {
    assert(!impl && "Cannot reset function implementation");
    impl = std::move(expr);
  }
  Expr *getImpl() const { return impl.get(); }
  Declaration *getDeclaration() const { return decl.get(); }
  llvm::StringRef getName() const { return decl->getName(); }
  std::string getMangledName() const { return decl->getMangledName(); }
  size_t size() const { return arguments.size(); }

  std::ostream& dump(std::ostream& s, size_t tab = 0) const override;

  /// LLVM RTTI
  static bool classof(const Expr *c) {
    return c->kind == Kind::Definition;
  }

private:
  Declaration::Ptr decl;
  std::vector<Variable::Ptr> arguments;
  Expr::Ptr impl;
};


/// Condition, ex: (if (or x y) (add x y) 0)
struct Condition : public Expr {
  using Ptr = std::unique_ptr<Condition>;
  Condition(Expr::Ptr cond, Expr::Ptr ifBlock, Expr::Ptr elseBlock)
      : Expr(Kind::Condition), cond(std::move(cond)),
        ifBlock(std::move(ifBlock)), elseBlock(std::move(elseBlock)) {
    assert(this->cond->getType() == Type::Bool &&
           "Condition should be boolean");
    assert(this->ifBlock->getType() == this->elseBlock->getType() &&
           "Type mismatch");
    type = this->ifBlock->getType();
  }

  Expr *getIfBlock() const { return ifBlock.get(); }
  Expr *getElseBlock() const { return elseBlock.get(); }
  Expr *getCond() const { return cond.get(); }

  std::ostream& dump(std::ostream& s, size_t tab = 0) const override;

  /// LLVM RTTI
  static bool classof(const Expr *c) {
    return c->kind == Kind::Condition;
  }

private:
  Expr::Ptr cond;
  Expr::Ptr ifBlock;
  Expr::Ptr elseBlock;
};

/// Build, ex: (build N (lam (var) (expr)))
///
/// Loops over range N using lambda L
struct Build : public Expr {
  using Ptr = std::unique_ptr<Build>;
  Build(Expr::Ptr range, Expr::Ptr var, Expr::Ptr expr)
      : Expr(Type::makeVector(expr->getType()), Kind::Build), 
        range(std::move(range)),
        var(std::move(var)), 
        expr(std::move(expr)) {}

  Expr *getRange() const { return range.get(); }
  Expr *getVariable() const { return var.get(); }
  Expr *getExpr() const { return expr.get(); }

  std::ostream& dump(std::ostream& s, size_t tab = 0) const override;

  /// LLVM RTTI
  static bool classof(const Expr *c) { return c->kind == Kind::Build; }

private:
  Expr::Ptr range;
  Expr::Ptr var;
  Expr::Ptr expr;
};

/// Tuple, ex: (tuple 10.0 42 (add@ff 1.0 2.0))
///
/// Builds a tuple, inferring the types
struct Tuple : public Expr {
  using Ptr = std::unique_ptr<Tuple>;
  Tuple(std::vector<Expr::Ptr> &&elements)
      : Expr(Kind::Tuple), 
        elements(std::move(elements)) {
    std::vector<Type> types;
    for (auto &el: this->elements)
      types.push_back(el->getType());
    type = { Type::Tuple, std::move(types) };
  }

  llvm::ArrayRef<Expr::Ptr> getElements() const { return elements; }
  Expr *getElement(size_t idx) {
    assert(idx < elements.size() && "Offset error");
    return elements[idx].get();
  }
  size_t size() const { return elements.size(); }

  std::ostream& dump(std::ostream& s, size_t tab = 0) const override;

  /// LLVM RTTI
  static bool classof(const Expr *c) { return c->kind == Kind::Tuple; }

private:
  std::vector<Expr::Ptr> elements;
};

/// Get, ex: (get$7$9 tuple)
///
/// Extract the Nth element from a tuple. 
/// Note: index range is [1, N]
struct Get : public Expr {
  using Ptr = std::unique_ptr<Get>;
  Get(size_t index, size_t max, Expr::Ptr expr)
      : Expr(Kind::Get), index(index), expr(std::move(expr)) {
    assert(this->expr->getType() == Type::Tuple && "Invalid expriable type");
    assert(index > 0 && index <= max && "Out of bounds tuple index");
    type = this->expr->getType().getSubType(index-1);
  }

  size_t getIndex() const { return index; }
  Expr *getExpr() const { return expr.get(); }
  Expr *getElement() const {
    auto tuple = llvm::dyn_cast<Tuple>(expr.get());
    return tuple->getElement(index-1);
  }

  std::ostream& dump(std::ostream& s, size_t tab = 0) const override;

  /// LLVM RTTI
  static bool classof(const Expr *c) { return c->kind == Kind::Get; }

private:
  size_t index;
  Expr::Ptr expr;
};

/// Fold, ex: (fold (lambda) init vector)
///
/// Reduce pattern. Initialises an accumulator (acc) with (init), loops over the
/// (vector), using (lambda) to update the accumulator value. The signature
/// of the (lambda) MUST be:
///   (lam AccType (acc_i : (Tuple AccType ElmType)) (expr))
/// for (vector) of type "(Vec ElmType)" and (acc) with type "AccType".
///
/// Fold will iterare the vector, calling the lambda like:
/// for elm in vector:
///   acc = lambda((tuple acc elm))
/// Then return (acc).
struct Fold : public Expr {
  using Ptr = std::unique_ptr<Fold>;
  Fold(Type type, Expr::Ptr body, Expr::Ptr acc, Expr::Ptr vector)
      : Expr(type, Kind::Fold), body(std::move(body)),
      acc(std::move(acc)), vector(std::move(vector)) {}

  Expr *getVector() const { return vector.get(); }
  Expr *getAcc() const { return acc.get(); }
  Expr *getBody() const { return body.get(); }

  std::ostream& dump(std::ostream& s, size_t tab = 0) const override;

  /// LLVM RTTI
  static bool classof(const Expr *c) { return c->kind == Kind::Fold; }

private:
  Expr::Ptr body;
  Expr::Ptr acc;
  Expr::Ptr vector;
};

/// Rule, ex: (rule "mul2" (v : Float) (mul v 2.0) (add v v))
///
/// Rules express ways to transform the graph. They need a special
/// MLIR dialect to be represented and cannot be lowered to LLVM.
struct Rule : public Expr {
  using Ptr = std::unique_ptr<Rule>;
  Rule(llvm::StringRef name, Expr::Ptr variable, Expr::Ptr pattern,
       Expr::Ptr result)
      : Expr(Kind::Rule), name(name),
        variable(std::move(variable)), pattern(std::move(pattern)),
        result(std::move(result)) {}

  llvm::StringRef getName() const { return name; }
  Expr *getExpr() const { return variable.get(); }
  Expr *getPattern() const { return pattern.get(); }
  Expr *getResult() const { return result.get(); }

  std::ostream& dump(std::ostream& s, size_t tab = 0) const override;

  /// LLVM RTTI
  static bool classof(const Expr *c) {
    return c->kind == Kind::Rule;
  }

private:
  std::string name;
  Expr::Ptr variable;
  Expr::Ptr pattern;
  Expr::Ptr result;
};

} // namespace AST
} // namespace Knossos
#endif /// _AST_H_
