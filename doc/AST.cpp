// This is a copy of AST.h with getters/setters etc stripped.

// All "Ptr"s are currently unique_ptrs, later implementations 
// might hash-cons and do something different.

// Type: Scalars don't need the vector of subTypes, 
// but it's cheap and simple to store an empty vector.
struct Type {
  enum Type_t {
    None,
    String,
    Bool,
    Integer,   // TODO: Bit depths. See #358
    Float,
    Tuple,
    Vector,
    Lambda,
    LinearMap  // Opaque type of "any" linear map
  };
  Type_t type;
  std::vector<Type> subTypes;
};

/// A node in the AST.
struct Expr {
  Type type;
};

/// Literals, ex: "Hello", 10.0, 123, false
struct Literal : public Expr {
  std::string value;
};

/// Named variables, ex:
///   str in (let (str "Hello") (print str))
///     x in (def a (x : Float) x)
struct Variable : public Expr {
  std::string name;
  Expr::Ptr init;
};

/// Let, ex: (let (x 10) (add x 10))
struct Let : public Expr {
  std::vector<Expr::Ptr> vars;  // TODO: make a vector of vars
  Expr::Ptr expr;
};

struct Signature {
  std::string name;
  std::vector<Type> argTypes;
};

/// Declaration, ex: (edef max Float (Float Float))
struct Declaration : public Expr {
  Signature sig;
};

/// Call, ex: (add x 3), (neg (mul (sin x) d_dcos)))
/// Call, ex: (fwd$to_float 10 dx)
struct Call : public Expr {
  Declaration* decl; // A pointer to the corresponding decl, owned elsewhere 
  std::vector<Expr::Ptr> operands;
};

/// Definition, ex: (def fwd$to_float Float ((x : Integer) (dx : (Tuple))) 0.0)
struct Definition : public Expr {
  Declaration::Ptr decl;
  std::vector<Variable::Ptr> arguments;
  Expr::Ptr impl;
};

/// Condition, ex: (if (or x y) (add x y) 0)
struct Condition : public Expr {
  Expr::Ptr cond;
  Expr::Ptr ifBlock;
  Expr::Ptr elseBlock;
};


/// Rule, ex: (rule "mul2" (v : Float) (mul v 2.0) (add v v))
///
/// Rules express ways to transform the graph. They need a special
/// MLIR dialect to be represented and cannot be lowered to LLVM.
struct Rule : public Expr {
  std::string name;
  Expr::Ptr variable;
  Expr::Ptr pattern;
  Expr::Ptr result;
};
