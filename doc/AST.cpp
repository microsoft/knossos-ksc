// This is a copy of AST.h with getters/setters etc stripped.
// It may drift from the precise AST defined there, but tries
// to represent the best current or near-future intentions.

// All "Ptr"s are currently unique_ptrs, later implementations 
// might hash-cons and do something different.

// Type
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
  // Sybtypes (e.g. Tuple T1 T2)
  // Scalars don't need the vector of subTypes, 
  // but it's cheap and simple to store an empty vector.
  std::vector<Type> subTypes;
};

typedef std::string Name; // Might later be atomized

// A node in the AST.
struct Expr {
  // Every node has a type, which may start as None,
  // And then be propagated by type propagation
  Type type;
};

// Literals, ex: "Hello", 10.0, 123, false
struct Literal : public Expr {
  std::string value;
};

// A variable use or definition
//     x in (add 2 x) ; type is implicit
//     x in (def a (x : Float) 2) ; type is explicit
struct Variable : public Expr {
  Name name;
};

// Variable binding, ex:
//   str in (let (str "Hello") (print str))
struct Binding : public Expr {
  Variable var;
  Expr::Ptr init;
};

// Let, ex: (let (x 10) (add x 10))
struct Let : public Expr {
  std::vector<Binding::Ptr> vars;
  Expr::Ptr expr;
};

// Declaration, ex: (edef max Float (Float Float))
struct Declaration : public Expr {
  Name name;
  std::vector<Type> argTypes;
};

/// Call
//    (add x 3)
//    (fwd$to_float 10 dx)
struct Call : public Expr {
  Declaration* decl; // A pointer to the corresponding decl, owned by e.g. Definition
  std::vector<Expr::Ptr> operands;
};

/// Definition, 
//   (def fwd$to_float Float ((x : Integer) (dx : (Tuple))) 0.0)
struct Definition : public Expr {
  Declaration::Ptr decl; 
  std::vector<Variable::Ptr> arguments;
  Expr::Ptr impl;
};

// Condition
//    (if (or x y) (add x y) 0)
struct Condition : public Expr {
  Expr::Ptr cond;
  Expr::Ptr ifBlock;
  Expr::Ptr elseBlock;
};


// Rule
//    (rule "mul2" (v : Float) (mul v 2.0) (add v v))
//
// Rules express ways to transform the graph. 
struct Rule : public Expr {
  Name name;
  std::vector<Variable::Ptr> variables;
  Expr::Ptr pattern;
  Expr::Ptr result;
};
