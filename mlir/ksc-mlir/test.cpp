#include "Parser/MLIR.h"
#include "Parser/Parser.h"
#include <iostream>

using namespace Knossos::AST;
using namespace Knossos::MLIR;
using namespace std;

static int verbose = 0;

// ======================================================= Helpers

#define LOC() Location { __FILE__, __LINE__, 0 }

Expr::Ptr parse(const Location& loc, const string &code) {
  Parser p(loc, code, verbose);
  if (verbose > 2)
    cout << " -- Tokens\n";
  p.tokenise();
  if (verbose > 2)
    p.getRootToken()->dump(std::cout);
  if (verbose > 1)
    cout << " -- AST\n";
  p.parse();
  if (verbose > 1)
    p.getRootNode()->dump(std::cout);
  return p.moveRoot();
}

// ======================================================= Lexer test

void test_lexer() {
  cout << "\n == test_lexer\n";
  Lexer l(LOC(), "(def f1 Integer ((x : Integer) (y : Integer)) (add x y))");
  auto root = l.lex();
  if (verbose > 2) {
    cout << " -- Tokens\n";
    root->dump(std::cout);
  }

  // Root can have many exprs, here only one
  assert(root->isValue == false);
  const Token *tok = root->getChild(0);
  // Kind is not a value and has 5 sub-exprs
  assert(tok->isValue == false);
  assert(tok->size() == 5);
  // Which are:
  assert(tok->getChild(0)->getValue() == "def");
  assert(tok->getChild(1)->getValue() == "f1");
  assert(tok->getChild(2)->getValue() == "Integer");
  const Token *args = tok->getChild(3);
  const Token *arg0 = args->getChild(0);
  assert(arg0->getChild(0)->getValue() == "x");
  assert(arg0->getChild(1)->getValue() == ":");
  assert(arg0->getChild(2)->getValue() == "Integer");
  const Token *arg1 = args->getChild(1);
  assert(arg1->getChild(0)->getValue() == "y");
  assert(arg1->getChild(1)->getValue() == ":");
  assert(arg1->getChild(2)->getValue() == "Integer");
  const Token *impl = tok->getChild(4);
  assert(impl->getChild(0)->getValue() == "add");
  assert(impl->getChild(1)->getValue() == "x");
  assert(impl->getChild(2)->getValue() == "y");
  cout << "    OK\n";
}

// ======================================================= Parser test

void test_parser_block() {
  cout << "\n == test_parser_block\n";
  const Expr::Ptr tree = parse(LOC(), "(tuple 10.0 42 \"\" \" \" \"Hello\" \"Hello world\")");

  // Root can have many exprs, here only one
  Block* root = llvm::dyn_cast<Block>(tree.get());
  assert(root);
  // Kind is Block and has 1 sub-expr
  Tuple* tuple = llvm::dyn_cast<Tuple>(root->getOperand(0));
  assert(tuple);
  // Block has 6 literals
  Literal* op0 = llvm::dyn_cast<Literal>(tuple->getElement(0));
  assert(op0);
  assert(op0->getValue() == "10.0");
  assert(op0->getType() == Type::Float);
  Literal* op1 = llvm::dyn_cast<Literal>(tuple->getElement(1));
  assert(op1);
  assert(op1->getValue() == "42");
  assert(op1->getType() == Type::Integer);
  Literal* op2 = llvm::dyn_cast<Literal>(tuple->getElement(2));
  assert(op2);
  assert(op2->getValue() == "");
  assert(op2->getType() == Type::String);
  Literal* op3 = llvm::dyn_cast<Literal>(tuple->getElement(3));
  assert(op3);
  assert(op3->getValue() == " ");
  assert(op3->getType() == Type::String);
  Literal* op4 = llvm::dyn_cast<Literal>(tuple->getElement(4));
  assert(op4);
  assert(op4->getValue() == "Hello");
  assert(op4->getType() == Type::String);
  Literal* op5 = llvm::dyn_cast<Literal>(tuple->getElement(5));
  assert(op5);
  assert(op5->getValue() == "Hello world");
  assert(op5->getType() == Type::String);
  cout << "    OK\n";
}

void test_parser_let() {
  cout << "\n == test_parser_let\n";
  const Expr::Ptr tree = parse(LOC(), "(let (x 10) (add x 10))");

  // Root can have many exprs, here only one
  Block* root = llvm::dyn_cast<Block>(tree.get());
  assert(root);
  // Kind is Let
  Let* def = llvm::dyn_cast<Let>(root->getOperand(0));
  assert(def);
  // Let has two parts: variable definitions and expression
  assert(def->getType() == Type::Integer);
  assert(!def->getBinding().isTupleUnpacking());
  Variable* x = def->getBinding().getVariable();
  assert(x);
  assert(x->getType() == Type::Integer);
  Call* expr = llvm::dyn_cast<Call>(def->getExpr());
  assert(expr);
  assert(expr->getDeclaration()->getName() == "add");
  assert(expr->getType() == Type::Integer);
  auto var = llvm::dyn_cast<Variable>(expr->getOperand(0));
  assert(var);
  assert(var->getName() == "x");
  assert(var->getType() == Type::Integer);
  auto lit = llvm::dyn_cast<Literal>(expr->getOperand(1));
  assert(lit);
  assert(lit->getValue() == "10");
  assert(lit->getType() == Type::Integer);
  cout << "    OK\n";
}

void test_parser_tuple_let() {
  cout << "\n == test_parser_tuple_let\n";
  const Expr::Ptr tree = parse(LOC(), "(let ((a b) (tuple 1 5)) (add a b))");

  // Root can have many exprs, here only one
  Block* root = llvm::dyn_cast<Block>(tree.get());
  assert(root);
  // Kind is Let
  Let* def = llvm::dyn_cast<Let>(root->getOperand(0));
  assert(def);
  // Let has two parts: variable definitions and expression
  assert(def->getType() == Type::Integer);
  assert(def->getBinding().isTupleUnpacking());
  assert(Tuple::classof(def->getBinding().getInit()));
  Variable* a = def->getBinding().getTupleVariable(0);
  assert(a);
  assert(a->getType() == Type::Integer);
  Variable* b = def->getBinding().getTupleVariable(1);
  assert(b);
  assert(b->getType() == Type::Integer);
  Call* expr = llvm::dyn_cast<Call>(def->getExpr());
  assert(expr);
  assert(expr->getDeclaration()->getName() == "add");
  assert(expr->getType() == Type::Integer);
  auto var_a = llvm::dyn_cast<Variable>(expr->getOperand(0));
  assert(var_a);
  assert(var_a->getName() == "a");
  assert(var_a->getType() == Type::Integer);
  auto var_b = llvm::dyn_cast<Variable>(expr->getOperand(1));
  assert(var_b);
  assert(var_b->getName() == "b");
  assert(var_b->getType() == Type::Integer);
  cout << "    OK\n";
}

void test_parser_decl() {
  cout << "\n == test_parser_decl\n";
  const Expr::Ptr tree = parse(LOC(), "(edef fun Float (Integer String Bool))");

  // Root can have many exprs, here only one
  Block* root = llvm::dyn_cast<Block>(tree.get());
  assert(root);
  // Kind is Declaration
  Declaration* decl = llvm::dyn_cast<Declaration>(root->getOperand(0));
  assert(decl);
  // Declaration has 3 parts: name, return type, arg types decl
  assert(decl->getName() == "fun");
  assert(decl->getType() == Type::Float);
  assert(decl->getArgType().isTuple());
  assert(decl->getArgType().getSubType(0) == Type::Integer);
  assert(decl->getArgType().getSubType(1) == Type::String);
  assert(decl->getArgType().getSubType(2) == Type::Bool);
  cout << "    OK\n";
}

void test_parser_def() {
  cout << "\n == test_parser_def\n";
  const Expr::Ptr tree =
      parse(LOC(), "(def fun Integer ((x : Integer) (y : Integer)) (add x 10))");

  // Root can have many exprs, here only one
  Block* root = llvm::dyn_cast<Block>(tree.get());
  assert(root);
  // Kind is Definition
  Definition* def = llvm::dyn_cast<Definition>(root->getOperand(0));
  assert(def);
  // Definition has 4 parts: name, return type, arg types def, expr
  assert(def->getName() == "fun");
  assert(def->getType() == Type::Integer);
  assert(def->size() == 2);
  Variable* x = llvm::dyn_cast<Variable>(def->getArgument(0));
  assert(x->getType() == Type::Integer);
  Call* expr = llvm::dyn_cast<Call>(def->getImpl());
  assert(expr);
  assert(expr->getDeclaration()->getName() == "add");
  assert(expr->getType() == Type::Integer);
  auto var = llvm::dyn_cast<Variable>(expr->getOperand(0));
  assert(var);
  assert(var->getName() == "x");
  assert(var->getType() == Type::Integer);
  auto lit = llvm::dyn_cast<Literal>(expr->getOperand(1));
  assert(lit);
  assert(lit->getValue() == "10");
  assert(lit->getType() == Type::Integer);
  cout << "    OK\n";
}

void test_parser_decl_def_use() {
  cout << "\n == test_parser_decl_def_use\n";
  const Expr::Ptr tree = parse(LOC(), 
                               "(edef fun Integer (Integer))\n"
                               "(def fun Integer ((x : Integer)) (add x 10))\n"
                               "(def main Integer () (add (fun 10) 10))");

  // Root can have many exprs, here only 3
  Block* root = llvm::dyn_cast<Block>(tree.get());
  assert(root);
  // We're interested in the main call only
  Definition* main = llvm::dyn_cast<Definition>(root->getOperand(2));
  assert(main);
  assert(main->getName() == "main");
  assert(main->getType() == Type::Integer);
  assert(main->size() == 0);
  // And its implementation
  Call* impl = llvm::dyn_cast<Call>(main->getImpl());
  assert(impl);
  assert(impl->getDeclaration()->getName() == "add");
  assert(impl->getType() == Type::Integer);
  // Arg1 is a call to fun
  Call* call = llvm::dyn_cast<Call>(impl->getOperand(0));
  assert(call);
  assert(call->getDeclaration()->getName() == "fun");
  assert(call->getType() == Type::Integer);
  auto arg0 = llvm::dyn_cast<Literal>(call->getOperand(0));
  assert(arg0);
  assert(arg0->getValue() == "10");
  assert(arg0->getType() == Type::Integer);
  // Arg2 is just a literal
  auto lit = llvm::dyn_cast<Literal>(impl->getOperand(1));
  assert(lit);
  assert(lit->getValue() == "10");
  assert(lit->getType() == Type::Integer);
  cout << "    OK\n";
}

void test_parser_cond() {
  cout << "\n == test_parser_cond\n";
  const Expr::Ptr tree = parse(LOC(), "(edef fun Integer (Integer))"
                               "(def fun Integer ((x : Integer)) (add x 10))"
                               "(if true (fun 10) (add 10 10))");

  // Root can have many exprs, here only 3
  Block* root = llvm::dyn_cast<Block>(tree.get());
  assert(root);
  // We're interested in the conditional only
  Condition* cond = llvm::dyn_cast<Condition>(root->getOperand(2));
  assert(cond);
  // Condition block is Bool true
  auto condVal = llvm::dyn_cast<Literal>(cond->getCond());
  assert(condVal);
  assert(condVal->getValue() == "true");
  assert(condVal->getType() == Type::Bool);
  // If block is "fun" call
  Call* call = llvm::dyn_cast<Call>(cond->getIfBlock());
  assert(call);
  assert(call->getDeclaration()->getName() == "fun");
  assert(call->getType() == Type::Integer);
  auto arg = llvm::dyn_cast<Literal>(call->getOperand(0));
  assert(arg);
  assert(arg->getValue() == "10");
  assert(arg->getType() == Type::Integer);
  // Else block is an "add" op
  Call* expr = llvm::dyn_cast<Call>(cond->getElseBlock());
  assert(expr);
  assert(expr->getDeclaration()->getName() == "add");
  assert(expr->getType() == Type::Integer);
  auto op0 = llvm::dyn_cast<Literal>(expr->getOperand(0));
  assert(op0);
  assert(op0->getValue() == "10");
  assert(op0->getType() == Type::Integer);
  auto op1 = llvm::dyn_cast<Literal>(expr->getOperand(1));
  assert(op1);
  assert(op1->getValue() == "10");
  assert(op1->getType() == Type::Integer);
  cout << "    OK\n";
}

void test_parser_rule() {
  cout << "\n == test_parser_rule\n";
  const Expr::Ptr tree = parse(LOC(), "(rule \"mul2\" (v : Float) (mul v 2.0) (add v v))");

  // Root can have many exprs, here only 3
  Block* root = llvm::dyn_cast<Block>(tree.get());
  assert(root);
  // Kind is Rule
  Rule* rule = llvm::dyn_cast<Rule>(root->getOperand(0));
  assert(rule);
  // Check name and variable
  assert(rule->getName() == "mul2");
  Variable *var = llvm::dyn_cast<Variable>(rule->getExpr());
  assert(var);
  assert(var->getName() == "v");
  assert(var->getType() == Type::Float);
  // From/To patterns
  Call *from = llvm::dyn_cast<Call>(rule->getPattern());
  Call *to = llvm::dyn_cast<Call>(rule->getResult());
  assert(from && to);
  assert(from->getDeclaration()->getName() == "mul");
  assert(from->getType() == Type::Float);
  assert(from->size() == 2);
  assert(to->getDeclaration()->getName() == "add");
  assert(to->getType() == Type::Float);
  assert(to->size() == 2);
  cout << "    OK\n";
}

void test_parser_vector_type() {
  cout << "\n == test_parser_vector_type\n";
  const Expr::Ptr tree = parse(LOC(), "(edef foo (Vec Float) (Vec Float))\n"
                               "(edef bar (Vec (Vec Float)) (Integer (Vec Float) Float))");

  // Root can have many exprs, here two
  Block* root = llvm::dyn_cast<Block>(tree.get());
  assert(root);
  // Kind is Decl
  Declaration *foo = llvm::dyn_cast<Declaration>(root->getOperand(0));
  Declaration *bar = llvm::dyn_cast<Declaration>(root->getOperand(1));
  assert(foo && bar);
  // Check vectors were detected properly
  auto fooTy = foo->getType();
  assert(fooTy == Type::Vector && fooTy.getSubType() == Type::Float);
  auto fooArgTy = foo->getArgType();
  assert(fooArgTy == Type::Vector && fooArgTy.getSubType() == Type::Float);
  auto barTy = bar->getType();
  assert(barTy == Type::Vector && barTy.getSubType() == Type::Vector);
  auto barSubTy = barTy.getSubType();
  assert(barSubTy == Type::Vector && barSubTy.getSubType() == Type::Float);
  assert(bar->getArgType().isTuple());
  assert(bar->getArgType().getSubType(0) == Type::Integer);
  auto barArgTy = bar->getArgType().getSubType(1);
  assert(barArgTy == Type::Vector && barArgTy.getSubType() == Type::Float);
  assert(bar->getArgType().getSubType(2) == Type::Float);
  cout << "    OK\n";
}

void test_parser_build() {
  cout << "\n == test_parser_build\n";
  const Expr::Ptr tree = parse(LOC(), "(build 10 (lam (i : Integer) (add i i)))");

  // Root can have many exprs, here only one
  Block* root = llvm::dyn_cast<Block>(tree.get());
  assert(root);
  // Kind is Build
  Build* build = llvm::dyn_cast<Build>(root->getOperand(0));
  assert(build);
  // Build has three parts: range definition, induction variable and loop body
  assert(build->getType() == Type::Vector);
  assert(build->getType().getSubType() == Type::Integer);
  auto range = llvm::dyn_cast<Literal>(build->getRange());
  assert(range);
  assert(range->getValue() == "10");
  assert(range->getType() == Type::Integer);
  Variable* v = llvm::dyn_cast<Variable>(build->getVariable());
  assert(v->getType() == Type::Integer);
  Call* expr = llvm::dyn_cast<Call>(build->getExpr());
  assert(expr);
  assert(expr->getDeclaration()->getName() == "add");
  assert(expr->getType() == Type::Integer);
  auto var = llvm::dyn_cast<Variable>(expr->getOperand(0));
  assert(var);
  assert(var->getName() == "i");
  assert(var->getType() == Type::Integer);
  var = llvm::dyn_cast<Variable>(expr->getOperand(1));
  assert(var);
  assert(var->getName() == "i");
  assert(var->getType() == Type::Integer);
  cout << "    OK\n";
}

void test_parser_vector() {
  cout << "\n == test_parser_vector\n";
  const Expr::Ptr tree = parse(LOC(), "(edef foo (Vec Float) (Vec Float)) "
                               "(edef bar (Vec (Vec Float)) (Integer (Vec Float) Float))");

  // Root can have many exprs, here two
  Block* root = llvm::dyn_cast<Block>(tree.get());
  assert(root);
  // Kind is Decl
  Declaration *foo = llvm::dyn_cast<Declaration>(root->getOperand(0));
  Declaration *bar = llvm::dyn_cast<Declaration>(root->getOperand(1));
  assert(foo && bar);
  // Check vectors were detected properly
  auto fooTy = foo->getType();
  assert(fooTy == Type::Vector && fooTy.getSubType() == Type::Float);
  auto fooArgTy = foo->getArgType();
  assert(fooArgTy == Type::Vector && fooArgTy.getSubType() == Type::Float);
  auto barTy = bar->getType();
  assert(barTy == Type::Vector && barTy.getSubType() == Type::Vector);
  auto barSubTy = barTy.getSubType();
  assert(barSubTy == Type::Vector && barSubTy.getSubType() == Type::Float);
  assert(bar->getArgType().isTuple());
  assert(bar->getArgType().getSubType(0) == Type::Integer);
  auto barArgTy = bar->getArgType().getSubType(1);
  assert(barArgTy == Type::Vector && barArgTy.getSubType() == Type::Float);
  assert(bar->getArgType().getSubType(2) == Type::Float);
  cout << "    OK\n";
}

void test_parser_tuple_type() {
  cout << "\n == test_parser_tuple_type\n";
  const Expr::Ptr tree = parse(LOC(), "(edef foo (Tuple Float Float) (Tuple Float Float))\n"
                               "(edef bar Float (Integer (Tuple Float Float) Float))\n"
                               "(edef baz Bool (Tuple Float (Tuple Integer Float)))");

  // Root can have many exprs, here two
  Block* root = llvm::dyn_cast<Block>(tree.get());
  assert(root);
  // Kind is Decl
  Declaration *foo = llvm::dyn_cast<Declaration>(root->getOperand(0));
  Declaration *bar = llvm::dyn_cast<Declaration>(root->getOperand(1));
  Declaration *baz = llvm::dyn_cast<Declaration>(root->getOperand(2));
  assert(foo && bar && baz);
  // Check vectors were detected properly
  auto fooTy = foo->getType();
  assert(fooTy.isTuple() && fooTy.getSubType(0) == Type::Float);
  auto fooArgTy = foo->getArgType();
  assert(fooArgTy.isTuple() &&
         fooArgTy.getSubType(0) == Type::Float &&
         fooArgTy.getSubType(1) == Type::Float);
  auto barTy = bar->getType();
  assert(barTy == Type::Float);
  assert(bar->getArgType().isTuple());
  assert(bar->getArgType().getSubType(0) == Type::Integer);
  auto barArgTy = bar->getArgType().getSubType(1);
  assert(barArgTy.isTuple() &&
         barArgTy.getSubType(0) == Type::Float &&
         barArgTy.getSubType(1) == Type::Float);
  assert(bar->getArgType().getSubType(2) == Type::Float);
  auto bazTy = baz->getType();
  assert(bazTy == Type::Bool);
  auto bazArgTy = baz->getArgType();
  assert(bazArgTy.isTuple() &&
         bazArgTy.getSubType(0) == Type::Float &&
         bazArgTy.getSubType(1).isTuple());
  auto bazSubArgTy = bazArgTy.getSubType(1);
  assert(bazSubArgTy.isTuple() &&
         bazSubArgTy.getSubType(0) == Type::Integer &&
         bazSubArgTy.getSubType(1) == Type::Float);
  cout << "    OK\n";
}

void test_parser_tuple() {
  cout << "\n == test_parser_tuple\n";
  const Expr::Ptr tree = parse(LOC(), "(tuple (add 3.14 2.72) false 42)");

  // Root can have many exprs, here two
  Block* root = llvm::dyn_cast<Block>(tree.get());
  assert(root);
  // Kind is Tuple
  Tuple* tuple = llvm::dyn_cast<Tuple>(root->getOperand(0));
  assert(tuple);
  auto type = tuple->getType();
  assert(type.isTuple() &&
         type.getSubType(0) == Type::Float &&
         type.getSubType(1) == Type::Bool &&
         type.getSubType(2) == Type::Integer);
  // Check elements are correct
  Call* op = llvm::dyn_cast<Call>(tuple->getElement(0));
  assert(op->getDeclaration()->getName() == "add");
  assert(llvm::dyn_cast<Literal>(op->getOperand(0))->getValue() == "3.14");
  assert(llvm::dyn_cast<Literal>(op->getOperand(1))->getValue() == "2.72");
  assert(llvm::dyn_cast<Literal>(tuple->getElement(1))->getValue() == "false");
  assert(llvm::dyn_cast<Literal>(tuple->getElement(2))->getValue() == "42");
  cout << "    OK\n";
}

void test_parser_get() {
  cout << "\n == test_parser_get\n";
  const Expr::Ptr tree = parse(LOC(), "(get$2$3 (tuple (add 3.14 2.72) false 42))");

  // Root can have many exprs, here two
  Block* root = llvm::dyn_cast<Block>(tree.get());
  assert(root);
  // Kind is Get
  Get* get = llvm::dyn_cast<Get>(root->getOperand(0));
  assert(get);
  assert(get->getIndex() == 2);
  Literal* elm = llvm::dyn_cast<Literal>(get->getElement());
  assert(elm);
  assert(elm->getType() == Type::Bool && elm->getValue() == "false");
  cout << "    OK\n";
}

void test_parser_fold() {
  cout << "\n == test_parser_fold\n";
  const Expr::Ptr tree = parse(LOC(), "\n"
                               "(def fun Float (v : (Vec Float))\n"
                               " (fold (lam (acc_x : (Tuple Float Float))\n"
                               "   (let ((acc x) acc_x)\n"
                               "     (mul acc x)))\n"
                               "   1.0\n"
                               "   v))");

  // Root can have many exprs, here two
  Block* root = llvm::dyn_cast<Block>(tree.get());
  assert(root);
  Definition* def = llvm::dyn_cast<Definition>(root->getOperand(0));
  assert(def);
  // Kind is Fold
  Fold* fold = llvm::dyn_cast<Fold>(def->getImpl());
  assert(fold);
  assert(fold->getType() == Type::Float);
  // Vector
  Variable* vec = llvm::dyn_cast<Variable>(fold->getVector());
  assert(vec);
  assert(vec->getType() == Type::Vector);
  assert(vec->getType().getSubType() == Type::Float);
  assert(vec->getName() == "v");
  // Lambda parameter
  Variable* acc_x = fold->getLambdaParameter();
  assert(acc_x);
  assert(acc_x->getType().isTuple());
  assert(acc_x->getType().getSubType(0) == Type::Float);
  assert(acc_x->getType().getSubType(1) == Type::Float);
  // Init
  Literal* init = llvm::dyn_cast<Literal>(fold->getInit());
  assert(init);
  assert(init->getType() == Type::Float);
  assert(init->getValue() == "1.0");
  // Lambda variables
  Let* let = llvm::dyn_cast<Let>(fold->getBody());
  assert(let);
  assert(let->getType() == Type::Float);
  Binding const& letBinding = let->getBinding();
  assert(letBinding.isTupleUnpacking());
  Variable* letAccVar = letBinding.getTupleVariable(0);
  assert(letAccVar);
  assert(letAccVar->getType() == Type::Float);
  assert(letAccVar->getName() == "acc");
  Variable* letXVar = letBinding.getTupleVariable(1);
  assert(letXVar);
  assert(letXVar->getType() == Type::Float);
  assert(letXVar->getName() == "x");
  assert(Variable::classof(letBinding.getInit()));
  // Lambda operation
  Call* op = llvm::dyn_cast<Call>(let->getExpr());
  assert(op);
  assert(op->getType() == Type::Float);
  assert(op->getDeclaration()->getName() == "mul");
  Variable* mulAcc = llvm::dyn_cast<Variable>(op->getOperand(0));
  assert(mulAcc);
  assert(mulAcc->getType() == Type::Float);
  assert(mulAcc->getName() == "acc");
  Variable* mulX = llvm::dyn_cast<Variable>(op->getOperand(1));
  assert(mulX);
  assert(mulX->getType() == Type::Float);
  assert(mulX->getName() == "x");
  cout << "    OK\n";
}

void test_pprint()
{
  cout << "\n == test_pprint\n";
  Lexer l(LOC(), "\n"
          "(def f1 Integer ((x : Integer) (y : Integer)) (add x y))\n"
          "(def f2 Float ((x : Integer) (y : Integer)) (if t (add x y) (mul x y)))");
  auto root = l.lex();
  cout << " -- Tokens\n";
  cout << root->pprint(80) << endl;
  cout << " -- Tokens\n";
  cout << root->pprint(60) << endl;
  cout << " -- Tokens\n";
  cout << root->pprint(40) << endl;
  cout << "     OK\n";
}

int test_all(int v=0) {
  verbose = v;

  test_lexer();

  test_parser_block();
  test_parser_let();
  test_parser_tuple_let();
  test_parser_decl();
  test_parser_def();
  test_parser_decl_def_use();
  test_parser_cond();
  test_parser_rule();
  test_parser_vector_type();
  test_parser_build();
  test_parser_vector();
  test_parser_tuple_type();
  test_parser_tuple();
  test_parser_get();
  test_parser_fold();
  test_pprint();

  cout << "\nAll tests OK\n";
  return 0;
}
