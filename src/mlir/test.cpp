#include <iostream>
#include "Parser.h"

using namespace Knossos::Parser;
using namespace std;

const Node::Expr parse(const string &code) {
  Lexer l(code);
  cout << " -- Tokens\n";
  const Token* tok = l.lex();
  tok->dump();
  Parser p(tok);
  cout << " -- AST\n";
  Node::Expr root = p.parse();
  root->dump();
  return root;
}

void test_lexer() {
  cout << "\n == test_lexer\n";
  Lexer l("(def f1 Integer ((x : Integer) (y : Integer)) (add@ii x y))");
  const Token* root = l.lex();
  cout << " -- Tokens\n";
  root->dump();

  // Root can have many exprs, here only one
  assert(root->isValue() == false);
  const Token* tok = root->getChild(0);
  // OpTy is not a value and has 5 sub-exprs
  assert(tok->isValue() == false);
  assert(tok->numChildren() == 5);
  // Which are:
  assert(tok->getChild(0)->getValue() == "def");
  assert(tok->getChild(1)->getValue() == "f1");
  assert(tok->getChild(2)->getValue() == "Integer");
  const Token* args = tok->getChild(3);
  const Token* arg0 = args->getChild(0);
  assert(arg0->getChild(0)->getValue() == "x");
  assert(arg0->getChild(1)->getValue() == ":");
  assert(arg0->getChild(2)->getValue() == "Integer");
  const Token* arg1 = args->getChild(1);
  assert(arg1->getChild(0)->getValue() == "y");
  assert(arg1->getChild(1)->getValue() == ":");
  assert(arg1->getChild(2)->getValue() == "Integer");
  const Token* impl = tok->getChild(4);
  assert(impl->getChild(0)->getValue() == "add@ii");
  assert(impl->getChild(1)->getValue() == "x");
  assert(impl->getChild(2)->getValue() == "y");
}

void test_parser_block() {
  cout << "\n == test_lexer\n";
  const Node::Expr tree = parse("(10.0 42 \"Hello\")");

  // Root can have many exprs, here only one
  assert(tree->opTy == Node::OpTy::Block);
  Block::Ptr root = dynamic_pointer_cast<Block>(tree);
  assert(root);
  // OpTy is Block and has 1 sub-expr
  assert(root->getOp(0)->opTy == Node::OpTy::Block);
  Block::Ptr block = dynamic_pointer_cast<Block>(root->getOp(0));
  assert(block);
  // Block has 3 literals
  assert(block->getOp(0)->opTy == Node::OpTy::Literal);
  Literal::Ptr op0 = dynamic_pointer_cast<Literal>(block->getOp(0));
  assert(op0->getName() == "10.0");
  assert(op0->getType() == Node::Type::Float);
  assert(block->getOp(1)->opTy == Node::OpTy::Literal);
  Literal::Ptr op1 = dynamic_pointer_cast<Literal>(block->getOp(1));
  assert(op1->getName() == "42");
  assert(op1->getType() == Node::Type::Integer);
  assert(block->getOp(2)->opTy == Node::OpTy::Literal);
  Literal::Ptr op2 = dynamic_pointer_cast<Literal>(block->getOp(2));
  assert(op2->getName() == "\"Hello\"");
  assert(op2->getType() == Node::Type::String);
}

void test_parser_let() {
  cout << "\n == test_parser_let\n";
  const Node::Expr tree = parse("(let (x 10) (add x 10))");

  // Root can have many exprs, here only one
  assert(tree->opTy == Node::OpTy::Block);
  Block::Ptr root = dynamic_pointer_cast<Block>(tree);
  assert(root);
  // OpTy is Let
  assert(root->getOp(0)->opTy == Node::OpTy::Let);
  Let::Ptr def = dynamic_pointer_cast<Let>(root->getOp(0));
  assert(def);
  // Let has two parts: variable definitions and expression
  assert(def->getName() == "let");
  assert(def->getType() == Node::Type::Integer);
  Variable::Ptr x = dynamic_pointer_cast<Variable>(def->getVariable());
  assert(x->getType() == Node::Type::Integer);
  Operation::Ptr expr = dynamic_pointer_cast<Operation>(def->getExpr());
  assert(expr);
  assert(expr->getName() == "add");
  assert(expr->getType() == Node::Type::Integer);
  assert(expr->getOperand(0)->getName() == "x");
  assert(expr->getOperand(0)->getType() == Node::Type::Integer);
  assert(expr->getOperand(1)->getName() == "10");
  assert(expr->getOperand(1)->getType() == Node::Type::Integer);
}

void test_parser_decl() {
  cout << "\n == test_parser_decl\n";
  const Node::Expr tree = parse("(edef fun Float (Integer String Bool))");

  // Root can have many exprs, here only one
  assert(tree->opTy == Node::OpTy::Block);
  Block::Ptr root = dynamic_pointer_cast<Block>(tree);
  assert(root);
  // OpTy is Declaration
  assert(root->getOp(0)->opTy == Node::OpTy::Declaration);
  Declaration::Ptr decl = dynamic_pointer_cast<Declaration>(root->getOp(0));
  assert(decl);
  // Declaration has 3 parts: name, return type, arg types decl
  assert(decl->getName() == "fun");
  assert(decl->getType() == Node::Type::Float);
  assert(decl->getOperandType(0) == Node::Type::Integer);
  assert(decl->getOperandType(1) == Node::Type::String);
  assert(decl->getOperandType(2) == Node::Type::Bool);
}

void test_parser_def() {
  cout << "\n == test_parser_def\n";
  const Node::Expr tree = parse("(def fun Integer ((x : Integer)) (add x 10))");

  // Root can have many exprs, here only one
  assert(tree->opTy == Node::OpTy::Block);
  Block::Ptr root = dynamic_pointer_cast<Block>(tree);
  assert(root);
  // OpTy is Definition
  assert(root->getOp(0)->opTy == Node::OpTy::Definition);
  Definition::Ptr def = dynamic_pointer_cast<Definition>(root->getOp(0));
  assert(def);
  // Definition has 4 parts: name, return type, arg types def, expr
  assert(def->getName() == "fun");
  assert(def->getType() == Node::Type::Integer);
  assert(def->numOperands() == 1);
  Variable::Ptr x = dynamic_pointer_cast<Variable>(def->getOperand(0));
  assert(x->getType() == Node::Type::Integer);
  Operation::Ptr expr = dynamic_pointer_cast<Operation>(def->getImpl());
  assert(expr);
  assert(expr->getName() == "add");
  assert(expr->getType() == Node::Type::Integer);
  assert(expr->getOperand(0)->getName() == "x");
  assert(expr->getOperand(0)->getType() == Node::Type::Integer);
  assert(expr->getOperand(1)->getName() == "10");
  assert(expr->getOperand(1)->getType() == Node::Type::Integer);
}

void test_parser_decl_def_use() {
  cout << "\n == test_parser_decl_def_use\n";
  const Node::Expr tree = parse(
      "(edef fun Integer (Integer))"
      "(def fun Integer ((x : Integer)) (add x 10))"
      "(def main Integer () (add (fun 10) 10)");

  // Root can have many exprs, here only 3
  assert(tree->opTy == Node::OpTy::Block);
  Block::Ptr root = dynamic_pointer_cast<Block>(tree);
  assert(root);
  assert(root->getOp(0)->opTy == Node::OpTy::Declaration);
  assert(root->getOp(1)->opTy == Node::OpTy::Definition);
  assert(root->getOp(2)->opTy == Node::OpTy::Definition);
  // We're interested in the main call only
  Definition::Ptr main = dynamic_pointer_cast<Definition>(root->getOp(2));
  assert(main);
  assert(main->getName() == "main");
  assert(main->getType() == Node::Type::Integer);
  assert(main->numOperands() == 0);
  // And its implementation
  assert(main->getImpl()->opTy == Node::OpTy::Operation);
  Operation::Ptr impl = dynamic_pointer_cast<Operation>(main->getImpl());
  assert(impl);
  assert(impl->getName() == "add");
  assert(impl->getType() == Node::Type::Integer);
  // Arg1 is a call to fun
  assert(impl->getOperand(0)->opTy == Node::OpTy::Operation);
  Operation::Ptr call = dynamic_pointer_cast<Operation>(impl->getOperand(0));
  assert(call);
  assert(call->getName() == "fun");
  assert(call->getType() == Node::Type::Integer);
  assert(call->getOperand(0)->opTy == Node::OpTy::Literal);
  assert(call->getOperand(0)->getName() == "10");
  assert(call->getOperand(0)->getType() == Node::Type::Integer);
  // Arg2 is just a literal
  assert(impl->getOperand(1)->opTy == Node::OpTy::Literal);
  assert(impl->getOperand(1)->getName() == "10");
  assert(impl->getOperand(1)->getType() == Node::Type::Integer);
}

void test_parser_cond() {
  cout << "\n == test_parser_cond\n";
  const Node::Expr tree = parse(
      "(edef fun Integer (Integer))"
      "(def fun Integer ((x : Integer)) (add x 10))"
      "(if (true) (fun 10) (add 10 10))");

  // Root can have many exprs, here only 3
  assert(tree->opTy == Node::OpTy::Block);
  Block::Ptr root = dynamic_pointer_cast<Block>(tree);
  assert(root);
  assert(root->getOp(0)->opTy == Node::OpTy::Declaration);
  assert(root->getOp(1)->opTy == Node::OpTy::Definition);
  assert(root->getOp(2)->opTy == Node::OpTy::Condition);
  // We're interested in the conditional only
  Condition::Ptr cond = dynamic_pointer_cast<Condition>(root->getOp(2));
  assert(cond);
  // Condition block is Bool true
  assert(cond->getCond()->opTy == Node::OpTy::Block);
  Block::Ptr c = dynamic_pointer_cast<Block>(cond->getCond());
  assert(c);
  assert(c->getOp(0)->opTy == Node::OpTy::Literal);
  assert(c->getOp(0)->getName() == "true");
  assert(c->getOp(0)->getType() == Node::Type::Bool);
  // If block is "fun" call
  assert(cond->getIfBlock()->opTy == Node::OpTy::Operation);
  Operation::Ptr call = dynamic_pointer_cast<Operation>(cond->getIfBlock());
  assert(call);
  assert(call->getName() == "fun");
  assert(call->getType() == Node::Type::Integer);
  assert(call->getOperand(0)->opTy == Node::OpTy::Literal);
  assert(call->getOperand(0)->getName() == "10");
  assert(call->getOperand(0)->getType() == Node::Type::Integer);
  // Else block is an "add" op
  Operation::Ptr expr = dynamic_pointer_cast<Operation>(cond->getElseBlock());
  assert(expr);
  assert(expr->getName() == "add");
  assert(expr->getType() == Node::Type::Integer);
  assert(expr->getOperand(0)->getName() == "10");
  assert(expr->getOperand(0)->getType() == Node::Type::Integer);
  assert(expr->getOperand(1)->getName() == "10");
  assert(expr->getOperand(1)->getType() == Node::Type::Integer);
}

int main() {
  test_lexer();
  test_parser_block();
  test_parser_let();
  test_parser_decl();
  test_parser_def();
  test_parser_decl_def_use();
  test_parser_cond();

  return 0;
}
