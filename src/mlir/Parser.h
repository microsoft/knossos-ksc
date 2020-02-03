/* Copyright Microsoft Corp. 2020 */
#ifndef _PARSER_H_
#define _PARSER_H_

#include <vector>
#include <string>
#include <memory>
#include <cassert>

#include "AST.h"

namespace Knossos {
namespace Parser {

// A token that has either value or children
// Values are literals, variables, names, reserved words, types
// Non-Values are lets, def/decl, ops, calls, control flow
//
// Do not confuse with "continuation values", those are higher level.
//
// Using raw pointers instead of shared because I think it should be unique
// but the ownership model can be a little confusing, so making it simple
// for now and deleting the pointers on exit.
class Token {
  std::string value;
  std::vector<Token*> children;

public:
  Token(std::string str) : value(str) { }
  Token() { }
  ~Token() { for (auto c: children) delete c; }

  size_t numChildren() const { return children.size(); }
  void addChild(Token* tok) {
    assert(value.empty() && "Can't add children to values");
    children.push_back(tok);
  }
  const Token* getChild(size_t i) const {
    assert(value.empty() && "Value tokens don't have children");
    assert(i < children.size() && "Offset error");
    return children[i];
  }

  bool isValue() const { return !value.empty() && children.size() == 0; }
  const std::string& getValue() const {
    assert(!value.empty() && "Not a value token");
    return value;
  }

  void dump(size_t tab=0) const;
};

// Tokenise the text into recursive tokens grouped by parenthesis.
class Lexer {
  std::string code;
  size_t len;
  Token* root;

  // Build a tree of tokens
  size_t lexToken(Token* tok, size_t index);

public:
  Lexer(const std::string &code) : code(code), len(code.size()), root(new Token()) {
    assert(len > 0 && "Empty code?");
  }
  ~Lexer() { if (root) delete root; }

  const Token* lex() {
    lexToken(root, 0);
    return root;
  }
};

// Identify each token as an AST node and build it.
class Parser {
  const Token* tok;
  Node::Expr root;

  // Build AST nodes from Tokens
  Node::Expr parseToken(const Token *tok);
  Node::Expr parseValue(const Token *tok);
  Node::Expr parseCall(const Token *tok);
  Node::Expr parseOperation(const Token *tok);
  Node::Expr parseLet(const Token* bond, const Token* expr);
  Node::Expr parseDecl(const Token* name, const Token* type, const Token* args);
  Node::Expr parseDef(const Token* name, const Token* type,
                      const Token* args, const Token* expr);
  Node::Expr parseCond(const Token *cond, const Token *ifBlock,
                       const Token *args);

public:
  Parser(const Token* tok) : tok(tok) { }

  const Node::Expr parse();
};

} // namespace Parser
} // namespace Knossos
#endif // _PARSER_H_
