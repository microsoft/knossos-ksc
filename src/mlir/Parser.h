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
class Token {
  std::string value;
  std::vector<Token*> children;

public:
  Token(std::string str) : value(str) { }
  Token() { }
  ~Token() { for (auto c: children) delete c; }

  size_t numChildren() const { return children.size(); }
  void addChild(Token* &&tok) {
    assert(value.empty() && "Can't add children to values");
    children.push_back(std::move(tok));
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
};

// Tokenise the text into recursive tokens grouped by parenthesis.
class Lexer {
  std::string code;
  size_t len;
  Token* root;

  // Build a tree of tokens
  size_t lexToken(Token* &tok, size_t index);

public:
  Lexer(std::string &code) : code(code), len(code.size()), root(new Token()) {
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
  Token root;
  AST::AST ast;

  // Build AST nodes from Tokens
  void parseToken(Token &tok);

public:
  Parser(Token root) : root(root) { }

  void parse() {
    parseToken(root);
  }
};

} // namespace Parser
} // namespace Knossos
#endif // _PARSER_H_
