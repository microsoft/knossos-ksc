/* Copyright Microsoft Corp. 2020 */
#ifndef _KSC_AST_LEXER_H_
#define _KSC_AST_LEXER_H_

#include <cassert>
#include <memory>
#include <string>
#include <vector>

#include "llvm/ADT/ArrayRef.h"

namespace Knossos {
namespace AST {
  
/// Code location.
struct Location {
  std::shared_ptr<std::string> filename;
  size_t line;
  size_t column;

  Location(const std::string& s, size_t line, size_t column):
    filename(std::make_shared<std::string>(s)),
    line(line),
    column(column)
  {}

  void nl() {
    ++line;
    column = 1;
  }
  void inc() {
    ++column;
  }

  std::ostream& dump(std::ostream&) const;
};

inline std::ostream& operator<<(std::ostream& s, Location const& loc) { return loc.dump(s); }

//================================================ Tokeniser / Lexer

/// A token that has either value or children
/// Values are literals, variables, names, reserved words, types
/// Non-Values are lets, def/decl, ops, calls, control flow
///
/// Do not confuse with "continuation values", those are higher level.
struct Token {
  using Ptr = std::unique_ptr<Token>;
  Token(Location loc, std::string str) : isValue(true), value(str), loc(loc) {}
  Token(Location loc) : isValue(false), loc(loc) {}

  const bool isValue;

  void addChild(Token::Ptr tok) {
    assert(!isValue && "Can't add children to values");
    children.push_back(std::move(tok));
  }
  std::vector<Ptr> const& getChildren() const {
    assert(!isValue && "No children in a value token");
    return children;
  }
  std::string const& getValue() const {
    assert(isValue && "Not a value token");
    return value;
  }
  const Token *getChild(size_t idx) const {
    assert(!isValue && "No children in a value token");
    assert(idx < children.size() && "Offset error");
    return children[idx].get();
  }

  const Token *getHead() const {
    assert(!isValue && "No children in a value token");
    assert(children.size() > 0 && "No head");
    return children[0].get();
  }

  llvm::ArrayRef<Ptr> getTail() const {
    assert(!isValue && "No children in a value token");
    assert(children.size() > 0 && "No tail");
    return llvm::ArrayRef<Ptr>(children).slice(1);
  }

  Location const& getLocation() const  { return loc; }

  size_t size() const { return children.size(); }

  std::ostream& dump(std::ostream& s) const;

  std::string pprint(int width = 80) const;


private:
  std::string value;
  std::vector<Ptr> children;
  Location loc;

  // Pretty printing
  struct ppresult {
    std::string s;
    size_t width;
  };

  static ppresult pprint(Token const* tok, int indent = 0, int width = 80);
};

inline std::ostream& operator<<(std::ostream& s, Token const* tok) 
{
  return tok->dump(s);
}

inline std::string Token::pprint(int width) const
{
  return pprint(this, 0, width).s;
}

/// Tokenise the text into recursive tokens grouped by parenthesis.
///
/// The Lexer will pass the ownership of the Tokens to the Parser.
class Lexer {
  std::string code;
  size_t len;
  Location loc;
  Token::Ptr root; // TODO move to lex()
  size_t multiLineComments;

  /// Build a tree of tokens
  size_t lexToken(Token *tok, size_t pos);

public:
  Lexer(std::string const& filename, std::string const& code);
  Lexer(Location const& loc, std::string const& code);

  Token::Ptr lex() {
    lexToken(root.get(), 0);
    assert(multiLineComments == 0);
    return std::move(root);
  }
};

#endif
