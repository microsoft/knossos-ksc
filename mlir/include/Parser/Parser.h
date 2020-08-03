/* Copyright Microsoft Corp. 2020 */
#ifndef _PARSER_H_
#define _PARSER_H_

#include <cassert>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>
#include <iosfwd>

#include "AST.h"
#include "llvm/ADT/StringSwitch.h"

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
  llvm::ArrayRef<Ptr> getChildren() const {
    assert(!isValue && "No children in a value token");
    return children;
  }
  llvm::StringRef getValue() const {
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

//================================================ Parse Tokens into Nodes

/// Identify each token as an AST node and build it.
/// The parser will take ownership of the Tokens.
class Parser {
  Lexer lex;
  Token::Ptr rootT;
  Block::Ptr rootE;
  Block::Ptr extraDecls;

  // TODO: Add lam
  enum class Keyword {
       LET,  EDEF, DEF,   IF, BUILD, INDEX,
      SIZE, TUPLE, GET, FOLD, RULE, NA,
  };
  Keyword isReservedWord(std::string name) const {
    return llvm::StringSwitch<Keyword>(name)
              .Case("edef", Keyword::EDEF)
              .Case("def", Keyword::DEF)
              .Case("rule", Keyword::RULE)
              .Case("let", Keyword::LET)
              .Case("if", Keyword::IF)
              .Case("build", Keyword::BUILD) // TODO: Prim not reserved word
              .Case("tuple", Keyword::TUPLE)
              .StartsWith("get$", Keyword::GET) // TODO: Prim not reserved word
              .Case("fold", Keyword::FOLD) // TODO: Prim not reserved word
              .Default(Keyword::NA);
  }
  /// Simple symbol table for parsing only (no validation)
  struct Symbols {
    Symbols(bool reassign=false) : reassign(reassign) {}
    bool exists(std::string name) {
      return symbols.find(name) != symbols.end();
    }
    void set(std::string name, Expr* val) {
      auto result = symbols.insert({name, val});
      // Already exists, replace
      if (!result.second && reassign)
        symbols[name] = val;
    }
    Expr* get(std::string name) {
      if (exists(name))
        return symbols[name];
      return nullptr;
    }
  private:
    bool reassign;
    std::map<std::string, Expr*> symbols;
  };
  Symbols variables{true};
  Symbols rules;

  std::map<Signature, Declaration*> function_decls;

  // Build AST nodes from Tokens
  Expr::Ptr parseToken(const Token *tok);
  // Specific Token parsers
  Type parseType(const Token *tok);
  Type parseRelaxedType(std::vector<const Token *> toks);
  Block::Ptr parseBlock(const Token *tok);
  Expr::Ptr parseValue(const Token *tok);  // Literal or Variable use
  Call::Ptr parseCall(const Token *tok);
  Variable::Ptr parseVariable(const Token *tok);
  Let::Ptr parseLet(const Token *tok);
  Declaration::Ptr parseDecl(const Token *tok);
  Definition::Ptr parseDef(const Token *tok);
  Rule::Ptr parseRule(const Token *tok);
  Condition::Ptr parseCond(const Token *tok);
  Build::Ptr parseBuild(const Token *tok);
  Tuple::Ptr parseTuple(const Token *tok);
  Get::Ptr parseGet(const Token *tok);
  Fold::Ptr parseFold(const Token *tok);

public:
  Parser(std::string const& filename, std::string const& code): 
      rootT(nullptr), 
      rootE(nullptr),
      extraDecls(nullptr),
      lex(filename, code) 
      {
        extraDecls = std::make_unique<Block>();
      }
  Parser(Location const& loc, std::string const& code): 
      rootT(nullptr), 
      rootE(nullptr),
      extraDecls(nullptr),
      lex(loc, code) 
      {
        extraDecls = std::make_unique<Block>();
      }

  void tokenise() {
    assert(!rootT && "Won't overwrite root token");
    rootT = lex.lex();
  }
  void parse() {
    assert(!rootE && "Won't overwrite root node");
    if (!rootT) tokenise();
    rootE = parseBlock(rootT.get());
  }
  const Token* getRootToken() {
    return rootT.get();
  }
  const Expr* getRootNode() {
    return rootE.get();
  }
  Expr::Ptr moveRoot() {
    return std::move(rootE);
  }
  const Block* getExtraDecls() {
    return extraDecls.get();
  }
  Declaration* addExtraDecl(std::string name, std::vector<Type> types, Type returnType);
};

} // namespace AST
} // namespace Knossos
#endif /// _PARSER_H_
