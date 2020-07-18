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

//================================================ Tokeniser / Lexer

/// A token that has either value or children
/// Values are literals, variables, names, reserved words, types
/// Non-Values are lets, def/decl, ops, calls, control flow
///
/// Do not confuse with "continuation values", those are higher level.
struct Token {
  using Ptr = std::unique_ptr<Token>;
  Token(std::string str) : isValue(true), value(str) {}
  Token() : isValue(false) {}

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

  const bool isValue;
  size_t size() const { return children.size(); }

  std::ostream& dump(std::ostream& s, size_t tab = 0) const;

private:
  std::string value;
  std::vector<Ptr> children;
};

inline std::ostream& operator<<(std::ostream& s, Token const* tok) 
{
  return tok->dump(s);
}

/// Tokenise the text into recursive tokens grouped by parenthesis.
///
/// The Lexer will pass the ownership of the Tokens to the Parser.
class Lexer {
  std::string code;
  size_t len;
  Token::Ptr root;
  size_t multiLineComments;

  /// Build a tree of tokens
  size_t lexToken(Token *tok, size_t pos);

public:
  Lexer(std::string &&code)
      : code(code), len(code.size()), root(new Token()), multiLineComments(0) {
    assert(len > 0 && "Empty code?");
  }

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
  Token::Ptr rootT;
  Expr::Ptr rootE;
  Lexer lex;

  // Lookup table of reserved operations and keywords
  Operation::Opcode isReservedOp(llvm::StringRef name) const {
    return llvm::StringSwitch<Operation::Opcode>(name)
              .Case("add", Operation::Opcode::ADD)
              .Case("sub", Operation::Opcode::SUB)
              .Case("mul", Operation::Opcode::MUL)
              .Case("div", Operation::Opcode::DIV)
              .Case("and", Operation::Opcode::AND)
              .Case("or", Operation::Opcode::OR)
              .Case("eq", Operation::Opcode::EQ)
              .Case("ne", Operation::Opcode::NE)
              .Case("lte", Operation::Opcode::LTE)
              .Case("gte", Operation::Opcode::GTE)
              .Case("lt", Operation::Opcode::LT)
              .Case("gt", Operation::Opcode::GT)
              .Case("abs", Operation::Opcode::ABS)
              .Case("neg", Operation::Opcode::NEG)
              .Case("exp", Operation::Opcode::EXP)
              .Case("log", Operation::Opcode::LOG)
              .Case("to_float", Operation::Opcode::TOF)
              .Case("to_int", Operation::Opcode::TOI)
              .Default(Operation::Opcode::MAYBE_CALL);
  }
  enum class Keyword {
       LET,  EDEF, DEF,   IF, BUILD, INDEX,
      SIZE, TUPLE, GET, FOLD, PRINT,  RULE, NA,
      // Stdlib hack
      SUM
  };
  Keyword isReservedWord(std::string name) const {
    return llvm::StringSwitch<Keyword>(name)
              .Case("let", Keyword::LET)
              .Case("edef", Keyword::EDEF)
              .Case("def", Keyword::DEF)
              .Case("if", Keyword::IF)
              .Case("build", Keyword::BUILD) // TODO: Prim not reserved word
              .Case("tuple", Keyword::TUPLE)
              .StartsWith("get$", Keyword::GET)
              .Case("fold", Keyword::FOLD)
              .Case("print", Keyword::PRINT)
              .Case("rule", Keyword::RULE)
              // Stdlib hack
              .Case("sum", Keyword::SUM) // TODO: Prim not reserved word
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
  Symbols functions;
  Symbols variables{true};
  Symbols rules;

  // Build AST nodes from Tokens
  Expr::Ptr parseToken(const Token *tok);
  // Specific Token parsers
  Type parseType(const Token *tok);
  Type parseRelaxedType(std::vector<const Token *> toks);
  Expr::Ptr parseBlock(const Token *tok);
  Expr::Ptr parseValue(const Token *tok);
  Expr::Ptr parseCall(const Token *tok);
  Expr::Ptr parseOperation(const Token *tok, Operation::Opcode op);
  Expr::Ptr parseVariable(const Token *tok);
  Expr::Ptr parseLet(const Token *tok);
  Expr::Ptr parseDecl(const Token *tok);
  Expr::Ptr parseDef(const Token *tok);
  Expr::Ptr parseCond(const Token *tok);
  Expr::Ptr parseBuild(const Token *tok);
  Expr::Ptr parseTuple(const Token *tok);
  Expr::Ptr parseGet(const Token *tok);
  Expr::Ptr parseFold(const Token *tok);
  Expr::Ptr parsePrint(const Token *tok);
  Expr::Ptr parseRule(const Token *tok);
  // Standard library parsers
  Expr::Ptr parseSum(const Token *tok);

public:
  Parser(std::string code)
      : rootT(nullptr), rootE(nullptr), lex(std::move(code)) {}

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
};

} // namespace AST
} // namespace Knossos
#endif /// _PARSER_H_
