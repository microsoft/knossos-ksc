/* Copyright Microsoft Corp. 2020 */
#ifndef _KSC_AST_PARSER_H_
#define _KSC_AST_PARSER_H_

#include <cassert>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>
#include <iosfwd>

#include "llvm/ADT/StringSwitch.h"

#include "AST.h"
#include "Lexer.h"

namespace Knossos { namespace AST {

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
      DEF, EDEF, GDEF, RULE,
      LET, IF, TUPLE, GET,
      BUILD, FOLD, NA,
  };
  Keyword isReservedWord(std::string name) const {
    return llvm::StringSwitch<Keyword>(name)
              .Case("def", Keyword::DEF)
              .Case("edef", Keyword::EDEF)
              .Case("gdef", Keyword::GDEF)
              .Case("rule", Keyword::RULE)
              .Case("let", Keyword::LET)
              .Case("if", Keyword::IF)
              .Case("tuple", Keyword::TUPLE)
              .StartsWith("get$", Keyword::GET) // TODO: Prim not reserved word
              .Case("build", Keyword::BUILD) // TODO: Prim not reserved word
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

  StructuredName parseStructuredName(const Token* tok);
  StructuredName parseStructuredNameWithType(const Token* tok);

  Block::Ptr parseBlock(const Token *tok);
  Expr::Ptr parseValue(const Token *tok);  // Literal or Variable use
  Call::Ptr parseCall(const Token *tok);
  Variable::Ptr parseVariableWithType(const Token *tok);
  Binding parseBinding(const Token *tok);
  Let::Ptr parseLet(const Token *tok);
  Declaration::Ptr parseDecl(const Token *tok);
  Definition::Ptr parseDef(const Token *tok);
  Rule::Ptr parseRule(const Token *tok);
  Condition::Ptr parseCond(const Token *tok);
  Lambda::Ptr parseLambda(const Token *tok);
  Build::Ptr parseBuild(const Token *tok);
  Tuple::Ptr parseTuple(const Token *tok);
  Get::Ptr parseGet(const Token *tok);
  Fold::Ptr parseFold(const Token *tok);

public:
  Parser(Location const& loc, std::string const& code, int verbosity): 
      lex(loc, code),
      rootT(nullptr), 
      rootE(nullptr),
      extraDecls(std::make_unique<Block>())
      {
        lex.setVerbosity(verbosity);
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
  Declaration* addExtraDecl(StructuredName const& name, Type argType, Type returnType);
};

} // namespace AST
} // namespace Knossos
#endif /// _PARSER_H_
