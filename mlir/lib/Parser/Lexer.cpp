
/* Copyright Microsoft Corp. 2020 */
#include <iostream>

#include "Parser/Lexer.h"

using namespace std;
using namespace Knossos::AST;

std::ostream& Location::dump(std::ostream& s) const
{
  return s << *filename << ":" << line << ":" << column;
}

//================================================ Lex source into Tokens

Lexer::Lexer(std::string const& filename, std::string const& code)
    : code(code)
    , len(code.size())
    , loc {filename, 1, 0 } 
    , root(new Token(loc))
    , multiLineComments(0)
{
  assert(len > 0 && "Empty code?");
}

Lexer::Lexer(Location const& loc, std::string const& code)
    : code(code)
    , len(code.size())
    , loc(loc)
    , root(new Token(loc))
    , multiLineComments(0)
{
  assert(len > 0 && "Empty code?");
}

// Lex a token out, recurse if another entry point is found
size_t Lexer::lexToken(Token *tok, size_t pos) {
  const char TAB = 0x09;
  const char SPC = 0x20;
  size_t tokenStart = pos;
  bool isInString = false;
  while (pos < len) {
    switch (code[pos]) {
    case ';':
      // Comment, to the end of the line
      while (code[pos] != '\n')
        tokenStart = ++pos;
      break;
    case '#':
      if (pos+1 < len && code[pos+1] != '|')
        break;
      assert(multiLineComments == 0);
      pos += 2; // consume #|
      multiLineComments = 1;
      // Multi-line comment
      while (multiLineComments) {
        switch (code[pos]) {
        case '|':
          if (pos+1 < len && code[pos+1] == '#') {
            multiLineComments--;
            pos++;
          }
          pos++;
          break;
        case '#':
          if (code[pos+1] == '|') {
            multiLineComments++;
            pos++;
          }
          pos++;
          break;
        case '\n':
          loc.nl();
        default:
          pos++;
          loc.inc();
        }
      }
      tokenStart = pos;
      break;
    case '\n':
      loc.nl();
    case TAB:
    case SPC:
    case '\xc2': // TODO other non-printing spaces?
    case '\r':
      // "Whitespace" is allowed inside strings
      if (isInString) {
        pos++;
        break;
      }
      // Maybe end of a value
      if (tokenStart != pos) {
        tok->addChild(
            make_unique<Token>(loc, code.substr(tokenStart, pos - tokenStart)));
      }
      // Or end of a token, which we ignore
      tokenStart = ++pos;
      break;
    case ')':
      // Maybe end of a value
      if (tokenStart != pos) {
        tok->addChild(
            make_unique<Token>(loc, code.substr(tokenStart, pos - tokenStart)));
      }
      // Finished parsing this token
      return ++pos;
    case '(': {
      // Recurse into sub-tokens
      auto t = make_unique<Token>(loc);
      tokenStart = pos = lexToken(t.get(), pos + 1);
      tok->addChild(move(t));
      break;
    }
    case '"':
      if (isInString) {
        // Strings need to capture the quotes, too
        size_t start = tokenStart - 1;
        size_t length = (pos - start + 1);
        tok->addChild(
            make_unique<Token>(loc, code.substr(start, length)));
      }
      tokenStart = ++pos;
      isInString = !isInString;
      break;
    default:
      // These are text, so we keep reading
      pos++;
    }
  }
  return pos;
}


//================================================ Dumps tokens, nodes to stdout

Token::ppresult Token::pprint(Token const* tok, int indent, int width)
{
  const int tab = 2;

  if (tok->isValue)
    return {tok->getValue().data(), tok->getValue().size()};

  int mywidth = 1; // for parens
  int maxwidth = 0;
  bool first = true;
  std::vector<std::string> strs;
  for (auto& t : tok->children) {
    ppresult p = pprint(t.get(), indent+tab, width);
    if (p.width > maxwidth)
      maxwidth = p.width;
    mywidth += (first ? 0 : 1) + p.width;
    strs.push_back(p.s);
    first = false;
  }

  int available_width = width - indent;
  ppresult ret;
  std::string sep;
  if (mywidth < available_width) {
    sep = " ";
    ret.width = mywidth;
  } else {
    sep = "\n" + std::string(indent, ' ');
    ret.width = maxwidth;
  }

  first = true;
  ret.s = "(";
  for (auto &s : strs) {
    if (!first)
      ret.s += sep;
    ret.s += s;
    first = false;
  }
  ret.s += ")";

  return ret;
}

std::ostream& Token::dump(std::ostream& s) const {
  return s << pprint(this, 0, 80).s;
}

}} // namespace
