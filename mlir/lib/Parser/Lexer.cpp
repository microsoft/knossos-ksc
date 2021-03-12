
/* Copyright Microsoft Corp. 2020 */
#include <iostream>
#include <iomanip>

#include "Parser/Lexer.h"
#include "Parser/Assert.h"

using namespace std;

namespace Knossos { namespace AST {
std::ostream& Location::dump(std::ostream& s) const
{
  return s << *filename << ":" << line << ":" << column;
}

//================================================ Lex source into Tokens

Lexer::Lexer(Location const& loc, std::string const& code)
    : code(code)
    , len(code.size())
    , loc(loc)
    , pos(0)
    , depth(0)
    , verbosity(0)
{
}

char Lexer::get()
{
  ASSERT(pos < len) << "\n" << loc << ": Unexpected end of file";
  char c = code[pos];
  ++pos;
  loc.inc();
  if (c == '\n')
    loc.nl();
  return c;
}

char Lexer::peek(int offset)
{
  ASSERT(pos+offset < len) << "\n" << loc << ": Unexpected end of file";
  return code[pos+offset];
}

bool isValidIdentifierChar(char c)
{
    if (isalnum(c)) return true;
    // TODO: This could be much faster..
    static std::string chars("!@$%^&*{}[]:.,<>?/'|=+-_~`");
    return chars.find(c) != std::string::npos; 
}

// Lex a token out, recurse if another entry point is found
Token::Ptr Lexer::lex(char c)
{
  ++depth;
  Token* tok = new Token(loc, c);
  char expected_closing_bracket = (c == 0) ? 0 : (c == '(') ? ')' : ']';
  while (pos < len) {
    char next = peek();
    if (next == ')' || next == ']') {
      ASSERT(next == expected_closing_bracket) << "\n" 
        << loc << ": Mismatched bracket: expected " << expected_closing_bracket << ", saw " << next << std::endl;
      break;
    }
        
    Location cloc = loc;
    char c = get();
    switch (c) {
      case '"': {
        // No comments inside strings
        // Newlines allowed inside strings
        auto start = pos - 1;
        do {
            c = get();
            // Skip \"
            if (c == '\\' && peek() == '"')
              get();
        } while (c != '"');

        // Strings need to capture the quotes, too
        auto str = code.substr(start, pos - start);
        auto child = make_unique<Token>(cloc, str);
        tok->addChild(move(child));
        break;
    }

    case ';': {
        // Comment, to the end of the line
        while (get() != '\n');
        break;
    }

    case '#':
        if (peek() == '|') {
            // Multi-line comment
            int commentDepth = 1;
            do {
                c = get();
                if (c == '#' && peek() == '|')
                ++commentDepth;
                if (c == '|' && peek() == '#')
                --commentDepth;
            } while (commentDepth > 0);
            c = get();
            assert(c == '#');
        }
        break;

    case '[':
    case '(': {
      // Recurse into sub-tokens
      tok->addChild(lex(c));
      c = get();
      break;
    }

    case '\n':
    case '\t':
    case ' ':
    case '\xc2': // TODO other non-printing spaces?
    case '\r':
      break;

    default:
      if (isValidIdentifierChar(c)) {
        auto start = pos-1;
        while (isValidIdentifierChar(peek()))
          get();
        auto t = make_unique<Token>(cloc, code.substr(start, pos - start));
        tok->addChild(move(t));
      }
      else
        ASSERT(0) << "\n" << cloc << ": Unhandled character [" << c << "], code " << (0xff & c) 
                  << " 0x" << setw(2) << setfill('0') << hex << (0xff & c);
        // Should restore stream flags, but exiting anyway.  Wow, ostream formatting...
    }
  }
  if (verbosity+1 > depth) tok->dump(std::cerr << std::string(depth, '-'), depth) << "\n";
  --depth;
  return Token::Ptr(tok);
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
  ret.s += tok->getOpeningBracket();
  for (auto &s : strs) {
    if (!first)
      ret.s += sep;
    ret.s += s;
    first = false;
  }
  ret.s += tok->getClosingBracket();

  return ret;
}

std::ostream& Token::dump(std::ostream& s, size_t indent) const {
  return s << pprint(this, indent, 80).s;
}

}} // namespace
