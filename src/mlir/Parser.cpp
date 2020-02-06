#include "Parser.h"

using namespace Knossos::Parser;
using namespace std;

// Lex a token out, recurse if another entry point is found
size_t Lexer::lexToken(Token *&tok, size_t index) {
  size_t tokenStart = index;
  while (index < len) {
    switch (code[index]) {
    case ' ':
      // Maybe end of a value
      if (tokenStart != index) {
        tok->addChild(new Token(code.substr(tokenStart, index - tokenStart)));
      }
      // Or end of a token, which we ignore
      tokenStart = ++index;
      break;
    case ')':
      // Maybe end of a value
      if (tokenStart != index) {
        tok->addChild(new Token(code.substr(tokenStart, index - tokenStart)));
      }
      // Finished parsing this token
      return ++index;
    case '(': {
      // Recurse into sub-tokens
      Token *t = new Token();
      tokenStart = index = lexToken(t, index + 1);
      tok->addChild(move(t));
      break;
    }
    default: {
      // These are text, so we keep reading
      index++;
      break;
    }
    }
  }
  return index;
}

