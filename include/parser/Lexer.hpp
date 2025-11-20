#ifndef PARSER_LEXER
#define PARSER_LEXER

#include "mc/MCInst.hpp"
#include "mc/MCOpCode.hpp"
#include "utils/ADT/StringRef.hpp"
#include <cstddef>
#include <string>

namespace parser {
using StringRef = utils::ADT::StringRef;

enum class TokenType {
  UNKNOWN,
  END_OF_FILE,
  NEWLINE,

  IDENTIFIER,  // e.g., my_label (.global) my_label
  INTEGER,     // e.g., 123, -42
  HEX_INTEGER, // e.g., 0xdeadbeef
  FLOAT,       // e.g., 1.5

  MODIFIERS,     // %lo, %hi
  EXPR_OPERATOR, // + / -

  INSTRUCTION, // e.g., addi, lw, sd
  REGISTER,    // e.g., a0, sp, x10
  MODE,        // rtz, rwio...

  DIRECTIVE,        // e.g., .global, .section
  LABEL_DEFINITION, // e.g., loop:, main:

  COMMA,  // ,
  LPAREN, // (
  RPAREN, // )
  COLON,  // :

  STRING_LITERAL // "..."
};

std::string to_string(TokenType type);

struct Token {
  TokenType type;
  std::string lexeme;
  mc::Location loc;

  Token() { this->type = TokenType::UNKNOWN; }

  Token(TokenType _type, std::string _lex, mc::Location _loc)
      : type(_type), lexeme(std::move(_lex)), loc(_loc) {}

  Token(const Token& token) {
    this->type = token.type;
    this->lexeme = token.lexeme;
    this->loc = token.loc;
  }

  Token& operator=(const Token& token) {
    this->type = token.type;
    this->lexeme = token.lexeme;
    this->loc = token.loc;
    return *this;
  }

  void print() const;
};

class Lexer {
public:
  Lexer(StringRef source);

  const Token& nextToken();

  template <std::size_t N> SmallVector<Token, N> peekNextTokens() {
    SmallVector<Token, N> tokens{};

    auto s_m_cursor = m_cursor;
    auto s_m_line = m_line;
    auto s_m_col = m_col;

    for (auto i = 0ull; i < N; ++i) {
      tokens.emplace_back(this->nextToken());
    }

    m_cursor = s_m_cursor;
    m_line = s_m_line;
    m_col = s_m_col;

    return tokens;
  }

private:
  StringRef m_source;

  /// location infomation
  std::size_t m_cursor = 0;
  std::size_t m_line = 1;
  std::size_t m_col = 1;

  bool isAtEnd() const;
  char advance();
  char peek() const;
  char peekNext() const;

  Token lastToken{};

  void skipWhitespaceAndComments();
  const Token& makeToken(TokenType type);
  const Token& makeToken(TokenType type, StringRef lexeme);
  const Token& errorToken(const char* message) const;

  const Token& scanIdentifier();
  const Token& scanNumber();
  const Token& scanString();
};
} // namespace parser
#endif