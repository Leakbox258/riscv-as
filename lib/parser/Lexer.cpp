#include "parser/Lexer.hpp"
#include "mc/MCOpCode.hpp"
#include "mc/MCOperand.hpp"
#include "mc/Pseudo.hpp"
#include "utils/likehood.hpp"
#include <algorithm>
#include <cctype>
#include <cstddef>
#include <string>

namespace parser {
std::string to_string(TokenType type) {
  switch (type) {
  case TokenType::UNKNOWN:
    return "UNKNOWN";
  case TokenType::END_OF_FILE:
    return "END_OF_FILE";
  case TokenType::NEWLINE:
    return "NEWLINE";
  case TokenType::IDENTIFIER:
    return "IDENTIFIER";
  case TokenType::INTEGER:
    return "INTEGER";
  case TokenType::HEX_INTEGER:
    return "HEX_INTEGER";
  case TokenType::FLOAT:
    return "FLOAT";
  case TokenType::MODIFIERS:
    return "MODIFERS";
  case TokenType::EXPR_OPERATOR:
    return "EXPR_OPERATOR";
  case TokenType::INSTRUCTION:
    return "INSTRUCTION";
  case TokenType::REGISTER:
    return "REGISTER";
  case TokenType::MODE:
    return "MODE";
  case TokenType::DIRECTIVE:
    return "DIRECTIVE";
  case TokenType::LABEL_DEFINITION:
    return "LABEL_DEFINITION";
  case TokenType::COMMA:
    return "COMMA";
  case TokenType::LPAREN:
    return "LPAREN";
  case TokenType::RPAREN:
    return "RPAREN";
  case TokenType::COLON:
    return "COLON";
  case TokenType::STRING_LITERAL:
    return "STRING_LITERAL";
  default:
    return "TYPE_ERROR";
  }
}
} // namespace parser

using namespace parser;

void Token::print() const {
  std::cout << "Token(" << to_string(type) << ", lexeme: '" << lexeme << "', "
            << "line: " << loc.line << ", col: " << loc.col << ")\n";
}

Lexer::Lexer(StringRef source) : m_source(source) {}

bool Lexer::isAtEnd() const { return m_cursor >= m_source.size(); }

char Lexer::advance() {
  if (!isAtEnd()) {
    m_col++;
    return m_source[m_cursor++];
  }
  return '\0';
}

char Lexer::peek() const {
  if (isAtEnd())
    return '\0';
  return m_source[m_cursor];
}

char Lexer::peekNext() const {
  if (m_cursor + 1 >= m_source.size())
    return '\0';
  return m_source[m_cursor + 1];
}

void Lexer::skipWhitespaceAndComments() {
  while (!isAtEnd()) {
    char c = peek();
    switch (c) {
    case ' ':
    case '\r':
    case '\t':
      advance();
      break;
    case '#': // Comment goes to the end of the line
      while (peek() != '\n' && !isAtEnd()) {
        advance();
      }
      break;
    default:
      return;
    }
  }
}

const Token& Lexer::makeToken(TokenType type) {
  lastToken =
      Token{type, m_source.slice(m_cursor - 1, 1).str(), {m_line, m_col - 1}};
  // For single-character tokens
  return lastToken;
}

const Token& Lexer::makeToken(TokenType type, StringRef lexeme) {
  lastToken = Token{type, lexeme.str(), {m_line, m_col - lexeme.size()}};

  return lastToken;
}

const Token& Lexer::scanIdentifier() {
  size_t start = m_cursor - 1;
  while (isalnum(peek()) || peek() == '_' || peek() == '.') {
    advance();
  }

  std::string lexeme = m_source.slice(start, m_cursor).str();

  /// lowercase: label, instruction, register, identifier
  std::transform(lexeme.begin(), lexeme.end(), lexeme.begin(),
                 [](unsigned char c) { return std::tolower(c); });

  // Check if it's a label definition
  if (peek() == ':') {
    advance(); // consume the ':'
    return makeToken(TokenType::LABEL_DEFINITION,
                     m_source.slice(start, m_cursor));
  }

  if (MnemonicContain(lexeme.c_str()) || PseudoContain(lexeme.c_str()) ||
      lexeme == "li") {
    return makeToken(TokenType::INSTRUCTION, lexeme);
  }

  /// TODO: more modes to be recognizable
  if (mc::RoundModes.find(lexeme.c_str())) {
    return makeToken(TokenType::MODE, lexeme);
  }

  if (mc::Registers.find(lexeme)) {
    return makeToken(TokenType::REGISTER, lexeme);
  }

  if (lexeme[0] != '.') {
    return makeToken(TokenType::IDENTIFIER, m_source.slice(start, m_cursor));
  }

  if (lastToken.type == TokenType::NEWLINE || // last
      lastToken.type == TokenType::UNKNOWN) { // very beginning
    return makeToken(TokenType::DIRECTIVE, m_source.slice(start, m_cursor));
  } else {
    return makeToken(TokenType::IDENTIFIER, m_source.slice(start, m_cursor));
  }
}

const Token& Lexer::scanNumber() {
  size_t start = m_cursor - 1;
  // Check for hexadecimal
  if (m_source[start] == '0' && (peek() == 'x' || peek() == 'X')) {
    advance(); // consume 'x'
    while (isxdigit(peek())) {
      advance();
    }
    return makeToken(TokenType::HEX_INTEGER, m_source.slice(start, m_cursor));
  }

  // Handle negative numbers at the start
  if (m_source[start] == '-') {
    if (!isdigit(peek())) {
      return makeToken(TokenType::UNKNOWN, m_source.slice(start, 1));
    }
  }

  // Decimal
  bool dot = false;
  while (isdigit(peek())) {

    advance();

    if (utils::is_unlikely(peek() == '.')) {
      dot = true;
      advance(); // skip '.'
    }
  }

  return makeToken(dot ? TokenType::FLOAT : TokenType::INTEGER,
                   m_source.slice(start, m_cursor));
}

const Token& Lexer::scanString() {
  size_t start = m_cursor; // Start after the opening quote
  while (peek() != '"' && !isAtEnd()) {
    if (peek() == '\n') { // Unterminated string
      m_line++;
      m_col = 1;
    }
    advance();
  }

  if (isAtEnd()) {
    return makeToken(TokenType::UNKNOWN, "Unterminated string");
  }

  advance(); // Consume the closing quote
  StringRef lexeme = m_source.slice(start, m_cursor - 1);
  return makeToken(TokenType::STRING_LITERAL, lexeme);
}

const Token& Lexer::nextToken() {
  skipWhitespaceAndComments();

  if (isAtEnd()) {
    lastToken = Token{TokenType::END_OF_FILE, "", {m_line, m_col}};
    return lastToken;
  }

  char c = advance();

  if (c == '%') {
    size_t start = m_cursor - 1;
    while (peek() != '(') {
      advance();
    }
    return makeToken(TokenType::MODIFIERS, m_source.slice(start, m_cursor));
  }

  if (isalpha(c) || c == '_' || c == '.') {
    return scanIdentifier();
  }

  if (isdigit(c) || (c == '-' && isdigit(peek()))) {
    return scanNumber();
  }

  switch (c) {
  case '\n': {
    makeToken(TokenType::NEWLINE);
    m_line++;
    m_col = 1;
    return lastToken;
  }
  case ',':
    return makeToken(TokenType::COMMA);
  case '(':
    return makeToken(TokenType::LPAREN);
  case ')':
    return makeToken(TokenType::RPAREN);
  case '"':
    return scanString();
  case ':':
    return makeToken(TokenType::COLON); // e.g. .section .text :
  case '+':
  case '-':
    return makeToken(TokenType::EXPR_OPERATOR);
  }

  return makeToken(TokenType::UNKNOWN, m_source.slice(m_cursor - 1, 1));
}