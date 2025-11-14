#ifndef PARSER_PARSER
#define PARSER_PARSER

#include "Lexer.hpp"
#include "mc/MCContext.hpp"
#include "mc/MCInst.hpp"
#include "mc/MCOpCode.hpp"
#include "mc/Pseudo.hpp"
#include "utils/ADT/StringMap.hpp"
#include "utils/macro.hpp"
#include <cstdint>

template <typename T> using StringSwitch = utils::ADT::StringSwitch<T>;
template <typename T, std::size_t N>
using SmallVector = utils::ADT::SmallVector<T, N>;
using mc::MCContext;
using mc::MCInst;

namespace parser {

template <typename V> using StringMap = utils::ADT::StringMap<V>;

class Parser {
private:
  mc::MCContext& ctx;
  Lexer& lexer;

  /// speed up mnemnoic class find
  StringMap<const mc::MCOpCode*> OpCacheTab;

public:
  Parser(mc::MCContext& _ctx LIFETIME_BOUND, Lexer& _lexer LIFETIME_BOUND)
      : ctx(_ctx), lexer(_lexer) {}

  void parse();

  ~Parser() = default;

private:
  Token token;
  MCInst* curInst = nullptr;
  SmallVector<std::string, 4> DirectiveStack{};

  MCContext::size_ty curTextOffset = 0;
  MCContext::size_ty curDataOffset = 0;
  MCContext::size_ty curBssOffset = 0;

  void advance();
  uint8_t RegHelper(const StringRef& reg);
  void JrBrHelper(const StringRef& label);

  /// FSM
  void ParseNewLine();
  void ParseComma();
  void ParseLParen();
  void ParseRParen();
  void ParseColon();
  void ParseIdentifier();
  void ParseInteger();
  void ParseHexInteger();
  void ParseFloat();
  void ParseModifier();

  void ParseInstruction();
  void ParsePseudo();

  void ParseRegister();
  void ParseDirective();
  void ParseLabelDef();
  void ParseString();

  /// this method assume mnemoic is valid
  const mc::MCOpCode* findOpCode(StringRef mnemonic);

  /// whether is a instruction or pseudo
  /// {l|s}{b|w|d} + jal jalr
  bool isPseudo() {
    utils_assert(token.type == TokenType::INSTRUCTION,
                 "not a asm inst or pseudo");

    auto op = token.lexeme.c_str();

    if (MnemonicContain(op) && PseudoContain(op)) {

      return StringSwitch<bool>(op)
          .BeginWith("j",
                     [&](auto&& _) {
                       auto peekTokens = lexer.peekNextTokens<2>();
                       if (peekTokens.back().type == TokenType::NEWLINE) {
                         return true;
                       } else {
                         return false;
                       }
                     })
          .Default([&](auto&& _) {
            auto peekTokens = lexer.peekNextTokens<4>();
            if (peekTokens.back().type == TokenType::NEWLINE) {
              return true;
            } else {
              return false;
            }
          });

    } else {
      return PseudoContain(op);
    }
  }
};

} // namespace parser

#endif