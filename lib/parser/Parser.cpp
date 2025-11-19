#include "parser/Parser.hpp"
#include "mc/MCContext.hpp"
#include "mc/MCExpr.hpp"
#include "mc/MCInst.hpp"
#include "mc/MCOpCode.hpp"
#include "mc/MCOperand.hpp"
#include "mc/Pseudo.hpp"
#include "parser/Lexer.hpp"
#include "utils/ADT/SmallVector.hpp"
#include "utils/ADT/StringSwitch.hpp"
#include "utils/logger.hpp"
#include "utils/macro.hpp"
#include "utils/misc.hpp"
#include <algorithm>
#include <cstdint>
#include <string>
#include <sys/types.h>

using namespace parser;
using namespace mc;

void Parser::advance() { token = this->lexer.nextToken(); }

uint8_t Parser::RegHelper(const StringRef& reg) {

  const unsigned char* find_reg;
  if (curInst) {
    find_reg =
        curInst->isCompressed() ? CRegisters.find(reg) : Registers.find(reg);
  } else {
    /// Pseudo
    find_reg = Registers.find(reg);
  }

  utils_assert(find_reg, "invalid register literal");
  return *find_reg;
}

void Parser::JrBrHelper(const StringRef& label) {
  ctx.addReloInst(curInst, label.str());

  if (curInst->isBranch()) {
    curInst->addOperand(MCOperand::makeExpr(
        ctx.getTextExpr(label, MCExpr::ExprTy::kBRANCH, 0)));
  } else if (curInst->isJmp()) {
    curInst->addOperand(
        MCOperand::makeExpr(ctx.getTextExpr(label, MCExpr::ExprTy::kJAL, 0)));
  } else {
    utils::unreachable("not a jr or br inst");
  }
}

void Parser::parse() {

  advance();

  while (token.type != TokenType::END_OF_FILE) {
    utils_assert(token.type != TokenType::UNKNOWN, "Lexer: error");

    switch (token.type) {
    case TokenType::NEWLINE:
      ParseNewLine();
      break;
    case TokenType::COMMA:
      ParseComma();
      break;
    case TokenType::LPAREN:
      ParseLParen();
      break;
    case TokenType::RPAREN:
      ParseRParen();
      break;
    case TokenType::COLON:
      ParseColon();
      break;
    case TokenType::IDENTIFIER:
      ParseIdentifier();
      break;
    case TokenType::INTEGER:
      ParseInteger();
      break;
    case TokenType::HEX_INTEGER:
      ParseHexInteger();
      break;
    case TokenType::FLOAT:
      ParseFloat();
      break;
    case TokenType::MODIFIERS:
      ParseModifier();
      break;
    case TokenType::INSTRUCTION:
      if (token.lexeme == "li") {
        ParseLi();
      } else if (isPseudo()) {
        ParsePseudo();
      } else {
        ParseInstruction();
      }
      break;
    case TokenType::REGISTER:
      ParseRegister();
      break;
    case TokenType::DIRECTIVE:
      ParseDirective();
      break;
    case TokenType::LABEL_DEFINITION:
      ParseLabelDef();
      break;
    case TokenType::STRING_LITERAL:
      ParseString();
      break;
    default:
      utils::unreachable("unkwnow type of current token");
    }
  }

  if (curInst) { /* when file dosen't end with '\n' */
    ctx.commitTextInst();
  }
}

const mc::MCOpCode* Parser::findOpCode(StringRef mnemonic) {
  auto lookup = OpCacheTab.find(mnemonic);

  if (lookup != nullptr) {
    utils_assert(*lookup, "CacheLookUpTab contains invalid key");
    return *lookup;
  }

  mc::MCOpCode const* op = MnemonicFind(mnemonic.c_str());
  OpCacheTab.insert(mnemonic, op);

  return op;
}

void Parser::ParseNewLine() {
  if (curInst) {
    curTextOffset = ctx.commitTextInst();
    curInst = nullptr;
  }
  advance();
}

void Parser::ParseComma() {
  utils_assert(curInst, "encounter dangling comma");
  advance();
}

void Parser::ParseLParen() {
  utils_assert(curInst, "encounter dangling left paren");
  advance();
  utils_assert(token.type == TokenType::REGISTER, "parse as an expr failed");
  curInst->addOperand(MCOperand::makeReg(RegHelper(token.lexeme)));
  advance();
  utils_assert(token.type == TokenType::RPAREN, "expecting right paren");
  advance();
}

void Parser::ParseRParen() {
  utils::unreachable("encounter dangling right paren");
}

void Parser::ParseColon() { advance(); }

void Parser::ParseIdentifier() {
  if (curInst) {
    /// as dest of jr/br
    JrBrHelper(token.lexeme);
  } else {
    /// check if is marked as global
    using Ndx = MCContext::NdxSection;
    auto isExist =
        !StringSwitch<bool>(DirectiveStack.back())
             .Case(".global", ".globl",
                   [&](auto&& _) {
                     const auto& section =
                         DirectiveStack[DirectiveStack.size() - 2];

                     if (section == ".data") {
                       return ctx.addReloSym(token.lexeme, curDataOffset,
                                             Ndx::data);
                     } else if (section == ".bss") {
                       return ctx.addReloSym(token.lexeme, curBssOffset,
                                             Ndx::bss);
                     } else if (section == ".text") {
                       return ctx.addReloSym(token.lexeme, curTextOffset,
                                             Ndx::text);
                     }

                     utils::unreachable("cant match the section of this label");
                   })
             .Error();

    utils_assert(!isExist, "global symbol redefinition");

    DirectiveStack.pop_back(); // .global, .globl
  }
  advance();
}

void Parser::ParseInteger() {
  auto dw = std::stoll(token.lexeme);
  if (curInst) {
    curInst->addOperand(MCOperand::makeImm(dw));
  } else {
    /// TODO: more directive

    if (DirectiveStack[DirectiveStack.size() - 2] == ".data") {
      StringSwitch<bool>(DirectiveStack.back())
          .Case(".half",
                [&](auto&& _) {
                  curDataOffset = ctx.pushDataBuf<uint16_t>(dw);
                  return true;
                })
          .Case(".word",
                [&](auto&& _) {
                  curDataOffset = ctx.pushDataBuf<uint32_t>(dw);
                  return true;
                })
          .Case(".dword",
                [&](auto&& _) {
                  curDataOffset = ctx.pushDataBuf<uint64_t>(dw);
                  return true;
                })
          .Case(".align",
                [&](auto&& _) {
                  utils_assert(dw < 16, "expectling align target to be "
                                        "small than 16");

                  curDataOffset = ctx.makeDataBufAlign(utils::pow2i(dw));

                  return true;
                })
          .Case(".balign",
                [&](auto&& _) {
                  auto e = utils::log2(dw);
                  utils_assert(e, "expecting dw to be pow of 2");

                  curDataOffset = ctx.makeDataBufAlign(dw);
                  return true;
                })
          .Error();

      DirectiveStack.pop_back();

    } else if (DirectiveStack[DirectiveStack.size() - 2] == ".bss") {

      StringSwitch<bool>(DirectiveStack.back())
          .Case(".zero",
                [&](auto&& _) {
                  curBssOffset = ctx.pushBssBuf(dw);
                  return true;
                })
          .Case(".space",
                [&](auto&& _) {
                  curBssOffset = ctx.pushBssBuf(dw);
                  return true;
                })
          .Case(".align",
                [&](auto&& _) {
                  utils_assert(dw < 16, "expectling align target to be "
                                        "small than 16");

                  curBssOffset = ctx.makeBssBufAlign(utils::pow2i(dw));

                  return true;
                })
          .Case(".balign",
                [&](auto&& _) {
                  auto e = utils::log2(dw);
                  utils_assert(e, "expecting dw to be pow of 2");

                  curBssOffset = ctx.makeBssBufAlign(dw);
                  return true;
                })
          .Error();

      DirectiveStack.pop_back();

    } else {
      utils::unreachable("expect literal in .data or .bss section");
    }
  }
  advance();
}

void Parser::ParseHexInteger() {
  auto dw = std::stoull(token.lexeme, nullptr, 16);
  if (curInst) {
    curInst->addOperand(MCOperand::makeImm(dw));
  } else {
    /// TODO: more directive

    if (DirectiveStack[DirectiveStack.size() - 2] == ".data") {
      StringSwitch<bool>(DirectiveStack.back())
          .Case(".half",
                [&](auto&& _) {
                  curDataOffset = ctx.pushDataBuf<uint16_t>(dw);
                  return true;
                })
          .Case(".word",
                [&](auto&& _) {
                  curDataOffset = ctx.pushDataBuf<uint32_t>(dw);
                  return true;
                })
          .Case(".dword",
                [&](auto&& _) {
                  curDataOffset = ctx.pushDataBuf<uint64_t>(dw);
                  return true;
                })
          .Case(".align",
                [&](auto&& _) {
                  utils_assert(dw < 16, "expectling align target to be "
                                        "small than 16");

                  curDataOffset = ctx.makeDataBufAlign(utils::pow2i(dw));

                  return true;
                })
          .Case(".balign",
                [&](auto&& _) {
                  auto e = utils::log2(dw);
                  utils_assert(e, "expecting dw to be pow of 2");

                  curDataOffset = ctx.makeDataBufAlign(dw);
                  return true;
                })
          .Error();

      DirectiveStack.pop_back();

    } else if (DirectiveStack[DirectiveStack.size() - 2] == ".bss") {
      utils_assert(dw == 0, "data def in bss supposed to be all zero");

      StringSwitch<bool>(DirectiveStack.back())
          .Case(".zero",
                [&](auto&& _) {
                  curBssOffset = ctx.pushBssBuf(dw);
                  return true;
                })
          .Case(".align",
                [&](auto&& _) {
                  utils_assert(dw < 16, "expectling align target to be "
                                        "small than 16");

                  curBssOffset = ctx.makeBssBufAlign(utils::pow2i(dw));

                  return true;
                })
          .Case(".balign",
                [&](auto&& _) {
                  auto e = utils::log2(dw);
                  utils_assert(e, "expecting dw to be pow of 2");

                  curBssOffset = ctx.makeBssBufAlign(dw);
                  return true;
                })
          .Error();

      DirectiveStack.pop_back();

    } else {
      utils::unreachable("expect literal in .data or .bss section");
    }
  }
  advance();
}

void Parser::ParseFloat() {

  utils_assert(!DirectiveStack.empty(), "expecting in an directive");
  auto [fimm, isDouble] =
      StringSwitch<std::tuple<uint64_t, bool>>(DirectiveStack.back())
          .Case(".float",
                [](auto&& Str) -> std::tuple<uint64_t, bool> {
                  float value;
                  auto [ptr, ec] = std::from_chars(
                      Str.data(), Str.data() + Str.size(), value);

                  utils_assert(ec == std::error_code{},
                               "parse float point failed");

                  return std::make_tuple(*reinterpret_cast<uint64_t*>(&value),
                                         false);
                })
          .Case(".double",
                [](auto&& Str) -> std::tuple<uint64_t, bool> {
                  double value;
                  auto [ptr, ec] = std::from_chars(
                      Str.data(), Str.data() + Str.size(), value);

                  utils_assert(ec == std::error_code{},
                               "parse float point failed");

                  return std::make_tuple(*reinterpret_cast<uint64_t*>(&value),
                                         false);
                })
          .Default();

  if (isDouble) {
    ctx.pushDataBuf(fimm);
  } else {
    ctx.pushDataBuf(static_cast<uint32_t>(fimm));
  }

  advance();
}

void Parser::ParseModifier() {
  utils_assert(curInst, "expect curInst to be valid");
  auto ty = mc::MCExpr::getExprTy(token.lexeme);
  utils_assert(ty, "invalid modifier");
  advance();
  utils_assert(token.type == TokenType::LPAREN,
               "expecting left paren after a modifier");
  advance();

  auto Symbol = token.lexeme; // IDENTIFIER

  utils_assert(token.type == TokenType::IDENTIFIER,
               "expecting an identifier in a modifier");

  advance();

  uint64_t Append = 0;
  if (token.type == TokenType::EXPR_OPERATOR) {
    /// parse op and constexpr
    auto op = token.lexeme == "+" ? 1ll : -1ll;

    advance();

    utils_assert(token.type == TokenType::INTEGER, "expecting integer append");

    op *= std::stoll(token.lexeme); // no hex

    Append = *reinterpret_cast<uint64_t*>(&op);

    advance();
  }

  utils_assert(token.type == TokenType::RPAREN,
               "expecting right paren after a modifier");

  curInst->addOperand(MCOperand::makeExpr(ctx.getTextExpr(Symbol, ty, Append)));

  ctx.addReloInst(&(*curInst), Symbol);

  advance();
}

void Parser::ParseInstruction() {
  /// must empty

  curInst = ctx.newTextInst(token.lexeme);
  curInst->modifyOffset(curTextOffset);
  curInst->modifyLoc(token.loc);

  auto [instAlign, padInst] =
      (*curInst).isCompressed()
          ? std::make_tuple(2, MCInst::makeNop(token.loc, curTextOffset))
          : std::make_tuple(4, MCInst::makeNop(token.loc, curTextOffset));

  /// make align
  if (curTextOffset % instAlign) {
    curInst->modifyOffset(ctx.addTextInst(std::move(padInst)));

    curTextOffset = curInst->getOffset();
  }

  advance();
}

void Parser::ParsePseudo() {
  /// collecting arguments

  auto& pseudo = *PseudoFind(token.lexeme.c_str());
  bool rd = pseudo.rd, rs = pseudo.rs, rt = pseudo.rt;

  auto args = pseudo.getArgTuple();

  while ((advance(), token.type != TokenType::NEWLINE)) {
    switch (token.type) {
    case TokenType::REGISTER: {
      auto reg = RegHelper(token.lexeme);
      if (rd)
        std::get<0>(args) = reg, rd = false;
      else if (rs)
        std::get<2>(args) = reg, rs = false;
      else if (rt)
        std::get<3>(args) = reg, rt = false;
    } break;
    case TokenType::IDENTIFIER: // assign once
      std::get<1>(args) = token.lexeme;
      std::get<4>(args) = token.lexeme;
      break;
    case TokenType::COMMA:
      continue;
    case TokenType::END_OF_FILE:
      goto __call_back;
    default:
      utils::unreachable("unknown item when unpacking pseudo");
    }
  }

__call_back:
  /// make align
  auto [instAlign, padInst] =
      std::make_tuple(4, MCInst::makeNop(token.loc, curTextOffset));

  if (curTextOffset % instAlign) {
    curTextOffset = ctx.addTextInst(std::move(padInst));
  }

  for (auto inst : std::apply(
           pseudo(), std::tuple_cat(std::make_tuple(std::ref(ctx)), args))) {
    curTextOffset += inst->isCompressed() ? 2 : 4;
  }
}

void Parser::ParseLi() {
  /// make align
  auto [instAlign, padInst] =
      std::make_tuple(4, MCInst::makeNop(token.loc, curTextOffset));

  if (curTextOffset % instAlign) {
    curTextOffset = ctx.addTextInst(std::move(padInst));
  }

  std::string reg;
  int64_t imme;
  while ((advance(), token.type != TokenType::NEWLINE)) {
    switch (token.type) {
    case TokenType::REGISTER:
      reg = token.lexeme;
      break;
    case TokenType::INTEGER:
      imme = std::stoll(token.lexeme);
      break;
    case TokenType::HEX_INTEGER:
      imme = std::stoll(token.lexeme, nullptr, 16);
      break;
    case TokenType::COMMA:
      continue;
    case TokenType::END_OF_FILE:
      goto __make_li;
    default:
      utils::unreachable("unknown item when unpacking pseudo");
    }
  }

__make_li:
  MCInst::MCInsts insts =
      MCInst::makeLi(token.loc, curTextOffset, reg.c_str(), imme);

  std::for_each(insts.begin(), insts.end(), [&](auto&& inst) {
    curTextOffset = ctx.addTextInst(std::move(inst));
  });
}

void Parser::ParseRegister() {
  utils_assert(curInst, "expect curInst to be valid");
  curInst->addOperand(MCOperand::makeReg(RegHelper(token.lexeme)));
  advance();
}

void Parser::ParseDirective() {
  auto isSectionDirective = StringSwitch<bool>(token.lexeme)
                                .Case(".data", ".bss", ".text", true)
                                .Default(false);

  if (isSectionDirective && !DirectiveStack.empty()) {
    /// end of the last section & start of the new section
    DirectiveStack.pop_back();
  }

  DirectiveStack.emplace_back(token.lexeme);

  advance();
}

void Parser::ParseLabelDef() {

  auto _ = StringSwitch<bool>(DirectiveStack.back())
               .Case(".text",
                     [&](auto&& _) {
                       utils_assert(ctx.addTextLabel(token.lexeme.substr(
                                        0, token.lexeme.length() - 1)),
                                    "text label redefinition!");
                       return true;
                     })
               .Case(".data",
                     [&](auto&& _) {
                       utils_assert(ctx.addDataVar(token.lexeme),
                                    "data label redefinition!");
                       return true;
                     })
               .Case(".bss",
                     [&](auto&& _) {
                       utils_assert(ctx.addBssVar(token.lexeme),
                                    "bss label redefinition");
                       return true;
                     })
               .Error();

  advance();
}

void Parser::ParseString() { utils::todo("sting def pesudo not impl yet"); }
