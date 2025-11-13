#ifndef MC_PSEUDO
#define MC_PSEUDO

#include "MCContext.hpp"
#include "MCExpr.hpp"
#include "MCInst.hpp"
#include "MCOpCode.hpp"
#include "MCOperand.hpp"
#include "utils/ADT/SmallVector.hpp"
#include "utils/ADT/StringMap.hpp"
#include "utils/lisp/lisp.hpp"
#include <array>
#include <cstdint>

namespace {
template <size_t N>
constexpr std::array<char, N> processPseudo(const char (&pseudo)[N]) {
  std::array<char, N> result{};
  for (size_t i = 0; i < N; ++i) {
    char c = pseudo[i];
    if (c == '_') {
      result[i] = '.';
    } else {
      result[i] = toLower(c);
    }
  }
  return result;
}
} // namespace

namespace mc {
using MCInstPtrs = utils::ADT::SmallVector<MCInst*, 4>;
template <typename T> using StringMap = utils::ADT::StringMap<T>;
using Lisp = utils::lisp::Lisp;
using LispNode = utils::lisp::LispNode;

struct Pseudo {
public:
  /// notice that Imme here is ref to defined imme
  /// eg: nop = addi x0, x0, `0`
  enum OperandKind : int8_t {
    Rd,
    Symbol,
    Rt,
    Rs,
    Offset,
    ImmeNeg1,
    Imme0,
    Imme1,
    X0,
    X1,
    X6 = X0 + 6,
  };

  static constexpr auto argTbl = std::make_tuple(
      Rd, Rt, Rs, Symbol, Offset, ImmeNeg1, Imme0, Imme1, X0, X1, X6);
  using Types = decltype(argTbl);

private:
  struct InstPattern {
    StringRef Op;
    std::array<OperandKind, 4> Operands{};
    uint32_t opNr;
    MCExpr::ExprTy reloTy = MCExpr::kInvalid;

    constexpr uint32_t push(OperandKind op) {
      Operands[opNr] = op;
      return ++opNr;
    }
  };

  std::array<InstPattern, 4> InstPatterns{};
  uint32_t InstNr = 0;

  bool rd = false, rt = false, rs = false, symbol = false, offset = false,
       imme0 = false, imme1 = false, immeNeg1 = false, x0 = false, x1 = false,
       x6 = false;

  constexpr void parseImpl(LispNode* rootNode) {

    for (uint32_t sonCnt = 0; sonCnt < rootNode->sonNr; ++sonCnt) {
      auto& son = rootNode->sons[sonCnt];
      auto& inst = InstPatterns[InstNr++];
      inst.Op = son->content.data();

      /// @note enum the items that may appear in .def file
      for (auto operand : son->sons) {
        auto op = StringSwitch<OperandKind>(operand->content)
                      .Case("rd",
                            [this](auto&& _) {
                              rd = true;
                              return Rd;
                            })
                      .Case("rt",
                            [this](auto&& _) {
                              rt = true;
                              return Rt;
                            })
                      .Case("rs",
                            [this](auto&& _) {
                              rs = true;
                              return Rs;
                            })
                      .Case("%hi20",
                            [this, &inst](auto&& _) {
                              symbol = true;
                              inst.reloTy = MCExpr::gHI;
                              return Symbol;
                            })
                      .Case("%lo12",
                            [this, &inst](auto&& _) {
                              symbol = true;
                              inst.reloTy = MCExpr::gLO;
                              return Symbol;
                            })
                      .Case("offset",
                            [this](auto&& _) {
                              offset = true;
                              return Offset;
                            })
                      .Case("0",
                            [this](auto&& _) {
                              imme0 = true;
                              return Imme0;
                            })
                      .Case("1",
                            [this](auto&& _) {
                              imme1 = true;
                              return Imme1;
                            })
                      .Case("-1",
                            [this](auto&& _) {
                              immeNeg1 = true;
                              return ImmeNeg1;
                            })
                      .Case("x0",
                            [this](auto&& _) {
                              x0 = true;
                              return X0;
                            })
                      .Case("x1",
                            [this](auto&& _) {
                              x1 = true;
                              return X1;
                            })
                      .Case("x6",
                            [this](auto&& _) {
                              x6 = true;
                              return X6;
                            })
                      .Error();

        inst.push(op);
      }
    }
  }

public:
  StringRef name;

  constexpr Pseudo(const char* PseudoName, const char* LispPattern)
      : name(PseudoName) {
    Lisp lisp(LispPattern);
    auto rootNode = lisp.getRoot();
    parseImpl(rootNode);
  }

  constexpr void addOperand(MCContext& ctx, MCInst* Inst,
                            const InstPattern& pattern, auto ArgsTuple) {

    for (uint32_t opCnt = 0; opCnt < pattern.opNr; ++opCnt) {
      const auto& opKind = pattern.Operands[opCnt];

      auto addOpImpl = [&]<std::size_t I>() {
        if (utils::in_interval<true, true>(ImmeNeg1, Imme1, opKind)) {
          Inst->addOperand(
              MCOperand::make((uint32_t)opKind - ImmeNeg1 - 1)); // -1, 0, 1
        } else if (opKind >= X0) {
          Inst->addOperand(MCOperand::make((MCReg)opKind - X0)); // x0, x1, x6
        } else if (opKind == Symbol || opKind == Offset) {
          auto& sym = std::get<I>(ArgsTuple);

          Inst->addOperand(
              MCOperand::make(ctx.getTextExpr(sym, pattern.reloTy)));

          ctx.addReloInst(Inst, sym);
        } else {
          Inst->addOperand(MCOperand::make(std::get<I>(ArgsTuple)));
        }
      };

      [&]<std::size_t... I>(std::index_sequence<I...>) {
        return ((std::get<I>(argTbl) == opKind
                     ? addOpImpl.template operator()<I>()
                     : void()),
                ...);
      }(std::make_index_sequence<std::tuple_size_v<Types>>{});
    }
  }

  auto operator()() {

    /// input args

    std::array<bool, std::tuple_size_v<Types>> flags = {
        rd, rt, rs, symbol, offset, immeNeg1, imme0, imme1};

    auto instsBuild = [this](MCContext& ctx, auto ArgsTuple) {
      SmallVector<StringRef, 4> Ops;
      for (uint32_t instCnt = 0; instCnt < InstNr; ++instCnt) {
        Ops.emplace_back(InstPatterns[instCnt].Op);
      }

      MCInstPtrs insts = ctx.newTextInsts(Ops);

      int idx = 0;
      for (auto& inst : insts) {
        addOperand(ctx, inst, InstPatterns[idx++], ArgsTuple);
      }

      ctx.commitTextInsts(insts);
    };

    auto argNormalize = [this, &flags](auto... args) {
      auto rawTuple = std::make_tuple(args...);
      constexpr std::size_t argNr = std::tuple_size_v<decltype(rawTuple)>;

      auto ArgTuple = [&]<std::size_t... I>(std::index_sequence<I...>) {
        auto getElem = [&]<std::size_t Idx>() {
          constexpr std::size_t argIdx = [this, &flags]() {
            std::size_t count = 0;
            for (std::size_t k = 0; k < Idx; ++k) {
              if (flags[k]) {
                count++;
              }
            }
            return count;
          }();

          if constexpr (flags[Idx] && argIdx < argNr) {
            return std::tuple{std::get<argIdx>(rawTuple)};
          } else {
            return std::make_tuple(nullptr); // hold space
          }
        };

        return std::tuple_cat(getElem.template operator()<I>()...);
      }(std::make_index_sequence<std::tuple_size_v<Types>>{});

      return ArgTuple;
    };

    /// return as a callback
    return [instsBuild, argNormalize](MCContext& ctx, auto... args) {
      instsBuild(argNormalize(args...));
    };
  }
};

#define PSEUDO_DEF(name, pattern)                                              \
  inline constexpr char _##name##_pseudo[] = #name;                            \
  inline constexpr char _##name##_pseudo##_Pattern[] = #pattern;               \
  static constexpr Pseudo name##_pseudo{_##name##_pseudo,                      \
                                        _##name##_pseudo##_Pattern};

#define PSEUDO(name, patternd) PSEUDO_DEF(name, pattern)
#include "Pseudo.def"
#undef PSEUDO

} // namespace mc

namespace parser {
#define PSEUDO_TLB(name) processPseudo(#name)
} // namespace parser

#endif