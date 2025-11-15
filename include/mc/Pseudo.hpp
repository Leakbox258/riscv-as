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
#include "utils/misc.hpp"
#include <array>
#include <cstdint>
#include <string>
#include <tuple>
#include <type_traits>

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
    Rs,
    Rt,
    Offset,
    ImmeNeg1,
    Imme0,
    Imme1,
    X0,
    X1,
    X6 = X0 + 6,
  };

  static constexpr auto argTbl = std::make_tuple(
      Rd, Symbol, Rs, Rt, Offset, ImmeNeg1, Imme0, Imme1, X0, X1, X6);
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
      inst.Op = son->content;

      /// @note enum the items that may appear in .def file
      for (auto operand : son->sons) {
        if (!operand) {
          break;
        }

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
                      .Case("%hi",
                            [this, &inst](auto&& _) {
                              symbol = true;

                              if (name == "CALL" || name == "TAIL") {
                                inst.reloTy = MCExpr::kCALL_PLT;
                              } else {
                                inst.reloTy = MCExpr::kPCREL_HI;
                              }

                              return Symbol;
                            })
                      .Case("%lo",
                            [this, &inst](auto&& _) {
                              symbol = true;

                              if (name == "CALL" || name == "TAIL") {
                                // inst.reloTy = MCExpr::kCALL_PLT;
                              } else {
                                inst.reloTy = MCExpr::kPCREL_LO;
                              }

                              return Symbol;
                            })
                      .Case("offset",
                            [this, &inst](auto&& _) {
                              offset = true;

                              if (name.begin_with("B")) {
                                inst.reloTy = MCExpr::kBRANCH;
                              } else {
                                inst.reloTy = MCExpr::kJAL;
                              }

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
                      .Default(Rd); /// just to make clang happy

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

  void addOperand(MCContext& ctx, MCInst* Inst, const InstPattern& pattern,
                  auto ArgsTuple) const {

    for (uint32_t opCnt = 0; opCnt < pattern.opNr; ++opCnt) {
      const auto& opKind = pattern.Operands[opCnt];

      auto addOpImpl = [&]<std::size_t I>() {
        if (utils::in_interval<true, true>(ImmeNeg1, Imme1, opKind)) {
          Inst->addOperand(
              MCOperand::makeImm((int64_t)opKind - ImmeNeg1 - 1)); // -1, 0, 1
        } else if (opKind >= X0) {
          Inst->addOperand(MCOperand::make(MCReg(opKind - X0))); // x0, x1, x6
        } else {
          /// avoid clang may-analysis type-invoke check
          auto arg = std::get<I>(ArgsTuple);

          if constexpr (std::is_same_v<decltype(arg), std::string>) {
            if (pattern.reloTy == MCExpr::kInvalid) {
              Inst->addOperand(MCOperand::makeImm(0));
            } else if (pattern.reloTy == MCExpr::kPCREL_LO) {
              auto inner_label = ctx.buildInnerTextLabel();
              Inst->addOperand(MCOperand::make(
                  ctx.getTextExpr(inner_label, pattern.reloTy)));
              ctx.addReloInst(Inst, inner_label);
            } else {
              Inst->addOperand(
                  MCOperand::make(ctx.getTextExpr(arg, pattern.reloTy)));
              ctx.addReloInst(Inst, arg);
            }
            Inst->relax();
          } else if constexpr (std::is_same_v<decltype(arg), MCReg>) {
            Inst->addOperand(MCOperand::make(arg));
          }
        }
        // else if (utils::in_set(opKind, Symbol, Offset)) {
        //   auto& sym = std::get<I>(ArgsTuple);

        //   Inst->addOperand(
        //       MCOperand::make(ctx.getTextExpr(sym, pattern.reloTy)));

        //   ctx.addReloInst(Inst, sym.str());
        // } else {
        //   Inst->addOperand(MCOperand::make(std::get<I>(ArgsTuple)));
        // }
      };

      [&]<std::size_t... I>(std::index_sequence<I...>) {
        return ((std::get<I>(argTbl) == opKind
                     ? addOpImpl.template operator()<I>()
                     : void()),
                ...);
      }(std::make_index_sequence<std::tuple_size_v<Types>>{});
    }
  }

  static auto getArgTuple() {
    // Rd Symbol Rt Rs Offset
    return std::tuple<MCReg, std::string, MCReg, MCReg, std::string, void*,
                      void*, void*, void*, void*, void*>();
  }

  auto operator()() const {

    auto instsBuild = [this](MCContext& ctx, auto... Args) {
      auto ArgsTuple = std::make_tuple(Args...);

      SmallVector<StringRef, 4> Ops;
      for (uint32_t instCnt = 0; instCnt < this->InstNr; ++instCnt) {
        Ops.emplace_back(this->InstPatterns[instCnt].Op);
      }

      MCInstPtrs insts = ctx.newTextInsts<4>(Ops);

      int idx = 0;
      for (auto inst : insts) {
        addOperand(ctx, inst, this->InstPatterns[idx++], ArgsTuple);
      }

      ctx.commitTextInsts(insts);

      return insts;
    };

    // auto argNormalize = [&flags](auto... args) {
    //   auto rawTuple = std::make_tuple(args...);
    //   constexpr std::size_t argNr = std::tuple_size_v<decltype(rawTuple)>;

    //   auto ArgTuple = [&]<std::size_t... I>(std::index_sequence<I...>) {
    //     auto getElem = [&]<std::size_t Idx>() {
    //       constexpr std::size_t argIdx = [&flags]() {
    //         std::size_t count = 0;
    //         for (std::size_t k = 0; k < Idx; ++k) {
    //           if (flags[k]) {
    //             count++;
    //           }
    //         }
    //         return count;
    //       }();

    //       if constexpr (flags[Idx] && argIdx < argNr) {
    //         return std::tuple{std::get<argIdx>(rawTuple)};
    //       } else {
    //         return std::make_tuple(nullptr); // hold space
    //       }
    //     };

    //     return std::tuple_cat(getElem.template operator()<I>()...);
    //   }(std::make_index_sequence<std::tuple_size_v<Types>>{});

    //   return ArgTuple;
    // };

    /// return as a callback
    return [instsBuild](MCContext& ctx, auto... args) {
      return instsBuild(ctx, args...);
    };
  }
};

#define PSEUDO_DEF(name, pattern)                                              \
  inline constexpr char _##name##_pseudo[] = #name;                            \
  inline constexpr char _##name##_pseudo##_Pattern[] = #pattern;               \
  static constexpr Pseudo name##_pseudo{_##name##_pseudo,                      \
                                        _##name##_pseudo##_Pattern};

#define PSEUDO(name, pattern) PSEUDO_DEF(name, pattern)
#include "Pseudo.def"
#undef PSEUDO

} // namespace mc

namespace parser {
#define PSEUDO_NAME(name) processPseudo(#name)

#define PSEUDO(name, pattern)                                                  \
  std::make_pair(PSEUDO_NAME(name), &mc::name##_pseudo),

constexpr inline auto PseudoMap = std::tuple{
#include "Pseudo.def"
};

#undef PSEUDO

template <size_t N>
constexpr bool PseudoContainImpl(const std::array<char, N>& arr,
                                 const char* c_str) {
  for (size_t i = 0; i < N; ++i) {
    if (arr[i] != c_str[i])
      return false;
    if (c_str[i] == '\0')
      return arr[i] == '\0';
  }
  return c_str[N] == '\0';
}

template <size_t I = 0, typename T>
constexpr bool PseudoContain(const T& value) {
  if constexpr (I < std::tuple_size_v<decltype(PseudoMap)>) {
    if (PseudoContainImpl(std::get<I>(PseudoMap).first, value)) {
      return true;
    }
    return PseudoContain<I + 1>(value);
  }
  return false;
}

template <size_t I = 0, typename T>
constexpr const mc::Pseudo* PseudoFind(const T& value) {
  if constexpr (I < std::tuple_size_v<decltype(PseudoMap)>) {
    if (PseudoContainImpl(std::get<I>(PseudoMap).first, value)) {
      return std::get<I>(PseudoMap).second;
    }
    return PseudoFind<I + 1>(value);
  }
  return nullptr;
}

} // namespace parser

#endif