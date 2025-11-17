#ifndef MC_OPERAND
#define MC_OPERAND

#include "mc/MCExpr.hpp"
#include "utils/ADT/StringMap.hpp"
#include "utils/macro.hpp"
#include "utils/misc.hpp"
#include <cstdint>
#include <type_traits>
namespace mc {

template <typename V> using StringMap = utils::ADT::StringMap<V>;

extern const StringMap<uint8_t> Registers;

extern const StringMap<uint8_t> CRegisters;

extern const StringMap<uint8_t> RoundModes;

class MCContext;
class MCExpr;
class MCInst;

using MCReg = uint8_t;

class MCOperand {
  friend MCContext;

  enum OpTy : unsigned char {
    kInvalid,
    kReg,
    kImme,
    kSFPImme,
    kDFPImme,
    kExpr,
    kInst,
  };

  OpTy Kind = kInvalid;

  union {
    MCReg Reg;
    int64_t Imm;
    uint32_t SFPImm;
    uint64_t DFPImm;
    MCExpr* Expr;
    const MCInst* Inst;
  };

  /// imms in RV maybe distribute separately
  /// when traversal MCOpCode::EnCoding, use this elem to record the length that
  /// is already used
  /// when op is arith: use 12 elements
  /// when op is branch: use 13 elements (the lowest never used)
  /// when op is jump: use 21 elements (the lowest never used)
  // std::array<unsigned char, 21> used{};

public:
  MCOperand() : DFPImm(0) {}

  static MCOperand makeReg(MCReg _Reg);

  static MCOperand makeImm(int64_t _Imm);

  static MCOperand makeSFPImm(uint32_t _SFPImm);

  static MCOperand makeDFPImm(uint64_t _DFPImm);

  static MCOperand makeExpr(MCExpr* _Expr);

  static MCOperand makeInst(const MCInst* _Inst);

  template <typename Ty> static MCOperand make(Ty op) {

    if constexpr (std::is_same_v<Ty, MCReg>) {
      return makeReg(op);
    } else if constexpr (std::is_same_v<Ty, int64_t>) {
      return makeImm(op);
    } else if constexpr (std::is_same_v<Ty, uint32_t>) {
      return makeSFPImm(op);
    } else if constexpr (std::is_same_v<Ty, uint64_t>) {
      return makeDFPImm(op);
    } else if constexpr (std::is_same_v<std::remove_cv_t<Ty>, MCExpr*>) {
      return makeExpr(op);
    } else if constexpr (std::is_same_v<std::remove_cv_t<Ty>, MCInst*>) {
      return makeInst(op);
    } else {
      static_assert(false, "MCOperand: unknown type");
    }
  }

  bool isValid() const { return Kind != kInvalid; }
  bool isReg() const { return Kind == kReg; }
  bool isImm() const { return Kind == kImme; }
  bool isSFPImm() const { return Kind == kSFPImme; }
  bool isDFPImm() const { return Kind == kDFPImme; }
  bool isGImm() const { return utils::in_set(Kind, kImme, kSFPImme, kDFPImme); }
  bool isExpr() const { return Kind == kExpr; }
  bool isInst() const { return Kind == kInst; }

  MCReg getReg() const {
    utils_assert(isReg(), "not a reg");
    return Reg;
  }

  MCReg& getReg() {
    utils_assert(isReg(), "not a reg");
    return Reg;
  }

  int64_t getImm() const {
    utils_assert(isImm(), "not an immediate");
    return Imm;
  }

  int64_t& getImm() {
    utils_assert(isImm(), "not an immediate");
    return Imm;
  }

  uint32_t getSFPImm() const {
    utils_assert(isSFPImm(), "not an SFP immediate");
    return SFPImm;
  }

  uint32_t& getSFPImm() {
    utils_assert(isSFPImm(), "not an SFP immediate");
    return SFPImm;
  }

  uint64_t getDFPImm() const {
    utils_assert(isDFPImm(), "not an FP immediate");
    return DFPImm;
  }

  uint64_t& getDFPImm() {
    utils_assert(isDFPImm(), "not an FP immediate");
    return DFPImm;
  }

  uint64_t getGImm() const {
    utils_assert(isGImm(), "not an Generic immediate");
    return Imm;
  }

  const MCExpr* getExpr() const {
    utils_assert(isExpr(), "not an expression");
    return Expr;
  }

  void setExpr(MCExpr::ExprTy ty) {
    utils_assert(isExpr(), "not an expression");
    Expr->setModifier(ty);
  }

  const MCInst* getInst() const {
    utils_assert(isInst(), "not a sub-instruction");
    return Inst;
  }

  /// encoding here expected to be processed
  void RewriteSymRelo(int64_t encoding);
};
} // namespace mc

#endif