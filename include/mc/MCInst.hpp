#ifndef MC_INST
#define MC_INST

#include "MCExpr.hpp"
#include "mc/MCOpCode.hpp"
#include "mc/MCOperand.hpp"
#include "utils/ADT/SmallVector.hpp"
#include "utils/macro.hpp"
#include "utils/source.hpp"

namespace mc {

using Location = utils::Location;

class MCInst {
public:
  using size_ty = std::size_t;

private:
  const MCOpCode* OpCode; // MCOpCode will all be static and constepxr

  Location Loc;
  size_ty Offset; // offset from the begin of .text
  SmallVector<MCOperand, 6> Operands;

  bool relaxable = false;

public:
  explicit MCInst(const StringRef& _OpCode LIFETIME_BOUND)
      : OpCode(parser::MnemonicFind(_OpCode.str().c_str())) {}

  explicit MCInst(const MCOpCode* _OpCode LIFETIME_BOUND) : OpCode(_OpCode) {}

  explicit MCInst(const MCOpCode* _OpCode LIFETIME_BOUND, Location _Loc,
                  size_ty _Offset)
      : OpCode(_OpCode), Loc(_Loc), Offset(_Offset) {}

  explicit MCInst(const StringRef& _OpCode LIFETIME_BOUND, Location _Loc,
                  size_ty _Offset)
      : OpCode(parser::MnemonicFind(_OpCode.str().c_str())), Loc(_Loc),
        Offset(_Offset) {}

  [[nodiscard]] decltype(Operands)::size_ty getOpSize() const {
    return Operands.size();
  }

  MCOperand& addOperand(MCOperand&& newOp);

  const MCOpCode* getOpCode() const { return OpCode; }
  utils::Location getLoc() const { return Loc; }

  template <decltype(Operands)::size_ty Idx>
  const MCOperand& getOperand() const {
    utils_assert(Idx < Operands.size(),
                 "random access to uninitialized memery");
    return Operands[Idx];
  }

  bool isCompressed() const { return OpCode->name.begin_with("C."); }

  /// allow 20 bits offset
  bool isJmp() const { return OpCode->name.begin_with("J"); }

  bool isBranch() const { return OpCode->name.begin_with("B"); }

  MCExpr::ExprTy getModifier() const;

  size_ty getOffset() const { return Offset; }
  void modifyOffset(size_ty newOffset) { Offset = newOffset; }
  void modifyLoc(Location newLoc) { Loc = newLoc; }

  using const_iter = decltype(Operands)::const_iter;
  using const_rev_iter = decltype(Operands)::const_rev_iter;

  const_iter begin() const { return Operands.begin(); }
  const_iter end() const { return Operands.end(); }

  const_rev_iter rbegin() const { return Operands.rbegin(); }
  const_rev_iter rend() const { return Operands.rend(); }

  void reloSym(int64_t offset);

private:
  bool hasExpr() const;

public:
  const MCOperand* getExprOp() const;

  MCOperand* getExprOp();
  MCExpr::ExprTy getExprTy() const;

  uint32_t getRiscvRType() const;

  constexpr static MCInst makeNop(Location Loc, size_ty Offset) {
    auto nop = MCInst(parser::MnemonicFind("addi"), Loc, Offset);
    nop.addOperand(MCOperand::makeReg(*Registers.find("x0")));
    nop.addOperand(MCOperand::makeReg(*Registers.find("x0")));
    nop.addOperand(MCOperand::makeImm(0));
    return nop;
  }

  constexpr static MCInst makeCNop(Location Loc, size_ty Offset) {
    return MCInst(parser::MnemonicFind("c.nop"), Loc, Offset);
  }

  uint32_t makeEncoding() const;

  void relax() { relaxable = true; }
  bool isRelaxable() const { return relaxable; }

private:
  /// 0 = Rd(optional), 1 = Rs1, 2 = Rs2, 3 = Rs3
  template <unsigned idx> const MCOperand& findRegOp() const {
    int i = OpCode->hasRd ? 0 : 1;
    for (auto& op : Operands) {
      if (!op.isReg()) {
        continue;
      }

      if (i == idx) {
        return op;
      } else {
        ++i;
      }
    }

    utils::unreachable("cant find the right reg op");
  }

  const MCOperand& findGImmOp() const;
};

} // namespace mc

#endif