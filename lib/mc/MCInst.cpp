#include "mc/MCInst.hpp"
#include "mc/MCExpr.hpp"
#include "mc/MCOpCode.hpp"
#include "mc/MCOperand.hpp"
#include "utils/logger.hpp"
#include "utils/macro.hpp"
#include "utils/misc.hpp"
#include <elf.h>

using namespace mc;

MCOperand& MCInst::addOperand(MCOperand&& newOp) {
  utils_assert(Operands.size() < Operands.capacity(),
               "too many operand for an inst");
  Operands.push_back(std::move(newOp));
  return Operands.back();
}

MCExpr::ExprTy MCInst::getModifier() const {
  for (auto& operand : Operands) {
    if (operand.isExpr()) {
      return operand.getExpr()->getModifier();
    }
  }

  return MCExpr::kInvalid;
}

void MCInst::reloSym(int64_t offset) {

  if (auto _ = getModifier()) {
    auto op = std::find_if(Operands.begin(), Operands.end(),
                           [&](MCOperand& op) { return op.isExpr(); });
    op->RewriteSymRelo(offset);
  } else {
    utils::unreachable("");
  }
}

bool MCInst::hasExpr() const {
  return std::any_of(Operands.begin(), Operands.end(),
                     [&](const MCOperand& op) { return op.isExpr(); });
}

const MCOperand* MCInst::getExprOp() const {
  return std::find_if(Operands.begin(), Operands.end(),
                      [&](const MCOperand& op) { return op.isExpr(); });
}

MCOperand* MCInst::getExprOp() {
  return std::find_if(Operands.begin(), Operands.end(),
                      [&](const MCOperand& op) { return op.isExpr(); });
}

MCExpr::ExprTy MCInst::getExprTy() const {
  using ExprTy = MCExpr::ExprTy;
  if (!hasExpr()) {
    return StringSwitch<ExprTy>(OpCode->name)
        .BeginWith("J", ExprTy::kJAL)
        .BeginWith("B", ExprTy::kBRANCH)
        .BeginWith("C_J", ExprTy::kRVC_JUMP)
        .BeginWith("C_B", ExprTy::kRVC_BRANCH)
        .Error();
  } else {
    return getExprOp()->getExpr()->getModifier();
  }
}

/// Get R_RISCV_...
uint32_t MCInst::getRiscvRType() const {

  using ExprTy = MCExpr::ExprTy;

  switch (getExprTy()) {
  case ExprTy::kInvalid:
    /// @warning this enum meaning that you need to fix reloType according to
    /// symbol itself
    utils::unreachable("");
  case ExprTy::kLO:
    return OpCode->imm_distribute == 1 ? R_RISCV_LO12_I : R_RISCV_LO12_S;
  case ExprTy::kPCREL_LO:
    return OpCode->imm_distribute == 1 ? R_RISCV_PCREL_LO12_I
                                       : R_RISCV_PCREL_LO12_S;
  case ExprTy::kHI:
    return R_RISCV_HI20;
  case ExprTy::kPCREL_HI:
    return R_RISCV_PCREL_HI20;
  case ExprTy::kGOT_PCREL_HI:
    return R_RISCV_GOT_HI20;
  case ExprTy::kTPREL_ADD:
    return R_RISCV_TPREL_ADD;
  case ExprTy::kTPREL_HI:
    return R_RISCV_TPREL_HI20;
  case ExprTy::kTLS_IE_PCREL_HI:
    return R_RISCV_TLS_GOT_HI20;
  case ExprTy::kTLS_GD_PCREL_HI:
    return R_RISCV_TLS_GD_HI20;
  case ExprTy::kJAL:
    return R_RISCV_JAL;
  case ExprTy::kBRANCH:
    return R_RISCV_BRANCH;
  case ExprTy::kRVC_JUMP:
    return R_RISCV_RVC_JUMP;
  case ExprTy::kRVC_BRANCH:
    return R_RISCV_RVC_BRANCH;
  case ExprTy::kCALL_PLT:
    return R_RISCV_CALL_PLT;
  }
}

using MCInsts = utils::ADT::SmallVector<MCInst, 4>;

MCInsts MCInst::makeLi(Location Loc, size_ty Offset, StringRef target,
                       int64_t imme) {
  MCInsts insts{};

  auto reg = *Registers.find(target);

  auto addi_able = [](int64_t imme) -> bool {
    return utils::in_interval<true, true, int64_t>(-2048, 2047, imme);
  };

  auto load32 = [&](int64_t imme) {
    if (addi_able(imme)) {
      /// addi x<>, x0, imme
      auto addi = MCInst(parser::MnemonicFind("addi"), Loc, Offset += 4);
      addi.addOperands(*Registers.find(target), *Registers.find("x0"), imme);

      insts.emplace_back(std::move(addi));
    } else {
      /// lui x<>, %hi(imme
      int64_t high20 = imme - (imme & 0xFFF), low12 = imme & 0xFFF;

      if (high20) {
        auto lui = MCInst(parser::MnemonicFind("lui"), Loc, Offset += 4);
        lui.addOperands(reg, int64_t(high20 + (low12 > 0x800 ? 0x1000 : 0)));
        insts.emplace_back(std::move(lui));
      }

      if (low12 < 0x800) {
        /// addi x<>, x<>,%lo(imme)
        auto addi = MCInst(parser::MnemonicFind("addi"), Loc, Offset += 4);
        addi.addOperands(reg, reg, int64_t(low12));

        insts.emplace_back(std::move(addi));
      } else {
        auto addi_0 = MCInst(parser::MnemonicFind("addi"), Loc, Offset += 4);
        addi_0.addOperands(reg, reg, int64_t(0x7FF));

        auto addi_1 = MCInst(parser::MnemonicFind("addi"), Loc, Offset += 4);
        addi_1.addOperands(reg, reg, int64_t(low12 - 0x7FF));

        insts.emplace_back(std::move(addi_0));
        insts.emplace_back(std::move(addi_1));
      }
    }
  };

  load32(imme & 0xFFFFFFFF);

  if (utils::clz_wrapper((uint64_t)imme) < 32) {
    auto slli = MCInst(parser::MnemonicFind("slli"), Loc, Offset += 4);
    slli.addOperands(reg, reg, (int64_t)32);
    load32(imme >> 32);
  }

  return insts;
}

uint32_t MCInst::makeEncoding() const {
  auto& pattern = OpCode->encodings;

  struct Bits {
    uint32_t bits = 0;
    unsigned len = 0;

    void add(uint32_t elem, unsigned length) {
      bits |= (elem & ((1u << length) - 1)) << len;
      len += length;
    }
  };

  Bits inst;

  for (auto& encode : pattern) {
    auto length = encode.length;
    auto highest = encode.highest;

    switch (encode.kind) {
    case EnCoding::kInvalid:
      /// typically, reach the end of the pattern array
      continue;
    case EnCoding::kStatic:
      inst.add(*encode.static_pattern, length);
      break;
    case EnCoding::kRd:
    case EnCoding::kRd_short:
      inst.add(this->findRegOp<0>().getReg(), length);
      break;
    case EnCoding::kRs1:
    case EnCoding::kRs1_short:
      inst.add(this->findRegOp<1>().getReg(), length);
      break;
    case EnCoding::kRs2:
    case EnCoding::kRs2_short:
      inst.add(this->findRegOp<2>().getReg(), length);
      break;
    case EnCoding::kRs3:
    case EnCoding::kRs3_short:
      inst.add(this->findRegOp<3>().getReg(), length);
      break;
    case EnCoding::kRm:
      inst.add(this->findRm()->getRm(), length);
      break;
    case EnCoding::kMemFence:
    case EnCoding::kImm:
    case EnCoding::kNzImm:
    case EnCoding::kUImm:
      auto immOp = this->findGImmOp();

      unsigned tmp_len = 0;

      /// [31:12]
      auto raw_imm = OpCode->name == "lui" || OpCode->name == "auipc"
                         ? immOp.getGImm() << 12
                         : immOp.getGImm();

      int64_t gimm =
          utils::signIntCompress(raw_imm, highest + 1); // expecting len

      for (auto [high, low] : *encode.bit_range) {
        if (tmp_len == length) {
          break;
        }

        auto immSlice = utils::signIntSlice(gimm, high, low);
        inst.add(immSlice, high - low + 1);
        tmp_len += high - low + 1;
      }

      break;
    }
  }

  if (this->isCompressed()) {
    utils_assert(inst.len == 16, "encoding check failed");
  } else {
    utils_assert(inst.len == 32, "encoding check failed");
  }

  return inst.bits;
}

std::optional<MCOperand> MCInst::findRm() const {
  std::optional<MCOperand> rm = std::nullopt;
  for (auto& op : Operands) {
    if (op.isRm()) {
      rm = op;
      break;
    }
  }

  if (!rm.has_value()) {
    /// rtz as default
    rm = MCOperand::makeRm((uint8_t)0b001);
  }

  return rm;
}

const MCOperand& MCInst::findGImmOp() const {
  // assume that only one immOp in per RV inst
  for (auto& op : Operands) {
    if (op.isGImm()) {
      return op;
    }
  }
  utils::unreachable("cant find the imm op");
}