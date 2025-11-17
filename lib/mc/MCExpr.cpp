#include "mc/MCExpr.hpp"
#include "utils/ADT/StringSwitch.hpp"

using namespace mc;

using ExprTy = MCExpr::ExprTy;

ExprTy MCExpr::getExprTy(const StringRef& Mod) {
  return utils::ADT::StringSwitch<ExprTy>(Mod)
      .Case("%lo", kLO)
      .Case("%hi", kHI)
      .Case("%pcrel_lo", kPCREL_LO)
      .Case("%pcrel_hi", kPCREL_HI)
      .Case("%got_pcrel_hi", kGOT_PCREL_HI)
      .Case("%tprel_add", kTPREL_ADD)
      .Case("%tprel_hi", kTPREL_HI)
      .Case("%tls_ie_pcrel_hi", kTLS_IE_PCREL_HI)
      .Case("%tls_gd_pcrel_hi", kTLS_GD_PCREL_HI)
      .Default(kInvalid);
}

unsigned MCExpr::getModifierSize(MCExpr::ExprTy mod) {
  switch (mod) {
  case kInvalid:
    utils::unreachable("unknown modifier");
  case kRVC_BRANCH:
    return 9;
  case kLO:
  case kPCREL_LO:
  case kBRANCH:
  case kRVC_JUMP:
    return 12;
  case kHI:
  case kPCREL_HI:
  case kGOT_PCREL_HI:
  case kTPREL_ADD:
  case kTPREL_HI:
  case kTLS_IE_PCREL_HI:
  case kTLS_GD_PCREL_HI:
    return 20;
  case kJAL:
    return 21;
  case kCALL_PLT:
    return 32; // auipc + ...
  }
  utils::unreachable("unknown modifier");
  return 0;
}
