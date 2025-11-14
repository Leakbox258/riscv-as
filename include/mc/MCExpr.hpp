#ifndef MC_EXPR
#define MC_EXPR

#include "utils/ADT/StringRef.hpp"
#include "utils/ADT/StringSwitch.hpp"

namespace mc {
using StringRef = utils::ADT::StringRef;

class MCExpr {
public:
  enum ExprTy : uint8_t {
    kInvalid,
    kLO, // 0 - 11
    kHI, // 12 - 31
    kPCREL_LO,
    kPCREL_HI,
    kGOT_PCREL_HI, // -fPIC
    kTPREL_ADD,
    kTPREL_HI,
    kTLS_IE_PCREL_HI, // Initial Exec
    kTLS_GD_PCREL_HI, // Global Dynamic

    /// for br & jr
    kJAL,
    kBRANCH,
    kRVC_JUMP,
    kRVC_BRANCH,

    /// pseudo call & tail
    kCALL_PLT,
    /// TODO: kCALL
  };

private:
  ExprTy Kind = kInvalid;
  std::string Symbol;
  uint64_t Append; // +/-
public:
  // MCExpr() : Kind(kInValid), Symbol() {}

  MCExpr(ExprTy ty, StringRef _Symbol, uint64_t _Append)
      : Kind(std::move(ty)), Symbol(_Symbol.str()), Append(_Append) {}

  bool isLO() const { return Kind == kLO; }
  bool isHI() const { return Kind == kHI; }
  bool isPCREL_LO() const { return Kind == kPCREL_LO; }
  bool isPCREL_HI() const { return Kind == kPCREL_HI; }
  bool isGOT_PCREL_HI() const { return Kind == kGOT_PCREL_HI; }
  bool isTPREL_ADD() const { return Kind == kTPREL_ADD; }
  bool isTPREL_HI() const { return Kind == kTPREL_HI; }
  bool isTLS_IE_PCREL_HI() const { return Kind == kTLS_IE_PCREL_HI; }
  bool isTLS_GD_PCREL_HI() const { return Kind == kTLS_GD_PCREL_HI; }

  StringRef getSym() const { return Symbol; }

  ExprTy getModifier() const { return Kind; }
  void setModifier(ExprTy ty) { Kind = ty; }

  uint64_t getAddend() const { return Append; }

  /// TODO: dump
};

inline MCExpr::ExprTy getExprTy(const StringRef& Mod) {
  return utils::ADT::StringSwitch<MCExpr::ExprTy>(Mod)
      .Case("%lo", MCExpr::kLO)
      .Case("%hi", MCExpr::kHI)
      .Case("%pcrel_lo", MCExpr::kPCREL_LO)
      .Case("%pcrel_hi", MCExpr::kPCREL_HI)
      .Case("%got_pcrel_hi", MCExpr::kGOT_PCREL_HI)
      .Case("%tprel_add", MCExpr::kTPREL_ADD)
      .Case("%tprel_hi", MCExpr::kTPREL_HI)
      .Case("%tls_ie_pcrel_hi", MCExpr::kTLS_IE_PCREL_HI)
      .Case("%tls_gd_pcrel_hi", MCExpr::kTLS_GD_PCREL_HI)
      .Default(MCExpr::kInvalid);
}

inline unsigned getModifierSize(MCExpr::ExprTy mod) {
  using ExprTy = MCExpr::ExprTy;
  switch (mod) {
  case ExprTy::kInvalid:
    utils::unreachable("unknown modifier");
  case ExprTy::kRVC_BRANCH:
    return 9;
  case ExprTy::kLO:
  case ExprTy::kPCREL_LO:
  case ExprTy::kBRANCH:
  case ExprTy::kRVC_JUMP:
    return 12;
  case ExprTy::kHI:
  case ExprTy::kPCREL_HI:
  case ExprTy::kGOT_PCREL_HI:
  case ExprTy::kTPREL_ADD:
  case ExprTy::kTPREL_HI:
  case ExprTy::kTLS_IE_PCREL_HI:
  case ExprTy::kTLS_GD_PCREL_HI:
    return 20;
  case ExprTy::kJAL:
    return 21;
  case ExprTy::kCALL_PLT:
    return 32; // auipc + ...
  }
  utils::unreachable("unknown modifier");
  return 0;
}

} // namespace mc

#endif
