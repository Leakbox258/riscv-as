#ifndef MC_EXPR
#define MC_EXPR

#include "utils/ADT/StringRef.hpp"
#include "utils/misc.hpp"
#include <utility>

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
  using ExprTy = MCExpr::ExprTy;

  static bool isStaticOffset(ExprTy ty) {
    return utils::in_interval<true, true>(kJAL, kRVC_BRANCH, ty);
  }

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
  bool isJAL() const { return Kind == kJAL; }
  bool isBranch() const { return Kind == kBRANCH; }
  bool isRVC_JUMP() const { return Kind == kRVC_JUMP; }
  bool isRVC_BRANCH() const { return Kind == kRVC_BRANCH; }
  bool isCALL_PLT() const { return Kind == kCALL_PLT; }

  StringRef getSym() const { return Symbol; }

  ExprTy getModifier() const { return Kind; }
  void setModifier(ExprTy ty) { Kind = ty; }

  uint64_t getAddend() const { return Append; }

  static ExprTy getExprTy(const StringRef& Mod);
  static unsigned getModifierSize(MCExpr::ExprTy mod);
};

} // namespace mc

#endif
