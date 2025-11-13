#ifndef MC_CONTEXT
#define MC_CONTEXT

#include "MCExpr.hpp"
#include "MCInst.hpp"
#include "MCOpCode.hpp"
#include "utils/ADT/ByteStream.hpp"
#include "utils/ADT/StringMap.hpp"
#include "utils/ADT/StringRef.hpp"
#include "utils/ADT/StringSet.hpp"
#include "utils/macro.hpp"
#include <cstddef>
#include <cstdint>
#include <cstring>
#include <deque>
#include <elf.h>
#include <fstream>
#include <ostream>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace mc {
using StringRef = utils::ADT::StringRef;
template <typename V> using StringMap = utils::ADT::StringMap<V>;
using StringSet = utils::ADT::StringSet<>;
using ByteStream = utils::ADT::ByteStream;
using MCInstPtrs = utils::ADT::SmallVector<MCInst*, 4>;

class MCContext {
public:
  using size_ty = std::size_t;

private:
  /// fs handle
  std::ostream& file;

  /// inst use symbols, which need gen Elf64_Rela or cul offset(.text)
  std::set<std::tuple<MCInst*, std::string>> ReloInst;

  /// buffer
  // ByteStream<> TextBuffer;

  /// ELF Header
  Elf64_Ehdr Elf_Ehdr;

  /// .text
  size_ty TextOffset = 0;
  StringMap<size_ty> TextLabels;
  std::deque<MCInst> Insts; // avoid realloction
  std::deque<MCExpr> Exprs; // avoid realloction

  /// .rela.text

public:
  enum NdxSection : uint8_t {
    text = 1,
    data,
    bss,
    und = 0,
  };

private:
  // symbols cross sections, define in .text, .data, .bss
  std::set<std::pair<std::string, NdxSection>> Symbols;
  std::vector<Elf64_Rela> Elf_Relas;

  std::set<std::string> ExternSymbols;

  /// .data
  StringMap<size_ty> DataVariables;
  ByteStream DataBuffer;

  /// .bss
  StringMap<size_ty> BssVariables;
  size_ty BssSize = 0;

  /// TODO: add .symtab section
  std::vector<Elf64_Sym> Elf_Syms;

  /// .strtab
  ByteStream StrTabBuffer;

  /// .shstrtab
  ByteStream SHStrTabBuffer;

  /// Section Header Table
  SmallVector<Elf64_Shdr, 8> Elf_Shdrs;

public:
  MCContext(std::ofstream& _file) : file(_file) {}
  MCContext(const MCContext&) = delete;
  MCContext(MCContext&&) = delete;
  MCContext& operator=(const MCContext&) = delete;

private:
  void mkStrTab();

  /// .text symbol inline
  void Relo();

  /// offset of each section
  StringMap<size_ty> Offsets;

  // elf header & section headers (table)
  void Ehdr_Shdr();

private:
  size_ty incTextOffset(bool IsCompressed = false) {
    return IsCompressed ? TextOffset += 2 : TextOffset += 4;
  }

public:
  /// build obj file
  void writein();

  bool addTextLabel(StringRef Str) {
    return this->TextLabels.insert(Str, TextOffset);
  }

  bool addTextLabel(StringRef Str, size_ty offset) {
    return this->TextLabels.insert(Str, std::move(offset));
  }

  /// add local symbols from .data .text .bss
  bool addReloSym(StringRef Str, NdxSection ndx) {
    return this->Symbols.insert({Str.str(), ndx}).second;
  }

  bool getTextOffset() const { return TextOffset; }

  size_ty addTextInst(MCInst&& inst) {

    auto newOffset = incTextOffset(inst.isCompressed());

    Insts.push_back(std::move(inst));

    return newOffset;
  }

  MCInst* newTextInst(const StringRef OpCode LIFETIME_BOUND) {
    this->Insts.emplace_back(MCInst(OpCode));

    return &this->Insts.back();
  }

  MCInst* newTextInst(const MCOpCode* OpCode LIFETIME_BOUND) {
    this->Insts.emplace_back(MCInst(OpCode));

    return &this->Insts.back();
  }

  size_ty commitTextInst() {

    auto newOffset = incTextOffset(this->Insts.back().isCompressed());

    return newOffset;
  }

  template <typename... Args> MCInstPtrs newTextInsts(Args&&... OpCodes) {
    MCInstPtrs insts{};

    insts.emplace_back(newTextInst(OpCodes...));

    return insts;
  }

  template <std::size_t N>
  MCInstPtrs newTextInsts(SmallVector<StringRef, N> Ops) {
    MCInstPtrs insts{};

    for (auto& op : Ops) {
      insts.emplace_back(newTextInst(op));
    }

    return insts;
  }

  size_ty commitTextInsts(const MCInstPtrs& insts) {
    size_ty newOffset = 0LL;

    for (const auto& inst : insts) {
      newOffset = incTextOffset(inst->isCompressed());
    }

    return newOffset;
  }

  const MCExpr* getTextExpr(std::string Symbol, MCExpr::ExprTy ty,
                            uint64_t Append = 0) {
    Exprs.emplace_back(ty, Symbol, Append);
    return &Exprs.back();
  }

  const MCExpr* getTextExpr(StringRef Symbol, MCExpr::ExprTy ty,
                            uint64_t Append = 0) {
    Exprs.emplace_back(ty, Symbol, Append);
    return &Exprs.back();
  }

  void addReloInst(MCInst* inst, std::string label) {
    ReloInst.emplace(inst, std::move(label));
  }

  template <typename T> size_ty pushDataBuf(T&& Value) {
    this->DataBuffer << std::forward<T>(Value);
    return this->DataBuffer.size();
  }

  template <size_ty N> size_ty pushDataBuf(char (&Value)[N]) {
    this->DataBuffer << std::forward<decltype(Value)>(Value);
    return this->DataBuffer.size();
  }

  bool addDataVar(StringRef Varibale) {
    return this->DataVariables.insert(Varibale, this->DataBuffer.size());
  }

  size_ty makeDataBufAlign(size_ty balign) {
    this->DataBuffer.balignTo(balign);
    return this->DataBuffer.size();
  }

  size_ty pushBssBuf(size_ty size) { return this->BssSize += size; }

  bool addBssVar(StringRef Varibale) {
    return this->BssVariables.insert(Varibale, this->BssSize);
  }

  size_ty makeBssBufAlign(size_ty balign) {
    return this->BssSize += (balign - this->BssSize % balign) % balign;
  }
};
} // namespace mc

#endif