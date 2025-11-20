#include "mc/MCContext.hpp"
#include "mc/MCExpr.hpp"
#include "utils/macro.hpp"
#include <algorithm>
#include <cstdint>
#include <cstring>
#include <elf.h>
#include <iterator>
#include <string>
#include <type_traits>
#include <vector>

using namespace mc;

bool MCContext::addTextLabel(StringRef Str) {
  return this->TextLabels.insert(Str, TextOffset);
}

bool MCContext::addTextLabel(StringRef Str, size_ty offset) {
  return this->TextLabels.insert(Str, std::move(offset));
}

std::string MCContext::buildInnerTextLabel() {
  auto inner_label = ".L" + std::to_string(InnerLabelNr++);
  this->TextLabels.insert(inner_label.c_str(), TextOffset);
  return inner_label;
}

bool MCContext::addReloSym(StringRef Str, size_ty offset, NdxSection ndx) {
  return this->Symbols.insert({Str.str(), offset, ndx}).second;
}

MCContext::size_ty MCContext::addTextInst(MCInst&& inst) {

  auto newOffset = incTextOffset(inst.isCompressed());

  Insts.push_back(std::move(inst));

  return newOffset;
}

MCInst* MCContext::newTextInst(const StringRef OpCode LIFETIME_BOUND) {
  this->Insts.emplace_back(MCInst(OpCode));

  return &this->Insts.back();
}

MCInst* MCContext::newTextInst(const MCOpCode* OpCode LIFETIME_BOUND) {
  this->Insts.emplace_back(MCInst(OpCode));

  return &this->Insts.back();
}

MCContext::size_ty MCContext::commitTextInst() {
  auto newOffset = incTextOffset(this->Insts.back().isCompressed());

  return newOffset;
}

MCContext::size_ty MCContext::commitTextInsts(const MCInstPtrs& insts) {
  size_ty newOffset;

  for (auto& inst : insts) {
    inst->modifyOffset(TextOffset);
    newOffset = incTextOffset(inst->isCompressed());
  }

  return newOffset;
}

void MCContext::mkStrTab() {
  /// gather .strtab context
  /// include label, symbol(relos, variables)
  StrTabBuffer << '\x00';

  for (const auto& [relo_sym, offset, ndx] : Symbols) {
    if (!StrTabBuffer.hasSym(relo_sym)) {
      StrTabBuffer << relo_sym << '\x00';
    }
  }

  for (const auto& label : TextLabels.keys()) {
    if (!StrTabBuffer.hasSym(label)) {
      StrTabBuffer << label << '\x00';
    }
  }

  /// find extern symbol in relo insts
  for (auto& [_, sym] : ReloInst) {
    if (!StrTabBuffer.hasSym(sym)) {
      StrTabBuffer << sym << '\x00';
      ExternSymbols.insert(sym);
    }
  }
}

void MCContext::mkShStrTab() {
  /// gather .shstrtab
  /// include names of each section
  SHStrTabBuffer << '\x00';
  SHStrTabBuffer << ".text" << '\x00';
  SHStrTabBuffer << ".data" << '\x00';
  SHStrTabBuffer << ".bss" << '\x00';
  SHStrTabBuffer << ".strtab" << '\x00';
  SHStrTabBuffer << ".symtab" << '\x00';
  SHStrTabBuffer << ".rela.text" << '\x00';
  SHStrTabBuffer << ".shstrtab" << '\x00';
}

void MCContext::mkSymtab() {
  /// begin with none (local)
  Elf64_Sym symbol = {};
  Elf_Syms.emplace_back(std::string{}, std::move(symbol));

  /// sections(local)
  auto sections = std::array{"", ".text", ".data", ".bss"};
  for (size_ty i = 1; i < sections.size(); ++i) {
    Elf64_Sym symbol = {};
    auto& SectionName = sections[i];

    // symbol.st_name = SHStrTabBuffer.findOffset(SectionName);
    symbol.st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
    symbol.st_other = ELF64_ST_VISIBILITY(STV_DEFAULT);
    symbol.st_shndx = i;

    Elf_Syms.emplace_back(std::string{SectionName}, std::move(symbol));
    ++local_syms;
  }

  /// labels(local)
  for (const auto& label : TextLabels.keys()) {

    if (std::find_if(Symbols.begin(), Symbols.end(), [&](const auto& tuple) {
          return label == std::get<0>(tuple);
        }) != Symbols.end()) {
      /// if a label will be declare as global, then avoid declare as local
      continue;
    }

    Elf64_Sym symbol = {};

    symbol.st_name = StrTabBuffer.findOffset(label);
    symbol.st_value = *TextLabels.find(label);
    symbol.st_info = ELF64_ST_INFO(STB_LOCAL, STT_NOTYPE);
    symbol.st_other = ELF64_ST_VISIBILITY(STV_DEFAULT);
    symbol.st_shndx = std::distance(
        sections.begin(), std::find(sections.begin(), sections.end(), ".text"));

    Elf_Syms.emplace_back(std::string{label}, std::move(symbol));
    ++local_syms;
  }

  /// relo symbols(local)
  for (const auto& [sym, offset, ndx] : Symbols) {
    Elf64_Sym symbol = {};

    symbol.st_name = StrTabBuffer.findOffset(sym);
    symbol.st_value = offset;
    symbol.st_info = ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE);
    symbol.st_other = ELF64_ST_VISIBILITY(STV_DEFAULT); // visibility
    symbol.st_shndx = ndx;

    Elf_Syms.emplace_back(std::string{sym}, std::move(symbol));
  }

  /// extern syms
  for (const auto& sym : ExternSymbols) {
    Elf64_Sym symbol = {};

    symbol.st_name = StrTabBuffer.findOffset(sym);
    symbol.st_info = ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE);
    symbol.st_other = ELF64_ST_VISIBILITY(STV_DEFAULT); // visibility
    symbol.st_shndx = und;

    Elf_Syms.emplace_back(std::string{sym}, std::move(symbol));
  }
}

/// FIXME: when use pseudo (not explicitly use %...), need to fix the relo kind
/// according to symbol itself
void MCContext::Relo() {

  for (auto [inst, sym] : ReloInst) {
    Elf64_Rela Rela{};
    Rela.r_offset = inst->getOffset();

    /// find symtab index in Elf_Syms
    auto SymTblIdx = std::distance(
        Elf_Syms.begin(), std::find_if(Elf_Syms.begin(), Elf_Syms.end(),
                                       [&](const auto& symEntry) {
                                         return symEntry.first == sym;
                                       }));

    Rela.r_info = ELF64_R_INFO(SymTblIdx, inst->getRiscvRType());

    Rela.r_addend = inst->getExprOp()->getExpr()->getAddend();

    if (MCExpr::isStaticOffset(inst->getExprTy()))
      inst->reloSym(*TextLabels.find(sym) -
                    inst->getOffset()); // offset of the label
    else
      inst->reloSym(0ll);

    Elf_Relas.emplace_back(std::move(Rela));

    /// Pesudo unpacked insts
    if (inst->isRelaxable()) {
      Elf_Relas.emplace_back(
          Elf64_Rela{.r_offset = 0,
                     .r_info = ELF64_R_INFO(0, R_RISCV_RELAX),
                     .r_addend = 0});
    }
  }
}

void MCContext::Ehdr_Shdr() {
  auto& hdr = this->Elf_Ehdr;

  /// magic number
  std::memcpy(hdr.e_ident,
              "\x7f"
              "ELF",
              4);
  hdr.e_ident[EI_CLASS] = ELFCLASS64;
  hdr.e_ident[EI_DATA] = ELFDATA2LSB;
  hdr.e_ident[EI_VERSION] = EV_CURRENT;
  hdr.e_ident[EI_OSABI] = ELFOSABI_NONE; // none

  hdr.e_type = ET_REL;
  hdr.e_machine = EM_RISCV;
  hdr.e_version = EV_CURRENT;
  hdr.e_flags = EF_RISCV_RVC | EF_RISCV_FLOAT_ABI_DOUBLE;
  hdr.e_ehsize = sizeof(std::decay_t<decltype(hdr)>);
  hdr.e_shentsize = sizeof(Elf64_Shdr);

  /// TODO: .section and more sections
  /// <void> .text .data .bss .strtab .symtab .rela_text .shstrtab
  hdr.e_shnum = 8;
  hdr.e_shstrndx = 7;

  /// estimate the offset to the section header table
  size_ty offset = 0;
  auto mkAlign = [&](size_ty alignment) {
    offset += (alignment - (offset % alignment)) % alignment;
  };

  {
    Offsets.insert("elf header", 0);
    offset += sizeof(std::decay_t<decltype(hdr)>);
  }

  {
    mkAlign(2);
    Offsets.insert(".text", offset);
    offset += TextOffset;
  }

  {
    mkAlign(1);
    Offsets.insert(".data", offset);
    offset += DataBuffer.size();
  }

  {
    mkAlign(1);
    Offsets.insert(".bss", offset);
    /// readelf: Section '.bss' has no data to dump.
  }

  {
    mkAlign(8);
    Offsets.insert(".symtab", offset);

    offset += (Elf_Syms.size()) * sizeof(Elf64_Sym);
  }

  {
    mkAlign(1);
    Offsets.insert(".strtab", offset);

    offset += StrTabBuffer.size();
  }

  {
    mkAlign(8);
    Offsets.insert(".rela.text", offset);
    offset += (Elf_Relas.size()) * sizeof(Elf64_Rela);
  }

  {
    mkAlign(1);
    Offsets.insert(".shstrtab", offset);

    offset += SHStrTabBuffer.size();
  }

  {
    mkAlign(8);
    Offsets.insert("section header table", offset);
  }

  {
    hdr.e_shoff = offset;

    /// empty section hdr
    {
      Elf64_Shdr shdr = {};
      shdr.sh_type = SHT_NULL;
      Elf_Shdrs.emplace_back(std::move(shdr));
    }

    auto SectionHeader = [&](StringRef name, uint32_t type, uint64_t flag,
                             uint64_t size, uint64_t alignment,
                             uint32_t link = 0, uint32_t info = 0,
                             uint64_t entsize = 0) {
      Elf64_Shdr shdr = {};

      auto offset = Offsets.find(name);
      utils_assert(offset, "cant find offset of this section");

      shdr.sh_name = SHStrTabBuffer.findOffset(name);
      shdr.sh_type = type;
      shdr.sh_flags = flag;
      shdr.sh_addr = 0;
      shdr.sh_offset = *offset;
      shdr.sh_size = size;
      shdr.sh_addralign = alignment;

      shdr.sh_entsize = entsize;
      shdr.sh_link = link;
      shdr.sh_info = info;

      Elf_Shdrs.emplace_back(std::move(shdr));
    };

    /// .text 1
    SectionHeader(".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR, TextOffset,
                  2);

    /// .data 2
    SectionHeader(".data", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE,
                  DataBuffer.size(), 1);

    /// .bss 3
    SectionHeader(".bss", SHT_NOBITS, SHF_ALLOC | SHF_WRITE, 0, 1);

    /// .symtab 4: link to .strtab
    SectionHeader(".symtab", SHT_SYMTAB, 0,
                  this->Elf_Syms.size() * sizeof(Elf64_Sym), 8, 5, local_syms,
                  sizeof(Elf64_Sym));

    /// .strtab 5
    SectionHeader(".strtab", SHT_STRTAB, 0, StrTabBuffer.size(), 1);

    /// .rela.text 6: link to .symtab
    SectionHeader(".rela.text", SHT_RELA, SHF_INFO_LINK,
                  Elf_Relas.size() * sizeof(Elf64_Rela), 8, 4, 1,
                  sizeof(Elf64_Sym));

    /// .shstrtab
    SectionHeader(".shstrtab", SHT_STRTAB, 0, SHStrTabBuffer.size(), 1);
  }
}

void MCContext::writein() {

  this->mkStrTab();

  this->mkShStrTab();

  this->mkSymtab();

  this->Relo();

  this->Ehdr_Shdr();

  size_ty curOffset = 0;

  auto streamWriteIn = [&](const char* data, size_ty n) {
    this->file.write(data, n);
    curOffset += n;
  };

  auto padSection = [&](size_ty offset) {
    /// pad current size to offset
    std::vector<char> buffer(offset - curOffset, '\x00');
    streamWriteIn(buffer.data(), buffer.size());
  };

  /// elf header
  {
    streamWriteIn((char*)&this->Elf_Ehdr, sizeof(Elf64_Ehdr));
  }

  /// .text
  {
    padSection(*this->Offsets.find(".text"));

    for (const auto& inst : this->Insts) {
      auto encode = inst.makeEncoding();
      streamWriteIn((char*)&encode, inst.isCompressed() ? 2 : 4);
    }
  }

  /// .data
  {
    padSection(*this->Offsets.find(".data"));
    streamWriteIn((const char*)DataBuffer.data(), DataBuffer.size());
  }

  /// .bss
  {
    padSection(*this->Offsets.find(".bss"));
    /// readelf: Section '.bss' has no data to dump.
  }

  /// .symtab
  {
    padSection(*this->Offsets.find(".symtab"));

    for (auto& [_, symbol] : this->Elf_Syms) {
      streamWriteIn((char*)&symbol, sizeof(Elf64_Sym));
    }
  }

  /// .strtab
  {
    padSection(*this->Offsets.find(".strtab"));
    streamWriteIn((const char*)StrTabBuffer.data(), StrTabBuffer.size());
  }

  /// .rela.text
  {
    padSection(*this->Offsets.find(".rela.text"));

    for (auto& relo : this->Elf_Relas) {
      streamWriteIn((char*)&relo, sizeof(Elf64_Rela));
    }
  }

  /// .shstrtab
  {
    padSection(*this->Offsets.find(".shstrtab"));
    streamWriteIn((const char*)SHStrTabBuffer.data(), SHStrTabBuffer.size());
  }

  /// dump section headers
  {
    padSection(*this->Offsets.find("section header table"));

    for (auto& shdr : Elf_Shdrs) {
      streamWriteIn((char*)&shdr, sizeof(Elf64_Shdr));
    }
  }
}