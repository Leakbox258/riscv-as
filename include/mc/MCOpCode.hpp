#ifndef MC_OPCODE
#define MC_OPCODE

#include "utils/ADT/SmallVector.hpp"
#include "utils/ADT/StringRef.hpp"
#include "utils/ADT/StringSwitch.hpp"
#include "utils/misc.hpp"
#include <cassert>
#include <cstdint>
#include <optional>
#include <tuple>

using StringRef = utils::ADT::StringRef;
template <typename T> using StringSwitch = utils::ADT::StringSwitch<T>;
template <typename T, std::size_t N>
using SmallVector = utils::ADT::SmallVector<T, N>;

namespace {
/// constexpr and marco helpers, dont use them anywhere else

constexpr char toLower(char c) {
  return (c >= 'A' && c <= 'Z') ? c + ('a' - 'A') : c;
}

template <size_t N>
constexpr std::array<char, N> processMnemonic(const char (&mnemonic)[N]) {
  std::array<char, N> result{};
  for (size_t i = 0; i < N; ++i) {
    char c = mnemonic[i];
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

struct EnCoding {
  enum EnCodeTy : uint16_t {
    kInvalid,
    kStatic, // opcode or padding
    kImm,
    kNzImm,
    kUImm,
    kRd,
    kRs1,
    kRs2,
    kRs3,
    kRd_short,
    kRs1_short,
    kRs2_short,
    kRs3_short,
    kRm, // rounding mode (F/D extension)
    /// TODO: r, w, io
    kMemFence,
    // AQ or RL will concat with mnemonic in A extension
    // eg: LR.D.AQ t1, (s0)
  };

  EnCodeTy kind = kInvalid;
  unsigned length = 0;
  unsigned highest = 0;

  std::optional<std::array<std::pair<uint16_t, uint16_t>, 8>> bit_range;
  std::optional<uint16_t> static_pattern;
};

struct MCOpCode {
private:
  constexpr static std::tuple<std::array<std::pair<uint16_t, uint16_t>, 8>,
                              unsigned, unsigned>
  parseBitRange(StringRef BitRange) {
    // 4:0
    // 20|10:1|11|19:12

    std::array<std::pair<uint16_t, uint16_t>, 8> bit_range{};

    auto concat_range = BitRange.split<8>('|');

    unsigned length = 0;
    unsigned highest = 0;
    unsigned idx = 0;

    for (auto rit = concat_range.rbegin(); rit != concat_range.rend(); ++rit) {
      auto range = *rit;

      if (range.empty()) {
        continue;
      }

      /// NOTE: for constexpr, std::array need a more
      /// large range
      auto ranges = range.split<8>(':');

      // `low` will probably be empty
      auto high = ranges[0];
      auto low = ranges[1].empty() ? ranges[0] : ranges[1];

      auto high_bit = utils::stoi(high.c_str(), high.size());
      auto low_bit = utils::stoi(low.c_str(), low.size());

      bit_range[idx++] = {high_bit, low_bit};
      length += high_bit - low_bit + 1;
      highest = static_cast<unsigned>(high_bit) > highest ? high_bit : highest;
    }

    return {bit_range, length, highest};
  }

public:
  StringRef name;

  /// 0 means no imm, 1 means consistent, otherwise imm will be splited
  unsigned imm_distribute = 0;

  bool hasRd = false;

  /// NOTE: for constexpr, std::array need a more
  /// large range than 6 which is more ideal
  std::array<EnCoding, 8> encodings;

  /// not sure whether is constexpr
  constexpr MCOpCode(const char* InstName, const char* Pattern)
      : name(InstName) {
    /// EG: offset[11:0] rs1[4:0] 011 rd[4:0] 0000011

    /// NOTE: for constexpr, std::array need a more
    /// large range than 6 which is more ideal
    auto raw_patterns = StringRef(Pattern).split<8>(' ');

    unsigned cnt = 0;
    for (auto raw_pattern_it = raw_patterns.rbegin();
         raw_pattern_it != raw_patterns.rend(); ++raw_pattern_it) {

      auto& raw_pattern = *raw_pattern_it;

      EnCoding encoding =
          StringSwitch<EnCoding>(raw_pattern)
              .BeginWith("offset",
                         [this](const StringRef& Str) -> EnCoding {
                           /// NOTE: for constexpr, std::array need a
                           /// more large range than 2 which is more ideal
                           ++imm_distribute;

                           auto [bit_range, length, highest] =
                               parseBitRange(Str.slice(7, Str.size() - 1));

                           return EnCoding{EnCoding::kImm, length, highest,
                                           bit_range, std::nullopt};
                         })
              .BeginWith("imm",
                         [this](const StringRef& Str) -> EnCoding {
                           ++imm_distribute;

                           /// NOTE: for constexpr, std::array need a
                           /// more large range than 2 which is more ideal
                           auto [bit_range, length, highest] =
                               parseBitRange(Str.slice(4, Str.size() - 1));

                           return EnCoding{EnCoding::kImm, length, highest,
                                           bit_range, std::nullopt};
                         })
              .BeginWith("nzimm",
                         [this](const StringRef& Str) -> EnCoding {
                           ++imm_distribute;
                           /// NOTE: for constexpr, std::array need a
                           /// more large range than 2 which is more ideal
                           auto [bit_range, length, highest] =
                               parseBitRange(Str.slice(6, Str.size() - 1));

                           return EnCoding{EnCoding::kImm, length, highest,
                                           bit_range, std::nullopt};
                         })
              .BeginWith("uimm",
                         [this](const StringRef& Str) -> EnCoding {
                           ++imm_distribute;
                           /// NOTE: for constexpr, std::array need a
                           /// more large range than 2 which is more ideal
                           auto [bit_range, length, highest] =
                               parseBitRange(Str.slice(5, Str.size() - 1));

                           return EnCoding{EnCoding::kImm, length, highest,
                                           bit_range, std::nullopt};
                         })
              .BeginWith("rd_",
                         [this](const StringRef& Str) -> EnCoding {
                           // [2:0]
                           auto [bit_range, length, highest] =
                               parseBitRange(Str.slice(4, Str.size() - 1));

                           hasRd = true;

                           return EnCoding{EnCoding::kRd_short, length, highest,
                                           bit_range, std::nullopt};
                         })
              .BeginWith("rd",
                         [this](const StringRef& Str) -> EnCoding {
                           // [4:0]
                           auto [bit_range, length, highest] =
                               parseBitRange(Str.slice(3, Str.size() - 1));

                           hasRd = true;

                           return EnCoding{EnCoding::kRd, length, highest,
                                           bit_range, std::nullopt};
                         })
              .BeginWith(
                  "rs",
                  [](const StringRef& Str) -> EnCoding {
                    int idx = utils::stoi(Str.c_str() + 2, 1); // rsx

                    // [4:0]
                    auto [bit_range, length, highest] = parseBitRange(Str.slice(
                        *(Str.c_str() + 3) == '_' ? 5 : 4, Str.size() - 1));

                    uint16_t kind = idx + (*(Str.c_str() + 3) == '_' ? 9 : 5);

                    return EnCoding{EnCoding::EnCodeTy(kind), length, highest,
                                    bit_range, std::nullopt};
                  })
              .BeginWith("rm",
                         [](const StringRef& Str) -> EnCoding {
                           // [2:0]
                           auto [bit_range, length, highest] =
                               parseBitRange(Str.slice(3, Str.size() - 1));

                           return EnCoding{EnCoding::kRd, length, highest,
                                           bit_range, std::nullopt};
                         })
              .BeginWith("pred",
                         [](const StringRef& Str) -> EnCoding {
                           // [3:0]
                           auto [bit_range, length, highest] =
                               parseBitRange(Str.slice(5, Str.size() - 1));

                           return EnCoding{EnCoding::kMemFence, length, highest,
                                           bit_range, std::nullopt};
                         })
              .BeginWith("succ",
                         [](const StringRef& Str) -> EnCoding {
                           // [3:0]
                           auto [bit_range, length, highest] =
                               parseBitRange(Str.slice(5, Str.size() - 1));

                           return EnCoding{EnCoding::kMemFence, length, highest,
                                           bit_range, std::nullopt};
                         })
              .Default([](const StringRef& Str) -> EnCoding {
                return EnCoding{
                    EnCoding::kStatic, static_cast<unsigned int>(Str.size()), 0,
                    std::nullopt, utils::stoi(Str.c_str(), Str.size(), 2)};
              });

      encodings[cnt++] = encoding;
    }
  }
};

#define ASM(name, pattern)                                                     \
  inline constexpr char _##name[] = #name;                                     \
  inline constexpr char _##name##_Pattern[] = #pattern;                        \
  static constexpr MCOpCode name{_##name, _##name##_Pattern};

#define INSTRUCTION(name, pattern) ASM(name, pattern)
#include "RISCV.def"
#undef INSTRUCTION

} // namespace mc

namespace parser {

#define MNEMONIC(name) processMnemonic(#name)

/// NOTE: tuple<pair<array<N>, MCOpCode*>, ...>
/// NOTE: avoid using std::make_tuple(), because of tailing comma
#define INSTRUCTION(name, pattern) std::make_pair(MNEMONIC(name), &mc::name),

constexpr inline auto MnemonicMap = std::tuple{
#include "RISCV.def"
};

#undef INSTRUCTION

template <size_t N>
constexpr bool MnemonicContainImpl(const std::array<char, N>& arr,
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
constexpr bool MnemonicContain(const T& value) {
  if constexpr (I < std::tuple_size_v<decltype(MnemonicMap)>) {
    if (MnemonicContainImpl(std::get<I>(MnemonicMap).first, value)) {
      return true;
    }
    return MnemonicContain<I + 1>(value);
  }
  return false;
}

template <size_t I = 0, typename T>
constexpr const mc::MCOpCode* MnemonicFind(const T& value) {
  if constexpr (I < std::tuple_size_v<decltype(MnemonicMap)>) {
    if (MnemonicContainImpl(std::get<I>(MnemonicMap).first, value)) {
      return std::get<I>(MnemonicMap).second;
    }
    return MnemonicFind<I + 1>(value);
  }
  return nullptr;
}

} // namespace parser

#endif