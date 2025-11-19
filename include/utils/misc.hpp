#ifndef UTILS_NUMERIC
#define UTILS_NUMERIC

#include "macro.hpp"
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <optional>
namespace utils {

template <bool LeftBounded = true, bool RightBounded = false, typename T>
constexpr inline bool in_interval(T left, T right, T val) {

  std::less<T> LessThan;
  std::equal_to<T> Equal;

  if (LessThan(left, val) && LessThan(val, right)) {
    return true;
  }

  bool res = false;

  if constexpr (LeftBounded) {
    res |= Equal(val, left);
  }

  if constexpr (RightBounded) {
    res |= Equal(val, right);
  }

  return res;
}

template <typename T, typename... Args>
constexpr inline bool in_set(T Value, const Args&... Enum) {
  static_assert(sizeof...(Args) != 0, "At least provide one Enum value");

  return ((Value == Enum) || ...);
}

template <typename Tx, typename Ty, typename Tuple, std::size_t... I>
constexpr Ty in_set_map_impl(Tx value, const Tuple& tuple,
                             std::index_sequence<I...>) {
  Ty result = static_cast<Ty>(0);

  (void)((value == std::get<2 * I>(tuple)
              ? (result = std::get<2 * I + 1>(tuple), true)
              : true) &&
         ...);

  return result;
}

template <typename Tx, typename Ty, typename... Args>
constexpr inline Ty in_set_map(Tx value,
                               Args&&... Enums) { /* Tx, Ty, Tx, Ty... */
  static_assert(sizeof...(Args) != 0 && sizeof...(Args) % 2 == 0,
                "Must provide even number of arguments");

  auto tuple = std::make_tuple(std::forward<Args>(Enums)...);
  constexpr std::size_t N = sizeof...(Args);

  return in_set_map_impl<Tx, Ty>(value, tuple,
                                 std::make_index_sequence<N / 2>{});
}

template <typename Tuple, std::size_t... I>
constexpr bool pairs_equal_impl(const Tuple& tuple, std::index_sequence<I...>) {
  return ((std::get<2 * I>(tuple) == std::get<2 * I + 1>(tuple)) && ...);
}

template <typename... Args> constexpr inline bool pairs_equal(Args&&... args) {
  static_assert(sizeof...(Args) != 0 && sizeof...(Args) % 2 == 0,
                "Must provide even number of arguments");
  auto tuple = std::make_tuple(std::forward<Args>(args)...);
  constexpr std::size_t N = sizeof...(Args);
  return pairs_equal_impl(tuple, std::make_index_sequence<N / 2>{});
}

template <typename R>
inline auto iter_size(
    R&& Range,
    std::enable_if_t<
        std::is_base_of<std::random_access_iterator_tag,
                        typename std::iterator_traits<
                            decltype(Range.begin())>::iterator_category>::value,
        void>* = nullptr) {
  return std::distance(Range.begin(), Range.end());
}

/// TODO: replace return value with std::optional
constexpr inline int stoi(const char* str, std::size_t len, int base = 10) {
  if (base < 2 || base > 36) {
    return -1;
  }

  if (!str) {
    return -2;
  }

  int result = 0;
  bool is_negative = false;
  size_t i = 0;

  if (str[i] == '-') {
    is_negative = true;
    ++i;
  }

  for (; i < len; ++i) {
    char ch = str[i];
    int digit = 0;

    if (ch >= '0' && ch <= '9') {
      digit = ch - '0';
    } else if (ch >= 'a' && ch <= 'z') {
      digit = ch - 'a' + 10;
    } else if (ch >= 'A' && ch <= 'Z') {
      digit = ch - 'A' + 10;
    } else {
      ///
      break;
    }

    if (digit >= base) {
      return -4;
    }

    result = result * base + digit;
  }

  return is_negative ? -result : result;
}

inline int popcounter_wrapper(unsigned short vic) {
  return __builtin_popcount(static_cast<unsigned>(vic));
}

inline int popcounter_wrapper(unsigned vic) { return __builtin_popcount(vic); }

inline int popcounter_wrapper(int vic) { return __builtin_popcount(vic); }

inline int popcounter_wrapper(unsigned long long vic) {
  return __builtin_popcountll(vic);
}

inline int popcounter_wrapper(uint64_t vic) {
  return __builtin_popcountll(vic);
}

inline int clz_wrapper(unsigned short vic) {
#if defined(__clang__)
  return vic ? __builtin_clzs(vic) : 16;
#else
  return vic ? __builtin_ctz((unsigned int)vic) : 16;
#endif
}

inline int ctz_wrapper(unsigned short vic) {
#if defined(__clang__)
  return vic ? __builtin_ctzs(vic) : 0;
#else
  return vic ? __builtin_ctz((unsigned int)vic) : 0;
#endif
}

inline int clz_wrapper(unsigned vic) { return vic ? __builtin_clz(vic) : 32; }

inline int ctz_wrapper(unsigned vic) { return vic ? __builtin_ctz(vic) : 0; }

inline int clz_wrapper(int vic) { return vic ? __builtin_clz(vic) : 32; }

inline int ctz_wrapper(int vic) { return vic ? __builtin_ctz(vic) : 0; }

inline int clz_wrapper(unsigned long long vic) {
  return vic ? __builtin_clzll(vic) : 64;
}

inline int ctz_wrapper(unsigned long long vic) {
  return vic ? __builtin_ctzll(vic) : 0;
}

inline int clz_wrapper(uint64_t vic) { return vic ? __builtin_clzll(vic) : 64; }

inline int ctz_wrapper(uint64_t vic) { return vic ? __builtin_ctzll(vic) : 0; }

inline std::optional<int> log2(std::size_t Value) {
  if (popcounter_wrapper(Value) != 1) {
    return std::nullopt;
  } else {
    return ctz_wrapper(Value);
  }
}

inline std::size_t pow2i(std::size_t x) { return __builtin_powi(x, 2); }

// 12 + 1 / 20 + 1
template <unsigned N> inline uint64_t signIntCompress(uint64_t integer) {
  int64_t signed_integer = *reinterpret_cast<int64_t*>(&integer);

  if (signed_integer >= 0) {
    utils_assert(signed_integer < std::pow(2, N - 1), "size limit excessed");

    return integer;
  } else {
    auto pad = static_cast<uint64_t>(-1) << N;
    utils_assert((integer & pad) == pad, "size limit excessed");

    return integer & static_cast<uint64_t>(-1) >> (64 - N);
  }
}

inline uint64_t signIntCompress(uint64_t integer, unsigned size) {
  int64_t signed_integer = *reinterpret_cast<int64_t*>(&integer);

  if (signed_integer >= 0) {
    // utils_assert(signed_integer < std::pow(2, size) - 1, "size limit
    // excessed");

    return integer;
  } else {
    auto pad = static_cast<uint64_t>(-1) << size;
    utils_assert((integer & pad) == pad, "size limit excessed");

    return integer & static_cast<uint64_t>(-1) >> (64 - size);
  }
}

inline uint32_t signIntSlice(uint64_t interger, unsigned high, unsigned low) {
  return (interger & (1ull << (high + 1)) - 1) >> low;
}

} // namespace utils

#endif