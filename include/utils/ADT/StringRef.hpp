#ifndef UTILS_ADT_STRINGREF
#define UTILS_ADT_STRINGREF

#include "ArrayRef.hpp"
#include "SmallVector.hpp"
#include "iterator_range.hpp"
#include "utils/macro.hpp"
#include "utils/memory.hpp"
#include <array>
#include <cassert>
#include <cstddef>
#include <cstdio>
#include <cstring>
#include <iterator>
#include <ostream>
#include <string>
#include <string_view>
#include <type_traits>
namespace utils {
namespace ADT {

template <typename T>
concept IsIntegralPtr =
    std::conjunction_v<std::is_pointer<T>,
                       std::is_integral<std::remove_pointer_t<T>>>;

class StringRef {
public:
  using iter = const char*;
  using const_iter = const char*;
  using size_ty = size_t;
  using value_ty = char;
  using rev_iter = std::reverse_iterator<iter>;
  using const_rev_iter = std::reverse_iterator<const_iter>;

private:
  const char* Data = nullptr;
  std::size_t Length = 0;

public:
  StringRef() = default;

  constexpr StringRef(std::string_view Str)
      : Data(Str.data()), Length(Str.size()) {}

  constexpr StringRef(const std::string& Str LIFETIME_BOUND)
      : Data(Str.data()), Length(Str.size()) {}

  constexpr StringRef(const char* data LIFETIME_BOUND, std::size_t length)
      : Data(data), Length(length) {}

  constexpr StringRef(const char* Str LIFETIME_BOUND)
      : StringRef(Str ? std::string_view(Str) : std::string_view()) {}

  constexpr StringRef(StringRef&& StrRef)
      : Data(StrRef.Data), Length(StrRef.Length) {}

  constexpr StringRef(const StringRef& StrRef)
      : Data(StrRef.Data), Length(StrRef.Length) {}

  template <typename T> constexpr StringRef& operator=(T&& StrRef) {
    static_assert(
        std::is_same_v<std::remove_reference_t<T>, StringRef> ||
        std::is_same_v<std::remove_reference_t<T>, std::string_view> ||
        std::is_same_v<std::remove_reference_t<T>, const std::string&> ||
        std::is_same_v<std::remove_reference_t<T>, const char*>);

    if constexpr (std::is_same_v<std::remove_reference_t<T>, StringRef>) {
      Data = StrRef.Data;
      Length = StrRef.Length;
    } else if constexpr (std::is_same_v<std::remove_reference_t<T>,
                                        std::string_view>) {
      Data = StrRef.data();
      Length = StrRef.size();
    } else if constexpr (std::is_same_v<std::remove_reference_t<T>,
                                        const std::string&>) {
      Data = StrRef.data();
      Length = StrRef.size();
    } else if constexpr (std::is_same_v<std::remove_reference_t<T>,
                                        const char*>) {
      Data = StrRef;
      Length = StrRef ? std::strlen(StrRef) : 0;
    }

    return *this;
  }

  [[nodiscard]] constexpr const char* data() const { return Data; }
  [[nodiscard]] constexpr size_ty size() const { return Length; }
  [[nodiscard]] constexpr bool empty() const { return !Length; }

  std::string str() const { return std::string(data(), size()); }
  constexpr std::string_view str_view() const {
    return std::string_view(data(), size());
  }
  constexpr const char* c_str() const { return data(); }

  /// @}
  /// @name Iters
  /// @{

  constexpr iter begin() const { return data(); }

  constexpr iter end() const { return data() + size(); }

  constexpr rev_iter rbegin() const {
    return std::make_reverse_iterator(end());
  }

  constexpr rev_iter rend() const {
    return std::make_reverse_iterator(begin());
  }

  const unsigned char* bytes_begin() const {
    return reinterpret_cast<const unsigned char*>(begin());
  }
  const unsigned char* bytes_end() const {
    return reinterpret_cast<const unsigned char*>(end());
  }
  iterator_range<const unsigned char*> bytes() const {
    return makeRange(bytes_begin(), bytes_end());
  }

  [[nodiscard]] char front() const {
    assert(!empty());
    return data()[0];
  }
  [[nodiscard]] char back() const {
    assert(!empty());
    return data()[size() - 1];
  }

  /// c-style char array
  template <std::size_t N>
  [[nodiscard]] constexpr bool operator==(const char (&Array)[N]) const {
    if (this->size() != N) {
      return false;
    }

    return !utils::memcmp(this->data(), Array, N);
  }

  /// check if the String the same
  template <typename T>
    requires ViewType<T, std::decay_t<decltype(Data)>>
  [[nodiscard]] bool operator==(T&& StrRef) const {
    if (StrRef.size() != this->size()) {
      return false;
    }

    return !utils::memcmp(this->data(), StrRef.data(), this->size());
  }

  template <typename T>
    requires ViewType<T, std::decay_t<decltype(Data)>>
  [[nodiscard]]
  bool same_instance(T&& StrRef) {
    return this->data() == StrRef.data(); // no size check
  }

  template <std::size_t N>
  [[nodiscard]] bool same_instance(const char (&Array)[N]) const {
    return N == this->size() && this->data() == Array;
  }

  [[nodiscard]] char operator[](std::size_t index) const {
    assert(index < this->size());
    return data()[index];
  }

  friend std::ostream& operator<<(std::ostream& os LIFETIME_BOUND,
                                  const StringRef& Str) {
    os << Str.str();
    return os;
  }

  template <std::size_t N>
  constexpr std::array<StringRef, N> split(char splitor) const {

    if (this->empty()) {
      return {};
    }

    char const* current = this->data();
    char const* last = current;

    std::array<StringRef, N> elements{};

    unsigned cnt = 0;
    while (*current != '\0' && current < end()) {

      if (*current == splitor) {
        if (current != last) {
          // avoid empty StringRef
          // which may happen when multi splitor or never trim
          elements[cnt++] = std::move(StringRef(last, current - last));
        }

        last = current += 1;
      } else {
        ++current;
      }
    }

    if (current != last && *last != splitor) {
      elements[cnt++] = std::move(StringRef(last, current - last));
    }

    return elements;
  }

  [[nodiscard]] constexpr StringRef
  slice(std::size_t Start,
        std::size_t End = std::numeric_limits<std::size_t>::max()) const {
    Start = std::min(Start, size());
    End = std::clamp(End, Start, size());
    return StringRef(data() + Start, End - Start);
  }

  [[nodiscard]] constexpr bool begin_with(StringRef Prefix) const {
    return size() >= Prefix.size() &&
           utils::memcmp(begin(), Prefix.data(), Prefix.size()) == 0;
  }
  [[nodiscard]] constexpr bool begin_with(char Prefix) const {
    return !empty() && front() == Prefix;
  }

  [[nodiscard]] constexpr bool end_with(StringRef Suffix) const {
    return size() >= Suffix.size() &&
           utils::memcmp(end() - Suffix.size(), Suffix.data(), Suffix.size()) ==
               0;
  }
  [[nodiscard]] constexpr bool ends_with(char Suffix) const {
    return !empty() && back() == Suffix;
  }

  /// you should handle template types and you format string carefully to
  /// avoid ub
  template <IsIntegralPtr... ArgTys>
  void sscan(StringRef format, ArgTys... Args) const {

    // expect Args to be a pack of ptrs
    std::sscanf(data(), format.Data, Args...);
  }
};

[[nodiscard]] inline bool operator==(const StringRef& StrRef0,
                                     const StringRef& StrRef1) {
  if (StrRef0.size() != StrRef1.size()) {
    return false;
  }
  return utils::memcmp(StrRef0.data(), StrRef1.data(), StrRef0.size()) == 0;
}

} // namespace ADT
} // namespace utils

#endif