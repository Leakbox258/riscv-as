#ifndef UTILS_ADT_BYTESTREAM
#define UTILS_ADT_BYTESTREAM

#include "SmallVector.hpp"
#include "StringRef.hpp"
#include <string>

namespace utils {
namespace ADT {

struct ByteStream {
  using size_ty = std::size_t;

  std::vector<uint8_t> buffer;

  explicit ByteStream() : buffer() { buffer.reserve(4096); }

  const unsigned char* data() const { return buffer.data(); }

  size_ty size() const { return buffer.size(); }

  void balignTo(size_ty balign) {
    auto paddingSize = (balign - (buffer.size() % balign)) % balign;

    for (auto i = 0ull; i < paddingSize; ++i) {
      buffer.push_back('\x00');
    }
  }

  template <IsPOD T> ByteStream& operator<<(T&& Value) {

    this->balignTo(sizeof(T));

    for (unsigned long i = 0; i < sizeof(T); ++i) {
      buffer.push_back(((uint8_t*)(&Value))[i]);
    }

    return *this;
  }

  template <size_ty Num> ByteStream& operator<<(const char (&Value)[Num]) {
    for (size_ty i = 0; i < Num; ++i) {
      buffer.push_back(Value[i]);
    }

    return *this;
  }

  ByteStream& operator<<(const std::string& Value) {
    for (size_ty i = 0; i < Value.size(); ++i) {
      buffer.push_back(Value[i]);
    }

    return *this;
  }

  /// for .strtab / .shstrtab
  /// WARNING: arg0 must end with '\x00'
  size_ty findOffset(StringRef Str) const;

  /// WARNING: arg0 must end with '\x00'
  bool hasSym(StringRef Str) const;

  /// for debug
  void dump() const;

}; // namespace ADT
} // namespace ADT
} // namespace utils
#endif
