#include "utils/ADT/ByteStream.hpp"

using namespace utils::ADT;

ByteStream::size_ty ByteStream::findOffset(StringRef Str) const {
  size_ty offset = 0;
  for (const auto& chr : buffer) {

    if (chr == *Str.begin()) {
      if (!std::memcmp(Str.data(), buffer.data() + offset, Str.size() + 1)) {
        return offset;
      }
    }

    ++offset;
  }

  utils::unreachable("cant find Str in ByteStream");
};

bool ByteStream::hasSym(StringRef Str) const {
  size_ty offset = 0;
  for (const auto& chr : buffer) {

    if (chr == *Str.begin()) {
      if (!std::memcmp(Str.data(), buffer.data() + offset, Str.size() + 1)) {
        return true;
      }
    }

    ++offset;
  }

  return false;
};

void ByteStream::dump() const {
  for (const auto& chr : buffer) {
    if (chr != '\x00')
      std::printf("%c", chr);
    else
      std::printf(".");
  }
}