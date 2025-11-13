#ifndef UTILS_LISP_LISP
#define UTILS_LISP_LISP

#include "mc/MCOpCode.hpp"
#include <cstdint>

namespace utils {
namespace lisp {

/// @note use someof astNode as the form of eval
struct LispNode {
  LispNode* fa = nullptr;
  std::array<LispNode*, 4> sons{};
  uint32_t sonNr;

  constexpr uint32_t push(LispNode* son) {
    sons[sonNr] = son;
    return ++sonNr;
  }

  /// contents
  StringRef content;
};

/// @note a `constexpr` simple list processor parser
class Lisp {

private:
  StringRef sourceCode;

  std::array<LispNode, 16> tree{};
  uint32_t nodeCnt = 0;
  LispNode* curRoot = &tree[0];

public:
  Lisp() = delete;
  Lisp(Lisp&&) = delete;
  Lisp(const Lisp&&) = delete;
  Lisp& operator=(Lisp&&) = delete;
  Lisp& operator=(const Lisp&) = delete;

  constexpr Lisp(const StringRef _source) : sourceCode(_source) { parse(); }

  constexpr LispNode* getRoot() { return curRoot; }

private:
  constexpr void parse() {
    auto tokens = sourceCode.split<128>(' ');
    uint32_t cnt = 0;

    while (!tokens[cnt].empty()) {
      StringRef curToken = tokens[cnt++];

      if (curToken == "(") {
        curRoot->push(&tree[++nodeCnt]);
        tree[nodeCnt].fa = curRoot;
        curRoot = &tree[nodeCnt];
        continue;
      } else if (curToken == ")") {
        curRoot = curRoot->fa;
        continue;
      }

      if (curRoot->content.empty()) {
        /// Operator
        curRoot->content = curToken;
      } else {
        /// if is not begin with '(', then this will be self-eval items
        curRoot->push(&tree[++nodeCnt]);
        tree[nodeCnt].fa = curRoot;
        tree[nodeCnt].content = curToken;
      }
    }
  }
};

} // namespace lisp
} // namespace utils

#endif