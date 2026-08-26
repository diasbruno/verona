#pragma once

#include "reader.hpp"
#include "semantic.hpp"

#include <stdexcept>
#include <string>

namespace termis {

class CodegenError final : public std::runtime_error {
 public:
  explicit CodegenError(Diagnostic diagnostic);

  const Diagnostic& diagnostic() const;

 private:
  Diagnostic diagnostic_;
};

std::string emit_llvm_ir(const Program& program);
std::string emit_object_file(const Program& program);

}  // namespace termis
