#include "codegen.hpp"
#include "reader.hpp"
#include "semantic.hpp"

#include <cstdlib>
#include <iostream>
#include <string_view>

namespace {

void require(bool condition, std::string_view message) {
  if (!condition) {
    std::cerr << "codegen test failed: " << message << '\n';
    std::exit(EXIT_FAILURE);
  }
}

std::string emit(std::string_view source) {
  auto forms = termis::read_forms(source);
  auto program = termis::analyze_forms(forms);
  return termis::emit_llvm_ir(program);
}

void contains(std::string_view haystack, std::string_view needle) {
  require(haystack.find(needle) != std::string_view::npos, "expected emitted LLVM fragment");
}

void emits_arithmetic_function() {
  const auto ir = emit("(fn add ((a i64) (b i64)) i64 (+ a b))");

  contains(ir, "define i64 @add(i64 %a, i64 %b)");
  contains(ir, "add i64 %a, %b");
  contains(ir, "ret i64");
}

void emits_let_do_and_unit() {
  const auto ir = emit(R"(
    (fn log () unit
      (do
        .
        .))
    (fn value () i64
      (let ((x 40)
            (y 2))
        (+ x y)))
  )");

  contains(ir, "define void @log()");
  contains(ir, "ret void");
  contains(ir, "define i64 @value()");
  contains(ir, "ret i64 42");
}

void emits_comparison_function() {
  const auto ir = emit("(fn greater ((a i64) (b i64)) bool (> a b))");

  contains(ir, "icmp sgt i64 %a, %b");
  contains(ir, "ret i1");
}

void emits_match_expression() {
  const auto ir = emit(R"(
    (fn choose ((flag bool)) i64
      (match flag
        (true 1)
        (false 0)))
  )");

  contains(ir, "icmp eq i1 %flag, true");
  contains(ir, "icmp eq i1 %flag, false");
  contains(ir, "phi i64");
}

void emits_match_binding() {
  const auto ir = emit(R"(
    (fn identity ((value i64)) i64
      (match value
        (x x)))
  )");

  contains(ir, "ret i64");
}

void emits_function_calls() {
  const auto ir = emit(R"(
    (fn add ((a i64) (b i64)) i64 (+ a b))
    (fn main () i64 (add 10 32))
  )");

  contains(ir, "call i64 @add(i64 10, i64 32)");
}

void emits_stable_signature_functions() {
  const auto ir = emit(R"(
    (fn add
      ((params ((a i64) (b i64)))
       (return i64)
       (requires ())
       (documentation "Add two values."))
      (+ a b))
    (fn main
      ((params ())
       (return i64)
       (requires ()))
      (add 10 32))
  )");

  contains(ir, "define i64 @add(i64 %a, i64 %b)");
  contains(ir, "call i64 @add(i64 10, i64 32)");
}

void emits_string_literal_calls() {
  const auto ir = emit(R"(
    (type Data (& u8))
    (extern fn write ((data Data)) i32 "puts")
    (fn main () i32 (write "hello"))
  )");

  contains(ir, "private unnamed_addr constant [6 x i8] c\"hello\\00\"");
  contains(ir, "call i32 @puts(ptr");
}

void emits_extern_function_calls() {
  const auto ir = emit(R"(
    (extern fn c-abs ((value i64)) i64 "llabs")
    (fn main () i64 (c-abs -42))
  )");

  contains(ir, "declare i64 @llabs(i64)");
  contains(ir, "call i64 @llabs(i64 -42)");
}

void emits_primitive_type_aliases() {
  const auto ir = emit(R"(
    (type UserId u64)
    (fn identity ((id UserId)) UserId id)
  )");

  contains(ir, "define i64 @identity(i64 %id)");
  contains(ir, "ret i64 %id");
}

void emits_primitive_generic_instantiations() {
  const auto ir = emit(R"(
    (type Identity (T) T)
    (fn identity ((value (Identity i64))) (Identity i64) value)
  )");

  contains(ir, "define i64 @identity(i64 %value)");
  contains(ir, "ret i64 %value");
}

void emits_alias_chains_through_generic_instantiations() {
  const auto ir = emit(R"(
    (type UserId u64)
    (type Identity (T) T)
    (type WrappedUserId (Identity UserId))
    (fn identity ((value WrappedUserId)) WrappedUserId value)
  )");

  contains(ir, "define i64 @identity(i64 %value)");
  contains(ir, "ret i64 %value");
}

void rejects_type_mismatch() {
  try {
    (void)emit("(fn bad () i64 true)");
  } catch (const termis::CodegenError& error) {
    require(error.diagnostic().message == "expression type mismatch",
            "expected type mismatch diagnostic");
    return;
  }

  require(false, "expected codegen failure");
}

}  // namespace

int main() {
  emits_arithmetic_function();
  emits_let_do_and_unit();
  emits_comparison_function();
  emits_match_expression();
  emits_match_binding();
  emits_function_calls();
  emits_stable_signature_functions();
  emits_string_literal_calls();
  emits_extern_function_calls();
  emits_primitive_type_aliases();
  emits_primitive_generic_instantiations();
  emits_alias_chains_through_generic_instantiations();
  rejects_type_mismatch();
}
