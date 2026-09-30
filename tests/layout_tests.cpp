#include "layout.hpp"
#include "reader.hpp"
#include "semantic.hpp"
#include "type.hpp"

#include <cstdlib>
#include <iostream>
#include <string_view>

namespace {

void require(bool condition, std::string_view message) {
  if (!condition) {
    std::cerr << "layout test failed: " << message << '\n';
    std::exit(EXIT_FAILURE);
  }
}

verona::TypePtr parse_one_type(std::string_view source) {
  auto forms = verona::read_forms(source);
  require(forms.size() == 1, "expected one form");
  return verona::parse_type(*forms[0]);
}

verona::Layout layout_of(std::string_view source) {
  verona::TypeEnvironment types;
  verona::LayoutEngine engine(types);
  return engine.compute(*parse_one_type(source));
}

void computes_primitive_layouts() {
  require(layout_of("i8").size == 1, "expected i8 size");
  require(layout_of("i16").alignment == 2, "expected i16 alignment");
  require(layout_of("i32").size == 4, "expected i32 size");
  require(layout_of("i64").alignment == 8, "expected i64 alignment");
  require(layout_of("isize").size == 8, "expected isize size");
  require(layout_of("usize").size == 8, "expected usize size");
  require(layout_of("bool").size == 1, "expected bool size");
  require(layout_of("unit").size == 0, "expected unit size");
}

void computes_pointer_array_and_slice_layouts() {
  const auto pointer = layout_of("(& i32)");
  require(pointer.size == 8, "expected pointer size");
  require(pointer.alignment == 8, "expected pointer alignment");

  const auto array = layout_of("(array u16 3)");
  require(array.size == 6, "expected array size");
  require(array.alignment == 2, "expected array alignment");

  const auto slice = layout_of("(slice u8)");
  require(slice.size == 16, "expected slice size");
  require(slice.alignment == 8, "expected slice alignment");
}

void computes_product_layouts() {
  const auto layout = layout_of("(product (a u8) (b u32) (c u16))");

  require(layout.size == 12, "expected padded product size");
  require(layout.alignment == 4, "expected product alignment");
  require(layout.fields.size() == 3, "expected field layouts");
  require(layout.fields[0].offset == 0, "expected first field offset");
  require(layout.fields[1].offset == 4, "expected padded second field offset");
  require(layout.fields[2].offset == 8, "expected third field offset");
}

void computes_union_and_sum_layouts() {
  const auto union_layout = layout_of("(union (small u8) (wide u64))");
  require(union_layout.size == 8, "expected union size");
  require(union_layout.alignment == 8, "expected union alignment");
  require(union_layout.fields[0].offset == 0, "expected union field offset");

  const auto sum_layout = layout_of("(sum None (Some u64))");
  require(sum_layout.size == 16, "expected sum size");
  require(sum_layout.alignment == 8, "expected sum alignment");
}

void computes_named_type_layouts() {
  auto forms = verona::read_forms("(type Point (product (x f32) (y f32)))");
  const auto program = verona::analyze_forms(forms);
  const auto point = parse_one_type("Point");
  verona::LayoutEngine engine(program.types);
  const auto layout = engine.compute(*point);

  require(layout.size == 8, "expected named type size");
  require(layout.alignment == 4, "expected named type alignment");
}

void computes_generic_type_layouts() {
  auto forms = verona::read_forms("(type Pair (A B) (product (first A) (second B)))");
  const auto program = verona::analyze_forms(forms);
  const auto pair = parse_one_type("(Pair i32 i64)");
  verona::LayoutEngine engine(program.types);
  const auto layout = engine.compute(*pair);

  require(layout.size == 16, "expected generic layout size");
  require(layout.alignment == 8, "expected generic layout alignment");
  require(layout.fields.size() == 2, "expected generic field layouts");
  require(layout.fields[0].offset == 0, "expected first generic field offset");
  require(layout.fields[1].offset == 8, "expected second generic field offset");
}

void rejects_void_layout() {
  try {
    (void)layout_of("void");
  } catch (const verona::LayoutError& error) {
    require(error.diagnostic().message == "void does not have a source-language value layout",
            "expected void layout diagnostic");
    return;
  }

  require(false, "expected void layout failure");
}

void rejects_non_literal_array_size() {
  try {
    (void)layout_of("(array u8 size)");
  } catch (const verona::TypeError& error) {
    require(error.diagnostic().message == "array size must be an integer literal",
            "expected array size diagnostic");
    return;
  }

  require(false, "expected array size failure");
}

}  // namespace

int main() {
  computes_primitive_layouts();
  computes_pointer_array_and_slice_layouts();
  computes_product_layouts();
  computes_union_and_sum_layouts();
  computes_named_type_layouts();
  computes_generic_type_layouts();
  rejects_void_layout();
  rejects_non_literal_array_size();
}
