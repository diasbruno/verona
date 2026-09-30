#include "reader.hpp"
#include "type.hpp"

#include <array>
#include <cstdlib>
#include <iostream>
#include <string_view>

namespace {

void require(bool condition, std::string_view message) {
  if (!condition) {
    std::cerr << "type test failed: " << message << '\n';
    std::exit(EXIT_FAILURE);
  }
}

verona::TypePtr parse_one_type(std::string_view source) {
  auto forms = verona::read_forms(source);
  require(forms.size() == 1, "expected one form");
  return verona::parse_type(*forms[0]);
}

void parses_primitive_types() {
  struct Case {
    std::string_view source;
    verona::PrimitiveType primitive;
  };

  static constexpr std::array<Case, 15> cases = {{
      {"i8", verona::PrimitiveType::i8},
      {"i16", verona::PrimitiveType::i16},
      {"i32", verona::PrimitiveType::i32},
      {"i64", verona::PrimitiveType::i64},
      {"isize", verona::PrimitiveType::isize},
      {"u8", verona::PrimitiveType::u8},
      {"u16", verona::PrimitiveType::u16},
      {"u32", verona::PrimitiveType::u32},
      {"u64", verona::PrimitiveType::u64},
      {"usize", verona::PrimitiveType::usize},
      {"f32", verona::PrimitiveType::f32},
      {"f64", verona::PrimitiveType::f64},
      {"bool", verona::PrimitiveType::bool_},
      {"unit", verona::PrimitiveType::unit},
      {"void", verona::PrimitiveType::void_},
  }};

  for (const auto& test_case : cases) {
    const auto type = parse_one_type(test_case.source);

    require(type->kind == verona::TypeKind::primitive, "expected primitive type");
    require(type->primitive == test_case.primitive, "expected matching primitive");
  }
}

void parses_named_and_applied_types() {
  const auto named = parse_one_type("Point");
  require(named->kind == verona::TypeKind::name, "expected named type");
  require(named->name == "Point", "expected Point name");

  const auto applied = parse_one_type("(Pair i32 f64)");
  require(applied->kind == verona::TypeKind::application, "expected type application");
  require(applied->name == "Pair", "expected Pair application");
  require(applied->arguments.size() == 2, "expected two type arguments");
  require(applied->arguments[0]->primitive == verona::PrimitiveType::i32, "expected first argument");
  require(applied->arguments[1]->primitive == verona::PrimitiveType::f64, "expected second argument");
}

void parses_core_type_constructors() {
  const auto pointer = parse_one_type("(& Point)");
  require(pointer->kind == verona::TypeKind::pointer, "expected pointer type");
  require(pointer->element->kind == verona::TypeKind::name, "expected pointed type");

  const auto array = parse_one_type("(array u8 256)");
  require(array->kind == verona::TypeKind::array, "expected array type");
  require(array->element->primitive == verona::PrimitiveType::u8, "expected array element");
  require(array->array_size == 256, "expected array size");

  const auto slice = parse_one_type("(slice u8)");
  require(slice->kind == verona::TypeKind::slice, "expected slice type");
  require(slice->element->primitive == verona::PrimitiveType::u8, "expected slice element");

  const auto function = parse_one_type("(fn (i32 i32) i32)");
  require(function->kind == verona::TypeKind::function, "expected function type");
  require(function->arguments.size() == 2, "expected two function arguments");
  require(function->result->primitive == verona::PrimitiveType::i32, "expected function result");
}

void parses_aggregate_types() {
  const auto product = parse_one_type("(product (x f32) (y f32))");
  require(product->kind == verona::TypeKind::product, "expected product type");
  require(product->fields.size() == 2, "expected product fields");
  require(product->fields[0].name == "x", "expected product field name");

  const auto sum = parse_one_type("(sum None (Some i32))");
  require(sum->kind == verona::TypeKind::sum, "expected sum type");
  require(sum->alternatives.size() == 2, "expected sum alternatives");
  require(sum->alternatives[0].name == "None", "expected empty alternative");
  require(sum->alternatives[1].payload.size() == 1, "expected payload alternative");

  const auto union_type = parse_one_type("(union (integer i64) (floating f64))");
  require(union_type->kind == verona::TypeKind::union_, "expected union type");
  require(union_type->fields.size() == 2, "expected union fields");
}

void rejects_bad_type_forms() {
  try {
    (void)parse_one_type("(array u8)");
  } catch (const verona::TypeError& error) {
    require(error.diagnostic().message == "array type requires an element type and compile-time size",
            "expected array diagnostic");
    return;
  }

  require(false, "expected bad array failure");
}

void parses_type_declarations() {
  auto forms = verona::read_forms("(type Pair (A B) (product (first A) (second B)))");
  const auto declaration = verona::parse_type_declaration(*forms[0]);

  require(declaration.name == "Pair", "expected declaration name");
  require(declaration.parameters.size() == 2, "expected type parameters");
  require(declaration.parameters[0].name == "A", "expected first type parameter");
  require(declaration.parameters[1].name == "B", "expected second type parameter");
  require(declaration.body->kind == verona::TypeKind::product, "expected declaration body");
}

void rejects_bad_type_parameters() {
  try {
    auto forms = verona::read_forms("(type Id (42) u64)");
    (void)verona::parse_type_declaration(*forms[0]);
  } catch (const verona::TypeError& error) {
    require(error.diagnostic().message == "type parameter must be a symbol",
            "expected type parameter diagnostic");
    return;
  }

  require(false, "expected bad type parameter failure");
}

void rejects_duplicate_type_parameters() {
  try {
    auto forms = verona::read_forms("(type Bad (T T) T)");
    (void)verona::parse_type_declaration(*forms[0]);
  } catch (const verona::TypeError& error) {
    require(error.diagnostic().message == "type parameter redefines existing parameter",
            "expected duplicate type parameter diagnostic");
    return;
  }

  require(false, "expected duplicate type parameter failure");
}

void rejects_duplicate_type_declarations() {
  try {
    verona::TypeEnvironment environment;
    auto first = verona::read_forms("(type UserId u64)");
    auto second = verona::read_forms("(type UserId i64)");
    environment.declare(verona::parse_type_declaration(*first[0]));
    environment.declare(verona::parse_type_declaration(*second[0]));
  } catch (const verona::TypeError& error) {
    require(error.diagnostic().message == "type declaration redefines existing type",
            "expected duplicate declaration diagnostic");
    return;
  }

  require(false, "expected duplicate declaration failure");
}

void instantiates_generic_type_declarations() {
  verona::TypeEnvironment environment;
  auto forms = verona::read_forms("(type Pair (A B) (product (first A) (second B)))");
  environment.declare(verona::parse_type_declaration(*forms[0]));

  const auto application = parse_one_type("(Pair i32 bool)");
  const auto instantiated = verona::instantiate_type_application(*application, environment);

  require(instantiated->kind == verona::TypeKind::product, "expected instantiated product");
  require(instantiated->fields.size() == 2, "expected instantiated fields");
  require(instantiated->fields[0].type->primitive == verona::PrimitiveType::i32,
          "expected substituted first field");
  require(instantiated->fields[1].type->primitive == verona::PrimitiveType::bool_,
          "expected substituted second field");
}

void instantiates_nested_generic_type_declarations() {
  verona::TypeEnvironment environment;
  auto box_forms = verona::read_forms("(type Box (T) (product (value T)))");
  auto pair_forms = verona::read_forms("(type Pair (A B) (product (first A) (second B)))");
  environment.declare(verona::parse_type_declaration(*box_forms[0]));
  environment.declare(verona::parse_type_declaration(*pair_forms[0]));

  const auto application = parse_one_type("(Box (Pair i32 bool))");
  const auto instantiated = verona::instantiate_type_application(*application, environment);

  require(instantiated->kind == verona::TypeKind::product, "expected outer instantiated product");
  require(instantiated->fields.size() == 1, "expected outer instantiated field");
  require(instantiated->fields[0].type->kind == verona::TypeKind::application,
          "expected nested generic application to be preserved");
  require(instantiated->fields[0].type->name == "Pair", "expected nested Pair application");
}

void formats_type_applications() {
  const auto type = parse_one_type("(Pair (& i32) (array bool 4))");

  require(verona::type_to_string(*type) == "(Pair (& i32) (array bool 4))",
          "expected canonical type application string");
}

void interns_generic_instantiations() {
  verona::TypeEnvironment environment;
  auto forms = verona::read_forms("(type Pair (A B) (product (first A) (second B)))");
  environment.declare(verona::parse_type_declaration(*forms[0]));

  const auto first_application = parse_one_type("(Pair i32 bool)");
  const auto second_application = parse_one_type("(Pair i32 bool)");
  const auto other_application = parse_one_type("(Pair bool i32)");

  verona::MonomorphizationRegistry registry;
  const auto& first = registry.intern(*first_application, environment);
  const auto first_key = first.key;
  const auto& second = registry.intern(*second_application, environment);
  const auto second_key = second.key;
  const auto& other = registry.intern(*other_application, environment);
  const auto other_key = other.key;

  require(registry.size() == 2, "expected unique generic instantiations");
  require(first_key == "(Pair i32 bool)", "expected first instantiation key");
  require(second_key == first_key, "expected repeated instantiation key");
  require(other_key == "(Pair bool i32)", "expected distinct instantiation key");
  require(registry.instantiations()[0].type->kind == verona::TypeKind::product,
          "expected instantiated type body");
}

void rejects_bad_generic_arity() {
  try {
    verona::TypeEnvironment environment;
    auto forms = verona::read_forms("(type Box (T) (product (value T)))");
    environment.declare(verona::parse_type_declaration(*forms[0]));
    const auto application = parse_one_type("(Box i32 bool)");
    (void)verona::instantiate_type_application(*application, environment);
  } catch (const verona::TypeError& error) {
    require(error.diagnostic().message == "generic type argument count mismatch",
            "expected generic arity diagnostic");
    return;
  }

  require(false, "expected bad generic arity failure");
}

}  // namespace

int main() {
  parses_primitive_types();
  parses_named_and_applied_types();
  parses_core_type_constructors();
  parses_aggregate_types();
  rejects_bad_type_forms();
  parses_type_declarations();
  rejects_bad_type_parameters();
  rejects_duplicate_type_parameters();
  rejects_duplicate_type_declarations();
  instantiates_generic_type_declarations();
  instantiates_nested_generic_type_declarations();
  formats_type_applications();
  interns_generic_instantiations();
  rejects_bad_generic_arity();
}
