#include "reader.hpp"
#include "semantic.hpp"
#include "type.hpp"

#include <cstdlib>
#include <iostream>
#include <string_view>

namespace {

void require(bool condition, std::string_view message) {
  if (!condition) {
    std::cerr << "semantic test failed: " << message << '\n';
    std::exit(EXIT_FAILURE);
  }
}

termis::Program analyze(std::string_view source) {
  auto forms = termis::read_forms(source);
  return termis::analyze_forms(forms);
}

void recognizes_core_forms() {
  auto program = analyze(R"(
    (type UserId u64)
    (provide core/math
      (fn +
        ((params ((left i64) (right i64)))
         (return i64)
         (requires ())
         (documentation "Add two signed integers."))))
    (fn noop () unit .)
    (extern fn c-abs ((value i64)) i64 "llabs")
    (let ((x 1)) x)
    (do .)
    (match value (_ .))
    (const size 42)
    (class Eq (T)
      (fn eq
        ((params ((left T) (right T)))
         (return bool)
         (requires ())
         (documentation "Compare two values for equality."))))
    (implements Eq (i64)
      (fn eq
        ((params ((left i64) (right i64)))
         (return bool)
         (requires ()))
        (= left right)))
    (+ 1 2)
    (call ok 1 0)
    ()
    (list 1 2)
    ((f) 1)
    42
  )");

  require(program.forms.size() == 16, "expected sixteen semantic forms");
  require(program.forms[0]->kind == termis::SemanticKind::type_declaration,
          "expected type declaration");
  require(program.forms[1]->kind == termis::SemanticKind::provide_declaration,
          "expected provide declaration");
  require(program.forms[2]->kind == termis::SemanticKind::function_declaration,
          "expected function declaration");
  require(program.forms[3]->kind == termis::SemanticKind::extern_function_declaration,
          "expected extern function declaration");
  require(program.forms[4]->kind == termis::SemanticKind::let_expression,
          "expected let expression");
  require(program.forms[5]->kind == termis::SemanticKind::do_expression,
          "expected do expression");
  require(program.forms[6]->kind == termis::SemanticKind::match_expression,
          "expected match expression");
  require(program.forms[7]->kind == termis::SemanticKind::const_declaration,
          "expected const declaration");
  require(program.forms[8]->kind == termis::SemanticKind::class_declaration,
          "expected class declaration");
  require(program.forms[9]->kind == termis::SemanticKind::implements_declaration,
          "expected implements declaration");
  require(program.forms[10]->kind == termis::SemanticKind::application,
          "expected application");
  require(program.forms[11]->kind == termis::SemanticKind::application,
          "expected call to be an application");
  require(program.forms[12]->kind == termis::SemanticKind::list_expression,
          "expected empty list expression");
  require(program.forms[13]->kind == termis::SemanticKind::list_expression,
          "expected list-headed list expression");
  require(program.forms[14]->kind == termis::SemanticKind::list_expression,
          "expected non-symbol-headed list expression");
  require(program.forms[15]->kind == termis::SemanticKind::atom,
          "expected atom");
}

void rejects_bad_type_declaration() {
  try {
    (void)analyze("(type 42 u64)");
  } catch (const termis::SemanticError& error) {
    require(error.diagnostic().message == "type declaration name must be a symbol",
            "expected type name diagnostic");
    return;
  }

  require(false, "expected bad type declaration failure");
}

void rejects_bad_type_body() {
  try {
    (void)analyze("(type Buffer (array u8))");
  } catch (const termis::TypeError& error) {
    require(error.diagnostic().message == "array type requires an element type and compile-time size",
            "expected type body diagnostic");
    return;
  }

  require(false, "expected bad type body failure");
}

void accepts_empty_lists() {
  auto program = analyze("()");

  require(program.forms.size() == 1, "expected one semantic form");
  require(program.forms[0]->kind == termis::SemanticKind::list_expression,
          "expected empty list to remain a list expression");
}

void collects_type_declarations() {
  auto program = analyze(R"(
    (type UserId u64)
    (type Pair (A B) (product (first A) (second B)))
  )");

  require(program.types.size() == 2, "expected two type declarations");

  const auto* user_id = program.types.find("UserId");
  require(user_id != nullptr, "expected UserId declaration");
  require(user_id->body->primitive == termis::PrimitiveType::u64, "expected UserId body");

  const auto* pair = program.types.find("Pair");
  require(pair != nullptr, "expected Pair declaration");
  require(pair->parameters.size() == 2, "expected Pair parameters");
}

void collects_provide_declarations() {
  auto program = analyze(R"(
    (class Integer (T))
    (provide core/math
      (fn +
        ((type-params (T))
         (params ((left T) (right T)))
         (return T)
         (requires ((Integer T)))
         (documentation "Add two integers.")))
      (fn =
        ((params ((left i64) (right i64)))
         (return bool)
         (requires ()))))
  )");

  require(program.provides.size() == 1, "expected one provide declaration");
  const auto* math = program.provides.find("core/math");
  require(math != nullptr, "expected core/math provide declaration");
  require(math->functions.size() == 2, "expected two provided functions");
  require(math->functions[0].name == "+", "expected plus function");
  require(math->functions[0].type_parameters.size() == 1, "expected plus type parameter");
  require(math->functions[0].parameters[0].type->name == "T", "expected generic left operand");
  require(math->functions[0].requirements.size() == 1, "expected integer requirement");
  require(math->functions[0].requirements[0].class_name == "Integer", "expected Integer requirement");
  require(math->functions[0].documentation == "Add two integers.",
          "expected function documentation");
  require(math->functions[1].result->primitive == termis::PrimitiveType::bool_,
          "expected equality to return bool");
}

void collects_class_declarations_and_implementations() {
  auto program = analyze(R"(
    (class Eq (T)
      (fn eq
        ((params ((left T) (right T)))
         (return bool)
         (requires ())
         (documentation "Compare two values for equality."))))
    (class Ord (T)
      (fn lt
        ((params ((left T) (right T)))
         (return bool)
         (requires ((Eq T))))))
    (implements Eq (i64)
      (fn eq
        ((params ((left i64) (right i64)))
         (return bool)
         (requires ()))
        (= left right)))
  )");

  require(program.classes.size() == 2, "expected two class declarations");
  const auto* eq = program.classes.find("Eq");
  require(eq != nullptr, "expected Eq class");
  require(eq->parameters.size() == 1, "expected Eq type parameter");
  require(eq->methods.size() == 1, "expected Eq method");
  require(eq->methods[0].name == "eq", "expected eq method");
  require(eq->methods[0].documentation == "Compare two values for equality.",
          "expected Eq method documentation");
  const auto* ord = program.classes.find("Ord");
  require(ord != nullptr, "expected Ord class");
  require(ord->methods[0].requirements.size() == 1, "expected Ord method requirement");
  require(ord->methods[0].requirements[0].class_name == "Eq", "expected Eq requirement");
  require(program.classes.implementations().size() == 1, "expected one implementation");
  require(program.classes.implementations()[0].class_name == "Eq", "expected Eq implementation");
}

void rejects_unknown_implements_class() {
  try {
    (void)analyze(R"(
      (implements Eq (i64)
        (fn eq
        ((params ((left i64) (right i64)))
         (return bool)
         (requires ()))
        (= left right)))
    )");
  } catch (const termis::SemanticError& error) {
    require(error.diagnostic().message == "implements references unknown class",
            "expected unknown class diagnostic");
    return;
  }

  require(false, "expected unknown class failure");
}

void rejects_missing_implements_methods() {
  try {
    (void)analyze(R"(
      (class Eq (T)
        (fn eq
        ((params ((left T) (right T)))
         (return bool)
         (requires ()))))
      (implements Eq (i64)
        (fn other
        ((params ((left i64) (right i64)))
         (return bool)
         (requires ()))
        true))
    )");
  } catch (const termis::SemanticError& error) {
    require(error.diagnostic().message == "implements is missing class method",
            "expected missing method diagnostic");
    return;
  }

  require(false, "expected missing method failure");
}

void rejects_mismatched_implements_method_signatures() {
  try {
    (void)analyze(R"(
      (class Eq (T)
        (fn eq
        ((params ((left T) (right T)))
         (return bool)
         (requires ()))))
      (implements Eq (i64)
        (fn eq
        ((params ((left i32) (right i64)))
         (return bool)
         (requires ()))
        true))
    )");
  } catch (const termis::SemanticError& error) {
    require(error.diagnostic().message == "implements method signature does not match class method",
            "expected mismatched method diagnostic");
    return;
  }

  require(false, "expected mismatched method failure");
}

void rejects_unknown_type_references() {
  try {
    (void)analyze("(type MissingBox Missing)");
  } catch (const termis::TypeError& error) {
    require(error.diagnostic().message == "unknown type name",
            "expected unknown type diagnostic");
    return;
  }

  require(false, "expected unknown type failure");
}

void rejects_missing_generic_arguments() {
  try {
    (void)analyze(R"(
      (type Box (T) (product (value T)))
      (type Bad Box)
    )");
  } catch (const termis::TypeError& error) {
    require(error.diagnostic().message == "generic type requires type arguments",
            "expected missing generic arguments diagnostic");
    return;
  }

  require(false, "expected missing generic arguments failure");
}

void rejects_bad_generic_argument_count() {
  try {
    (void)analyze(R"(
      (type Box (T) (product (value T)))
      (type Bad (Box i32 bool))
    )");
  } catch (const termis::TypeError& error) {
    require(error.diagnostic().message == "generic type argument count mismatch",
            "expected generic arity diagnostic");
    return;
  }

  require(false, "expected bad generic arity failure");
}

void rejects_applying_concrete_types() {
  try {
    (void)analyze(R"(
      (type UserId u64)
      (type Bad (UserId i32))
    )");
  } catch (const termis::TypeError& error) {
    require(error.diagnostic().message == "type does not accept type arguments",
            "expected concrete type application diagnostic");
    return;
  }

  require(false, "expected concrete type application failure");
}

}  // namespace

int main() {
  recognizes_core_forms();
  rejects_bad_type_declaration();
  rejects_bad_type_body();
  accepts_empty_lists();
  collects_type_declarations();
  collects_provide_declarations();
  collects_class_declarations_and_implementations();
  rejects_unknown_implements_class();
  rejects_missing_implements_methods();
  rejects_mismatched_implements_method_signatures();
  rejects_unknown_type_references();
  rejects_missing_generic_arguments();
  rejects_bad_generic_argument_count();
  rejects_applying_concrete_types();
}
