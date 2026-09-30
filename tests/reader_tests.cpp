#include "reader.hpp"

#include <cstdlib>
#include <iostream>
#include <string_view>

namespace {

template <typename T>
const T& as(const verona::Form& form) {
  return std::get<T>(form.kind);
}

void require(bool condition, std::string_view message) {
  if (!condition) {
    std::cerr << "reader test failed: " << message << '\n';
    std::exit(EXIT_FAILURE);
  }
}

void parses_atoms() {
  const auto forms = verona::read_forms("alpha 42 -7 1.5 \"hi\\nthere\" unit");

  require(forms.size() == 6, "expected six forms");
  require(as<verona::Symbol>(*forms[0]).name == "alpha", "expected symbol");
  require(as<verona::IntegerLiteral>(*forms[1]).value == 42, "expected integer");
  require(as<verona::IntegerLiteral>(*forms[2]).value == -7, "expected signed integer");
  require(as<verona::FloatLiteral>(*forms[3]).value == 1.5, "expected float");
  require(as<verona::StringLiteral>(*forms[4]).value == "hi\nthere", "expected escaped string");
  require(std::holds_alternative<verona::UnitLiteral>(forms[5]->kind), "expected unit");
}

void parses_lists() {
  const auto forms = verona::read_forms("(fn noop () unit unit)");

  require(forms.size() == 1, "expected one top-level form");
  const auto& list = as<verona::List>(*forms[0]);
  require(list.elements.size() == 5, "expected fn list elements");
  require(as<verona::Symbol>(*list.elements[0]).name == "fn", "expected fn symbol");
  require(as<verona::Symbol>(*list.elements[1]).name == "noop", "expected function name");
  require(as<verona::List>(*list.elements[2]).elements.empty(), "expected empty parameter list");
  require(std::holds_alternative<verona::UnitLiteral>(list.elements[3]->kind),
          "expected unit type");
  require(std::holds_alternative<verona::UnitLiteral>(list.elements[4]->kind), "expected unit body");
}

void parses_unit_inside_lists() {
  const auto forms = verona::read_forms("(a unit b)");

  require(forms.size() == 1, "expected one top-level form");
  const auto& list = as<verona::List>(*forms[0]);
  require(list.elements.size() == 3, "expected three list elements");
  require(as<verona::Symbol>(*list.elements[0]).name == "a", "expected first symbol");
  require(std::holds_alternative<verona::UnitLiteral>(list.elements[1]->kind), "expected unit");
  require(as<verona::Symbol>(*list.elements[2]).name == "b", "expected second symbol");
}

void rejects_dot_prefixed_floats() {
  try {
    (void)verona::read_forms(".5");
  } catch (const verona::ReadError& error) {
    require(error.diagnostic().message == "floating-point literals must start with a digit",
            "expected dot-prefixed float diagnostic");
    return;
  }

  require(false, "expected dot-prefixed float failure");
}

}  // namespace

int main() {
  parses_atoms();
  parses_lists();
  parses_unit_inside_lists();
  rejects_dot_prefixed_floats();
}
