#include "type.hpp"

#include <array>
#include <sstream>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>

namespace termis {
namespace {

const Symbol* as_symbol(const Form& form) {
  return std::get_if<Symbol>(&form.kind);
}

const List* as_list(const Form& form) {
  return std::get_if<List>(&form.kind);
}

const Form& element(const List& list, std::size_t index) {
  return *list.elements[index];
}

[[noreturn]] void fail(SourceLocation location, std::string message) {
  throw TypeError(Diagnostic{location, std::move(message)});
}

PrimitiveType primitive_from_name(std::string_view name, bool& found) {
  struct Entry {
    std::string_view name;
    PrimitiveType type;
  };

  static constexpr std::array<Entry, 15> entries = {{
      {"i8", PrimitiveType::i8},
      {"i16", PrimitiveType::i16},
      {"i32", PrimitiveType::i32},
      {"i64", PrimitiveType::i64},
      {"isize", PrimitiveType::isize},
      {"u8", PrimitiveType::u8},
      {"u16", PrimitiveType::u16},
      {"u32", PrimitiveType::u32},
      {"u64", PrimitiveType::u64},
      {"usize", PrimitiveType::usize},
      {"f32", PrimitiveType::f32},
      {"f64", PrimitiveType::f64},
      {"bool", PrimitiveType::bool_},
      {"unit", PrimitiveType::unit},
      {"void", PrimitiveType::void_},
  }};

  for (const auto& entry : entries) {
    if (entry.name == name) {
      found = true;
      return entry.type;
    }
  }

  found = false;
  return PrimitiveType::unit;
}

TypePtr make_type(TypeKind kind, SourceLocation location) {
  auto type = std::make_unique<Type>();
  type->kind = kind;
  type->location = location;
  return type;
}

TypePtr substitute_type_impl(const Type& type, const std::unordered_map<std::string, const Type*>& substitutions);

std::vector<TypePtr> clone_type_vector(const std::vector<TypePtr>& types) {
  std::vector<TypePtr> cloned;
  cloned.reserve(types.size());
  for (const auto& type : types) {
    cloned.push_back(clone_type(*type));
  }
  return cloned;
}

std::vector<TypePtr> substitute_type_vector(const std::vector<TypePtr>& types,
                                            const std::unordered_map<std::string, const Type*>& substitutions) {
  std::vector<TypePtr> substituted;
  substituted.reserve(types.size());
  for (const auto& type : types) {
    substituted.push_back(substitute_type_impl(*type, substitutions));
  }
  return substituted;
}

std::string require_symbol_name(const Form& form, std::string message) {
  const auto* symbol = as_symbol(form);
  if (symbol == nullptr) {
    fail(form.location, std::move(message));
  }
  return symbol->name;
}

Field parse_field(const Form& form, std::string_view aggregate_name) {
  const auto* list = as_list(form);
  if (list == nullptr || list->elements.size() != 2) {
    fail(form.location, std::string(aggregate_name) + " field requires a name and type");
  }

  return Field{
      require_symbol_name(element(*list, 0), std::string(aggregate_name) + " field name must be a symbol"),
      parse_type(element(*list, 1)),
  };
}

SumAlternative parse_sum_alternative(const Form& form) {
  if (const auto* symbol = as_symbol(form)) {
    return SumAlternative{symbol->name, {}};
  }

  const auto* list = as_list(form);
  if (list == nullptr || list->elements.empty()) {
    fail(form.location, "sum alternative requires a constructor name");
  }

  SumAlternative alternative{
      require_symbol_name(element(*list, 0), "sum alternative name must be a symbol"),
      {},
  };
  for (std::size_t index = 1; index < list->elements.size(); ++index) {
    alternative.payload.push_back(parse_type(element(*list, index)));
  }
  return alternative;
}

TypePtr parse_type_list(const Form& form, const List& list) {
  if (list.elements.empty()) {
    fail(form.location, "type expression list cannot be empty");
  }

  const auto* head = as_symbol(element(list, 0));
  if (head == nullptr) {
    fail(element(list, 0).location, "type constructor must be a symbol");
  }

  if (head->name == "&") {
    if (list.elements.size() != 2) {
      fail(form.location, "pointer type requires exactly one element type");
    }
    auto type = make_type(TypeKind::pointer, form.location);
    type->element = parse_type(element(list, 1));
    return type;
  }

  if (head->name == "array") {
    if (list.elements.size() != 3) {
      fail(form.location, "array type requires an element type and compile-time size");
    }
    const auto* size = std::get_if<IntegerLiteral>(&element(list, 2).kind);
    if (size == nullptr) {
      fail(element(list, 2).location, "array size must be an integer literal");
    }
    auto type = make_type(TypeKind::array, form.location);
    type->element = parse_type(element(list, 1));
    type->array_size = size->value;
    return type;
  }

  if (head->name == "slice") {
    if (list.elements.size() != 2) {
      fail(form.location, "slice type requires exactly one element type");
    }
    auto type = make_type(TypeKind::slice, form.location);
    type->element = parse_type(element(list, 1));
    return type;
  }

  if (head->name == "fn") {
    if (list.elements.size() != 3) {
      fail(form.location, "function type requires arguments and return type");
    }
    const auto* arguments = as_list(element(list, 1));
    if (arguments == nullptr) {
      fail(element(list, 1).location, "function type arguments must be a list");
    }
    auto type = make_type(TypeKind::function, form.location);
    for (const auto& argument : arguments->elements) {
      type->arguments.push_back(parse_type(*argument));
    }
    type->result = parse_type(element(list, 2));
    return type;
  }

  if (head->name == "product") {
    auto type = make_type(TypeKind::product, form.location);
    for (std::size_t index = 1; index < list.elements.size(); ++index) {
      type->fields.push_back(parse_field(element(list, index), "product"));
    }
    return type;
  }

  if (head->name == "sum") {
    auto type = make_type(TypeKind::sum, form.location);
    for (std::size_t index = 1; index < list.elements.size(); ++index) {
      type->alternatives.push_back(parse_sum_alternative(element(list, index)));
    }
    return type;
  }

  if (head->name == "union") {
    auto type = make_type(TypeKind::union_, form.location);
    for (std::size_t index = 1; index < list.elements.size(); ++index) {
      type->fields.push_back(parse_field(element(list, index), "union"));
    }
    return type;
  }

  auto type = make_type(TypeKind::application, form.location);
  type->name = head->name;
  for (std::size_t index = 1; index < list.elements.size(); ++index) {
    type->arguments.push_back(parse_type(element(list, index)));
  }
  return type;
}

}  // namespace

TypeError::TypeError(Diagnostic diagnostic)
    : std::runtime_error(diagnostic.message), diagnostic_(std::move(diagnostic)) {}

const Diagnostic& TypeError::diagnostic() const {
  return diagnostic_;
}

TypePtr parse_type(const Form& form) {
  if (const auto* symbol = as_symbol(form)) {
    bool found = false;
    const auto primitive = primitive_from_name(symbol->name, found);
    if (found) {
      auto type = make_type(TypeKind::primitive, form.location);
      type->primitive = primitive;
      return type;
    }

    auto type = make_type(TypeKind::name, form.location);
    type->name = symbol->name;
    return type;
  }

  if (const auto* list = as_list(form)) {
    return parse_type_list(form, *list);
  }

  fail(form.location, "expected type expression");
}

TypeDeclaration parse_type_declaration(const Form& form) {
  const auto* list = as_list(form);
  if (list == nullptr || list->elements.empty()) {
    fail(form.location, "type declaration must be a list");
  }

  const auto head = require_symbol_name(element(*list, 0), "type declaration head must be a symbol");
  if (head != "type") {
    fail(element(*list, 0).location, "expected type declaration");
  }

  if (list->elements.size() != 3 && list->elements.size() != 4) {
    fail(form.location, "type declaration expects either 3 or 4 forms");
  }

  TypeDeclaration declaration{
      require_symbol_name(element(*list, 1), "type declaration name must be a symbol"),
      element(*list, 1).location,
      {},
      nullptr,
  };

  std::size_t body_index = 2;
  if (list->elements.size() == 4) {
    const auto* parameters = as_list(element(*list, 2));
    if (parameters == nullptr) {
      fail(element(*list, 2).location, "type parameters must be a list");
    }

    std::unordered_set<std::string> parameter_names;
    for (const auto& parameter : parameters->elements) {
      auto name = require_symbol_name(*parameter, "type parameter must be a symbol");
      if (!parameter_names.insert(name).second) {
        fail(parameter->location, "type parameter redefines existing parameter");
      }
      declaration.parameters.push_back(TypeParameter{std::move(name), parameter->location});
    }
    body_index = 3;
  }

  declaration.body = parse_type(element(*list, body_index));
  return declaration;
}

void TypeEnvironment::declare(TypeDeclaration declaration) {
  if (declaration_indexes_.contains(declaration.name)) {
    fail(declaration.location, "type declaration redefines existing type");
  }

  const auto index = declarations_.size();
  declaration_indexes_.emplace(declaration.name, index);
  declarations_.push_back(std::move(declaration));
}

const TypeDeclaration* TypeEnvironment::find(std::string_view name) const {
  const auto found = declaration_indexes_.find(std::string(name));
  if (found == declaration_indexes_.end()) {
    return nullptr;
  }
  return &declarations_[found->second];
}

const std::vector<TypeDeclaration>& TypeEnvironment::declarations() const {
  return declarations_;
}

std::size_t TypeEnvironment::size() const {
  return declarations_.size();
}

TypePtr clone_type(const Type& type) {
  auto cloned = make_type(type.kind, type.location);
  cloned->primitive = type.primitive;
  cloned->name = type.name;
  cloned->array_size = type.array_size;

  if (type.element) {
    cloned->element = clone_type(*type.element);
  }
  if (type.result) {
    cloned->result = clone_type(*type.result);
  }
  cloned->arguments = clone_type_vector(type.arguments);

  cloned->fields.reserve(type.fields.size());
  for (const auto& field : type.fields) {
    cloned->fields.push_back(Field{field.name, clone_type(*field.type)});
  }

  cloned->alternatives.reserve(type.alternatives.size());
  for (const auto& alternative : type.alternatives) {
    cloned->alternatives.push_back(SumAlternative{
        alternative.name,
        clone_type_vector(alternative.payload),
    });
  }

  return cloned;
}

namespace {

TypePtr substitute_type_impl(const Type& type, const std::unordered_map<std::string, const Type*>& substitutions) {
  switch (type.kind) {
    case TypeKind::name: {
      const auto found = substitutions.find(type.name);
      if (found != substitutions.end()) {
        return clone_type(*found->second);
      }
      break;
    }

    case TypeKind::primitive:
    case TypeKind::pointer:
    case TypeKind::array:
    case TypeKind::slice:
    case TypeKind::function:
    case TypeKind::product:
    case TypeKind::sum:
    case TypeKind::union_:
    case TypeKind::application:
      break;
  }

  auto substituted = make_type(type.kind, type.location);
  substituted->primitive = type.primitive;
  substituted->name = type.name;
  substituted->array_size = type.array_size;

  if (type.element) {
    substituted->element = substitute_type_impl(*type.element, substitutions);
  }
  if (type.result) {
    substituted->result = substitute_type_impl(*type.result, substitutions);
  }
  substituted->arguments = substitute_type_vector(type.arguments, substitutions);

  substituted->fields.reserve(type.fields.size());
  for (const auto& field : type.fields) {
    substituted->fields.push_back(Field{field.name, substitute_type_impl(*field.type, substitutions)});
  }

  substituted->alternatives.reserve(type.alternatives.size());
  for (const auto& alternative : type.alternatives) {
    substituted->alternatives.push_back(SumAlternative{
        alternative.name,
        substitute_type_vector(alternative.payload, substitutions),
    });
  }

  return substituted;
}

}  // namespace

TypePtr instantiate_type_application(const Type& application, const TypeEnvironment& environment) {
  switch (application.kind) {
    case TypeKind::application:
      break;

    case TypeKind::primitive:
    case TypeKind::name:
    case TypeKind::pointer:
    case TypeKind::array:
    case TypeKind::slice:
    case TypeKind::function:
    case TypeKind::product:
    case TypeKind::sum:
    case TypeKind::union_:
      fail(application.location, "expected type application");
  }

  const auto* declaration = environment.find(application.name);
  if (declaration == nullptr) {
    fail(application.location, "unknown generic type name");
  }
  if (declaration->parameters.empty()) {
    fail(application.location, "type does not accept type arguments");
  }
  if (declaration->parameters.size() != application.arguments.size()) {
    fail(application.location, "generic type argument count mismatch");
  }

  std::unordered_map<std::string, const Type*> substitutions;
  for (std::size_t index = 0; index < declaration->parameters.size(); ++index) {
    substitutions.emplace(declaration->parameters[index].name, application.arguments[index].get());
  }

  return substitute_type_impl(*declaration->body, substitutions);
}

namespace {

std::string primitive_name(PrimitiveType primitive) {
  switch (primitive) {
    case PrimitiveType::i8:
      return "i8";
    case PrimitiveType::i16:
      return "i16";
    case PrimitiveType::i32:
      return "i32";
    case PrimitiveType::i64:
      return "i64";
    case PrimitiveType::isize:
      return "isize";
    case PrimitiveType::u8:
      return "u8";
    case PrimitiveType::u16:
      return "u16";
    case PrimitiveType::u32:
      return "u32";
    case PrimitiveType::u64:
      return "u64";
    case PrimitiveType::usize:
      return "usize";
    case PrimitiveType::f32:
      return "f32";
    case PrimitiveType::f64:
      return "f64";
    case PrimitiveType::bool_:
      return "bool";
    case PrimitiveType::unit:
      return "unit";
    case PrimitiveType::void_:
      return "void";
  }
}

void append_type_string(std::ostream& out, const Type& type) {
  switch (type.kind) {
    case TypeKind::primitive:
      out << primitive_name(type.primitive);
      return;

    case TypeKind::name:
      out << type.name;
      return;

    case TypeKind::pointer:
      out << "(& ";
      append_type_string(out, *type.element);
      out << ')';
      return;

    case TypeKind::array:
      out << "(array ";
      append_type_string(out, *type.element);
      out << ' ' << type.array_size << ')';
      return;

    case TypeKind::slice:
      out << "(slice ";
      append_type_string(out, *type.element);
      out << ')';
      return;

    case TypeKind::function:
      out << "(fn (";
      for (std::size_t index = 0; index < type.arguments.size(); ++index) {
        if (index != 0) {
          out << ' ';
        }
        append_type_string(out, *type.arguments[index]);
      }
      out << ") ";
      append_type_string(out, *type.result);
      out << ')';
      return;

    case TypeKind::product:
      out << "(product";
      for (const auto& field : type.fields) {
        out << " (" << field.name << ' ';
        append_type_string(out, *field.type);
        out << ')';
      }
      out << ')';
      return;

    case TypeKind::sum:
      out << "(sum";
      for (const auto& alternative : type.alternatives) {
        if (alternative.payload.empty()) {
          out << ' ' << alternative.name;
        } else {
          out << " (" << alternative.name;
          for (const auto& payload : alternative.payload) {
            out << ' ';
            append_type_string(out, *payload);
          }
          out << ')';
        }
      }
      out << ')';
      return;

    case TypeKind::union_:
      out << "(union";
      for (const auto& field : type.fields) {
        out << " (" << field.name << ' ';
        append_type_string(out, *field.type);
        out << ')';
      }
      out << ')';
      return;

    case TypeKind::application:
      out << '(' << type.name;
      for (const auto& argument : type.arguments) {
        out << ' ';
        append_type_string(out, *argument);
      }
      out << ')';
      return;
  }
}

}  // namespace

std::string type_to_string(const Type& type) {
  std::ostringstream out;
  append_type_string(out, type);
  return out.str();
}

const MonomorphizedType& MonomorphizationRegistry::intern(const Type& application,
                                                         const TypeEnvironment& environment) {
  switch (application.kind) {
    case TypeKind::application:
      break;

    case TypeKind::primitive:
    case TypeKind::name:
    case TypeKind::pointer:
    case TypeKind::array:
    case TypeKind::slice:
    case TypeKind::function:
    case TypeKind::product:
    case TypeKind::sum:
    case TypeKind::union_:
      fail(application.location, "expected type application");
  }

  const auto key = type_to_string(application);
  const auto existing = instantiation_indexes_.find(key);
  if (existing != instantiation_indexes_.end()) {
    return instantiations_[existing->second];
  }

  auto instantiated = instantiate_type_application(application, environment);
  const auto index = instantiations_.size();
  instantiation_indexes_.emplace(key, index);
  instantiations_.push_back(MonomorphizedType{key, std::move(instantiated)});
  return instantiations_.back();
}

const std::vector<MonomorphizedType>& MonomorphizationRegistry::instantiations() const {
  return instantiations_;
}

std::size_t MonomorphizationRegistry::size() const {
  return instantiations_.size();
}

namespace {

void validate_type_reference_impl(const Type& type,
                                  const TypeEnvironment& environment,
                                  const std::unordered_set<std::string>& parameters) {
  switch (type.kind) {
    case TypeKind::primitive:
      return;

    case TypeKind::name: {
      if (parameters.contains(type.name)) {
        return;
      }
      const auto* declaration = environment.find(type.name);
      if (declaration == nullptr) {
        fail(type.location, "unknown type name");
      }
      if (!declaration->parameters.empty()) {
        fail(type.location, "generic type requires type arguments");
      }
      return;
    }

    case TypeKind::application: {
      if (parameters.contains(type.name)) {
        fail(type.location, "type parameter cannot be used as a type constructor");
      }
      const auto* declaration = environment.find(type.name);
      if (declaration == nullptr) {
        fail(type.location, "unknown generic type name");
      }
      if (declaration->parameters.empty()) {
        fail(type.location, "type does not accept type arguments");
      }
      if (declaration->parameters.size() != type.arguments.size()) {
        fail(type.location, "generic type argument count mismatch");
      }
      for (const auto& argument : type.arguments) {
        validate_type_reference_impl(*argument, environment, parameters);
      }
      return;
    }

    case TypeKind::pointer:
    case TypeKind::array:
    case TypeKind::slice:
      validate_type_reference_impl(*type.element, environment, parameters);
      return;

    case TypeKind::function:
      for (const auto& argument : type.arguments) {
        validate_type_reference_impl(*argument, environment, parameters);
      }
      validate_type_reference_impl(*type.result, environment, parameters);
      return;

    case TypeKind::product:
    case TypeKind::union_:
      for (const auto& field : type.fields) {
        validate_type_reference_impl(*field.type, environment, parameters);
      }
      return;

    case TypeKind::sum:
      for (const auto& alternative : type.alternatives) {
        for (const auto& payload : alternative.payload) {
          validate_type_reference_impl(*payload, environment, parameters);
        }
      }
      return;
  }
}

}  // namespace

void validate_type_reference(const Type& type,
                             const TypeEnvironment& environment,
                             const std::unordered_set<std::string>& parameters) {
  validate_type_reference_impl(type, environment, parameters);
}

void validate_type_references(const TypeEnvironment& environment) {
  for (const auto& declaration : environment.declarations()) {
    std::unordered_set<std::string> parameters;
    for (const auto& parameter : declaration.parameters) {
      parameters.insert(parameter.name);
    }
    validate_type_reference_impl(*declaration.body, environment, parameters);
  }
}

}  // namespace termis
