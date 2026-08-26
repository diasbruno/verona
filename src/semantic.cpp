#include "semantic.hpp"
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

std::size_t element_count(const Form& form) {
  if (const auto* list = as_list(form)) {
    return list->elements.size();
  }
  return 0;
}

const Form& list_element(const Form& form, std::size_t index) {
  return *as_list(form)->elements[index];
}

const Form& element(const List& list, std::size_t index) {
  return *list.elements[index];
}

void require_count_at_least(const Form& form, std::size_t minimum, std::string message) {
  if (element_count(form) < minimum) {
    throw SemanticError(Diagnostic{form.location, std::move(message)});
  }
}

void require_exact_count(const Form& form, std::size_t expected, std::string message) {
  if (element_count(form) != expected) {
    throw SemanticError(Diagnostic{form.location, std::move(message)});
  }
}

void require_symbol(const Form& form, std::string message) {
  if (as_symbol(form) == nullptr) {
    throw SemanticError(Diagnostic{form.location, std::move(message)});
  }
}

void require_list(const Form& form, std::string message) {
  if (as_list(form) == nullptr) {
    throw SemanticError(Diagnostic{form.location, std::move(message)});
  }
}

std::vector<TypeParameter> parse_type_parameters(const Form& form, std::string_view parameter_kind) {
  const auto* parameters = as_list(form);
  if (parameters == nullptr) {
    throw SemanticError(Diagnostic{form.location, std::string(parameter_kind) + " parameters must be a list"});
  }

  std::vector<TypeParameter> parsed;
  std::unordered_set<std::string> parameter_names;
  for (const auto& parameter : parameters->elements) {
    const auto* symbol = as_symbol(*parameter);
    if (symbol == nullptr) {
      throw SemanticError(Diagnostic{parameter->location, std::string(parameter_kind) + " parameter must be a symbol"});
    }
    if (!parameter_names.insert(symbol->name).second) {
      throw SemanticError(Diagnostic{parameter->location,
                                     std::string(parameter_kind) + " parameter redefines existing parameter"});
    }
    parsed.push_back(TypeParameter{symbol->name, parameter->location});
  }
  return parsed;
}

FunctionParameter parse_function_parameter(const Form& form, std::string_view owner) {
  const auto* parameter = as_list(form);
  if (parameter == nullptr || parameter->elements.size() != 2) {
    throw SemanticError(Diagnostic{form.location, std::string(owner) + " parameter requires name and type"});
  }
  const auto* parameter_name = as_symbol(list_element(form, 0));
  if (parameter_name == nullptr) {
    throw SemanticError(Diagnostic{list_element(form, 0).location,
                                   std::string(owner) + " parameter name must be a symbol"});
  }
  return FunctionParameter{
      parameter_name->name,
      parse_type(list_element(form, 1)),
  };
}

TypeRequirement parse_type_requirement(const Form& form) {
  const auto* list = as_list(form);
  if (list == nullptr || list->elements.empty()) {
    throw SemanticError(Diagnostic{form.location, "type requirement requires a class name"});
  }
  const auto* class_name = as_symbol(list_element(form, 0));
  if (class_name == nullptr) {
    throw SemanticError(Diagnostic{list_element(form, 0).location, "type requirement class name must be a symbol"});
  }

  TypeRequirement requirement{class_name->name, list_element(form, 0).location, {}};
  for (std::size_t index = 1; index < list->elements.size(); ++index) {
    requirement.arguments.push_back(parse_type(list_element(form, index)));
  }
  return requirement;
}

bool is_stable_signature(const Form& form) {
  const auto* fields = as_list(form);
  if (fields == nullptr || fields->elements.empty()) {
    return false;
  }
  const auto* first_field = as_list(list_element(form, 0));
  if (first_field == nullptr || first_field->elements.empty()) {
    return false;
  }
  const auto* first_head = as_symbol(element(*first_field, 0));
  return first_head != nullptr &&
         (first_head->name == "params" || first_head->name == "return" ||
          first_head->name == "requires" || first_head->name == "documentation");
}

void parse_stable_signature_fields(FunctionSignatureDeclaration& signature,
                                   const Form& form,
                                   std::string_view owner) {
  const auto* fields = as_list(form);
  if (fields == nullptr) {
    throw SemanticError(Diagnostic{form.location, std::string(owner) + " signature must be a list"});
  }

  bool found_params = false;
  bool found_return = false;
  bool found_requires = false;
  bool found_documentation = false;
  for (const auto& field_form : fields->elements) {
    const auto* field = as_list(*field_form);
    if (field == nullptr || field->elements.empty()) {
      throw SemanticError(Diagnostic{field_form->location, std::string(owner) + " signature field must be a list"});
    }
    const auto* head = as_symbol(element(*field, 0));
    if (head == nullptr) {
      throw SemanticError(Diagnostic{element(*field, 0).location,
                                     std::string(owner) + " signature field name must be a symbol"});
    }

    if (head->name == "params") {
      if (found_params || field->elements.size() != 2) {
        throw SemanticError(Diagnostic{field_form->location, std::string(owner) + " params field expects one list"});
      }
      const auto* parameters = as_list(element(*field, 1));
      if (parameters == nullptr) {
        throw SemanticError(Diagnostic{element(*field, 1).location,
                                       std::string(owner) + " params field value must be a list"});
      }
      for (const auto& parameter_form : parameters->elements) {
        signature.parameters.push_back(parse_function_parameter(*parameter_form, owner));
      }
      found_params = true;
      continue;
    }

    if (head->name == "return") {
      if (found_return || field->elements.size() != 2) {
        throw SemanticError(Diagnostic{field_form->location, std::string(owner) + " return field expects one type"});
      }
      signature.result = parse_type(element(*field, 1));
      found_return = true;
      continue;
    }

    if (head->name == "requires") {
      if (found_requires || field->elements.size() != 2) {
        throw SemanticError(Diagnostic{field_form->location, std::string(owner) + " requires field expects one list"});
      }
      const auto* requirements = as_list(element(*field, 1));
      if (requirements == nullptr) {
        throw SemanticError(Diagnostic{element(*field, 1).location,
                                       std::string(owner) + " requires field value must be a list"});
      }
      for (const auto& requirement_form : requirements->elements) {
        signature.requirements.push_back(parse_type_requirement(*requirement_form));
      }
      found_requires = true;
      continue;
    }

    if (head->name == "documentation") {
      if (found_documentation || field->elements.size() != 2) {
        throw SemanticError(Diagnostic{field_form->location,
                                       std::string(owner) + " documentation field expects one string"});
      }
      const auto* documentation = std::get_if<StringLiteral>(&element(*field, 1).kind);
      if (documentation == nullptr) {
        throw SemanticError(Diagnostic{element(*field, 1).location,
                                       std::string(owner) + " documentation field value must be a string"});
      }
      signature.documentation = documentation->value;
      found_documentation = true;
      continue;
    }

    throw SemanticError(Diagnostic{field_form->location, "unknown function signature field"});
  }

  if (!found_params) {
    throw SemanticError(Diagnostic{form.location, std::string(owner) + " signature requires params field"});
  }
  if (!found_return) {
    throw SemanticError(Diagnostic{form.location, std::string(owner) + " signature requires return field"});
  }
  if (!found_requires) {
    throw SemanticError(Diagnostic{form.location, std::string(owner) + " signature requires requires field"});
  }
}

FunctionSignatureDeclaration parse_function_signature(const Form& form, std::string_view owner, bool allow_body = false) {
  const auto* list = as_list(form);
  if (list == nullptr || list->elements.size() < 3 || (!allow_body && list->elements.size() > 4)) {
    throw SemanticError(Diagnostic{form.location, std::string(owner) + " method requires fn, signature, and optional body"});
  }
  const auto* head = as_symbol(list_element(form, 0));
  if (head == nullptr || head->name != "fn") {
    throw SemanticError(Diagnostic{list_element(form, 0).location, std::string(owner) + " method must be a function signature"});
  }
  const auto* name = as_symbol(list_element(form, 1));
  if (name == nullptr) {
    throw SemanticError(Diagnostic{list_element(form, 1).location, std::string(owner) + " method name must be a symbol"});
  }

  FunctionSignatureDeclaration signature{name->name, list_element(form, 1).location, {}, nullptr, {}, std::nullopt};
  if (is_stable_signature(list_element(form, 2))) {
    parse_stable_signature_fields(signature, list_element(form, 2), owner);
    if (!allow_body && list->elements.size() != 3) {
      throw SemanticError(Diagnostic{form.location, std::string(owner) + " method signature cannot have a body"});
    }
    return signature;
  }

  if (list->elements.size() < 4 || (!allow_body && list->elements.size() != 4)) {
    throw SemanticError(Diagnostic{form.location, std::string(owner) + " method requires fn, parameters, and result type"});
  }
  const auto* parameters = as_list(list_element(form, 2));
  if (parameters == nullptr) {
    throw SemanticError(Diagnostic{list_element(form, 2).location, std::string(owner) + " method parameters must be a list"});
  }

  signature.result = parse_type(list_element(form, 3));
  for (const auto& parameter_form : parameters->elements) {
    signature.parameters.push_back(parse_function_parameter(*parameter_form, std::string(owner) + " method"));
  }
  return signature;
}

ClassDeclaration parse_class_declaration(const Form& form) {
  const auto* list = as_list(form);
  if (list == nullptr || list->elements.size() < 4) {
    throw SemanticError(Diagnostic{form.location, "class declaration requires a name, parameters, and methods"});
  }
  const auto* name = as_symbol(list_element(form, 1));
  if (name == nullptr) {
    throw SemanticError(Diagnostic{list_element(form, 1).location, "class name must be a symbol"});
  }

  ClassDeclaration declaration{
      name->name,
      list_element(form, 1).location,
      parse_type_parameters(list_element(form, 2), "class"),
      {},
  };
  std::unordered_set<std::string> method_names;
  for (std::size_t index = 3; index < list->elements.size(); ++index) {
    auto method = parse_function_signature(list_element(form, index), "class");
    if (!method_names.insert(method.name).second) {
      throw SemanticError(Diagnostic{method.location, "class method redefines existing method"});
    }
    declaration.methods.push_back(std::move(method));
  }
  return declaration;
}

ImplementsDeclaration parse_implements_declaration(const Form& form) {
  const auto* list = as_list(form);
  if (list == nullptr || list->elements.size() < 4) {
    throw SemanticError(Diagnostic{form.location, "implements declaration requires a class, arguments, and methods"});
  }
  const auto* class_name = as_symbol(list_element(form, 1));
  if (class_name == nullptr) {
    throw SemanticError(Diagnostic{list_element(form, 1).location, "implements class name must be a symbol"});
  }
  const auto* arguments = as_list(list_element(form, 2));
  if (arguments == nullptr) {
    throw SemanticError(Diagnostic{list_element(form, 2).location, "implements arguments must be a list"});
  }

  ImplementsDeclaration declaration{class_name->name, list_element(form, 1).location, {}, {}};
  for (const auto& argument : arguments->elements) {
    declaration.arguments.push_back(parse_type(*argument));
  }

  std::unordered_set<std::string> method_names;
  for (std::size_t index = 3; index < list->elements.size(); ++index) {
    auto method = parse_function_signature(list_element(form, index), "implements", true);
    if (!method_names.insert(method.name).second) {
      throw SemanticError(Diagnostic{method.location, "implements method redefines existing method"});
    }
    declaration.methods.push_back(std::move(method));
  }
  return declaration;
}

void require_optional_string(const Form& form, std::string message) {
  if (!std::holds_alternative<StringLiteral>(form.kind)) {
    throw SemanticError(Diagnostic{form.location, std::move(message)});
  }
}

SemanticKind classify(const Form& form) {
  const auto* list = as_list(form);
  if (list == nullptr) {
    return SemanticKind::atom;
  }

  if (list->elements.empty()) {
    return SemanticKind::list_expression;
  }

  const auto* head = as_symbol(*list->elements.front());
  if (head == nullptr) {
    return SemanticKind::list_expression;
  }

  if (head->name == "list") {
    return SemanticKind::list_expression;
  }

  if (head->name == "type") {
    require_count_at_least(form, 3, "type declaration requires a name and body");
    require_symbol(list_element(form, 1), "type declaration name must be a symbol");
    if (element_count(form) == 4) {
      require_list(list_element(form, 2), "type parameters must be a list");
      (void)parse_type(list_element(form, 3));
    } else {
      require_exact_count(form, 3, "type declaration expects either 3 or 4 forms");
      (void)parse_type(list_element(form, 2));
    }
    return SemanticKind::type_declaration;
  }

  if (head->name == "class") {
    require_count_at_least(form, 4, "class declaration requires a name, parameters, and methods");
    require_symbol(list_element(form, 1), "class name must be a symbol");
    require_list(list_element(form, 2), "class parameters must be a list");
    return SemanticKind::class_declaration;
  }

  if (head->name == "implements") {
    require_count_at_least(form, 4, "implements declaration requires a class, arguments, and methods");
    require_symbol(list_element(form, 1), "implements class name must be a symbol");
    require_list(list_element(form, 2), "implements arguments must be a list");
    return SemanticKind::implements_declaration;
  }

  if (head->name == "fn") {
    require_count_at_least(form, 4, "function declaration requires name, signature, and body");
    require_symbol(list_element(form, 1), "function name must be a symbol");
    require_list(list_element(form, 2), "function signature must be a list");
    return SemanticKind::function_declaration;
  }

  if (head->name == "extern") {
    require_count_at_least(form, 4, "extern function declaration requires fn, name, and signature");
    if (element_count(form) < 4 || element_count(form) > 6) {
      throw SemanticError(Diagnostic{form.location, "extern function declaration expects 4, 5, or 6 forms"});
    }
    const auto* extern_kind = as_symbol(list_element(form, 1));
    if (extern_kind == nullptr || extern_kind->name != "fn") {
      throw SemanticError(Diagnostic{list_element(form, 1).location,
                                     "extern declaration currently supports only fn"});
    }
    require_symbol(list_element(form, 2), "extern function name must be a symbol");
    require_list(list_element(form, 3), "extern function signature must be a list");
    if (element_count(form) == 5 && is_stable_signature(list_element(form, 3))) {
      require_optional_string(list_element(form, 4), "extern function link name must be a string");
    } else if (element_count(form) == 6) {
      require_optional_string(list_element(form, 5), "extern function link name must be a string");
    }
    return SemanticKind::extern_function_declaration;
  }

  if (head->name == "let") {
    require_count_at_least(form, 3, "let expression requires bindings and a body");
    require_list(list_element(form, 1), "let bindings must be a list");
    return SemanticKind::let_expression;
  }

  if (head->name == "do") {
    require_count_at_least(form, 2, "do expression requires at least one body form");
    return SemanticKind::do_expression;
  }

  if (head->name == "match") {
    require_count_at_least(form, 3, "match expression requires a value and at least one arm");
    return SemanticKind::match_expression;
  }

  if (head->name == "const") {
    require_exact_count(form, 3, "const declaration requires a name and value");
    require_symbol(list_element(form, 1), "const name must be a symbol");
    return SemanticKind::const_declaration;
  }

  return SemanticKind::application;
}

SemanticNodePtr analyze_form(const Form& form) {
  return std::make_unique<SemanticNode>(SemanticNode{classify(form), &form});
}

void validate_signature_types(const FunctionSignatureDeclaration& signature,
                              const TypeEnvironment& types,
                              const ClassEnvironment& classes,
                              const std::unordered_set<std::string>& parameters) {
  for (const auto& parameter : signature.parameters) {
    validate_type_reference(*parameter.type, types, parameters);
  }
  validate_type_reference(*signature.result, types, parameters);
  for (const auto& requirement : signature.requirements) {
    const auto* declaration = classes.find(requirement.class_name);
    if (declaration == nullptr) {
      throw SemanticError(Diagnostic{requirement.location, "type requirement references unknown class"});
    }
    if (declaration->parameters.size() != requirement.arguments.size()) {
      throw SemanticError(Diagnostic{requirement.location, "type requirement class argument count mismatch"});
    }
    for (const auto& argument : requirement.arguments) {
      validate_type_reference(*argument, types, parameters);
    }
  }
}

bool type_matches_implementation(const Type& required,
                           const Type& actual,
                           const std::unordered_map<std::string, const Type*>& substitutions) {
  if (required.kind == TypeKind::name) {
    const auto found = substitutions.find(required.name);
    if (found != substitutions.end()) {
      return type_to_string(actual) == type_to_string(*found->second);
    }
  }

  if (required.kind != actual.kind) {
    return false;
  }

  switch (required.kind) {
    case TypeKind::primitive:
      return required.primitive == actual.primitive;

    case TypeKind::name:
      return required.name == actual.name;

    case TypeKind::pointer:
    case TypeKind::slice:
      return type_matches_implementation(*required.element, *actual.element, substitutions);

    case TypeKind::array:
      return required.array_size == actual.array_size &&
             type_matches_implementation(*required.element, *actual.element, substitutions);

    case TypeKind::function:
      if (required.arguments.size() != actual.arguments.size()) {
        return false;
      }
      for (std::size_t index = 0; index < required.arguments.size(); ++index) {
        if (!type_matches_implementation(*required.arguments[index], *actual.arguments[index], substitutions)) {
          return false;
        }
      }
      return type_matches_implementation(*required.result, *actual.result, substitutions);

    case TypeKind::product:
    case TypeKind::union_:
      if (required.fields.size() != actual.fields.size()) {
        return false;
      }
      for (std::size_t index = 0; index < required.fields.size(); ++index) {
        if (required.fields[index].name != actual.fields[index].name ||
            !type_matches_implementation(*required.fields[index].type, *actual.fields[index].type, substitutions)) {
          return false;
        }
      }
      return true;

    case TypeKind::sum:
      if (required.alternatives.size() != actual.alternatives.size()) {
        return false;
      }
      for (std::size_t index = 0; index < required.alternatives.size(); ++index) {
        const auto& required_alternative = required.alternatives[index];
        const auto& actual_alternative = actual.alternatives[index];
        if (required_alternative.name != actual_alternative.name ||
            required_alternative.payload.size() != actual_alternative.payload.size()) {
          return false;
        }
        for (std::size_t payload_index = 0; payload_index < required_alternative.payload.size(); ++payload_index) {
          if (!type_matches_implementation(*required_alternative.payload[payload_index],
                                     *actual_alternative.payload[payload_index],
                                     substitutions)) {
            return false;
          }
        }
      }
      return true;

    case TypeKind::application:
      if (required.name != actual.name || required.arguments.size() != actual.arguments.size()) {
        return false;
      }
      for (std::size_t index = 0; index < required.arguments.size(); ++index) {
        if (!type_matches_implementation(*required.arguments[index], *actual.arguments[index], substitutions)) {
          return false;
        }
      }
      return true;
  }
}

bool method_matches_implementation(const FunctionSignatureDeclaration& required,
                             const FunctionSignatureDeclaration& actual,
                             const std::unordered_map<std::string, const Type*>& substitutions) {
  if (required.parameters.size() != actual.parameters.size()) {
    return false;
  }
  for (std::size_t index = 0; index < required.parameters.size(); ++index) {
    if (!type_matches_implementation(*required.parameters[index].type, *actual.parameters[index].type, substitutions)) {
      return false;
    }
  }
  return type_matches_implementation(*required.result, *actual.result, substitutions);
}

}  // namespace

SemanticError::SemanticError(Diagnostic diagnostic)
    : std::runtime_error(diagnostic.message), diagnostic_(std::move(diagnostic)) {}

const Diagnostic& SemanticError::diagnostic() const {
  return diagnostic_;
}

void ClassEnvironment::declare(ClassDeclaration declaration) {
  if (declaration_indexes_.contains(declaration.name)) {
    throw SemanticError(Diagnostic{declaration.location, "class declaration redefines existing class"});
  }

  const auto index = declarations_.size();
  declaration_indexes_.emplace(declaration.name, index);
  declarations_.push_back(std::move(declaration));
}

void ClassEnvironment::declare(ImplementsDeclaration declaration) {
  implementations_.push_back(std::move(declaration));
}

const ClassDeclaration* ClassEnvironment::find(std::string_view name) const {
  const auto found = declaration_indexes_.find(std::string(name));
  if (found == declaration_indexes_.end()) {
    return nullptr;
  }
  return &declarations_[found->second];
}

const std::vector<ClassDeclaration>& ClassEnvironment::declarations() const {
  return declarations_;
}

const std::vector<ImplementsDeclaration>& ClassEnvironment::implementations() const {
  return implementations_;
}

std::size_t ClassEnvironment::size() const {
  return declarations_.size();
}

Program analyze_forms(const std::vector<FormPtr>& forms) {
  Program program;
  program.forms.reserve(forms.size());
  for (const auto& form : forms) {
    auto node = analyze_form(*form);
    switch (node->kind) {
      case SemanticKind::type_declaration:
        program.types.declare(parse_type_declaration(*form));
        break;

      case SemanticKind::class_declaration:
        program.classes.declare(parse_class_declaration(*form));
        break;

      case SemanticKind::implements_declaration:
        program.classes.declare(parse_implements_declaration(*form));
        break;

      case SemanticKind::function_declaration:
      case SemanticKind::extern_function_declaration:
      case SemanticKind::let_expression:
      case SemanticKind::do_expression:
      case SemanticKind::match_expression:
      case SemanticKind::const_declaration:
      case SemanticKind::application:
      case SemanticKind::list_expression:
      case SemanticKind::atom:
        break;
    }
    program.forms.push_back(std::move(node));
  }
  validate_type_references(program.types);
  for (const auto& declaration : program.classes.declarations()) {
    std::unordered_set<std::string> parameters;
    for (const auto& parameter : declaration.parameters) {
      parameters.insert(parameter.name);
    }
    for (const auto& method : declaration.methods) {
      validate_signature_types(method, program.types, program.classes, parameters);
    }
  }
  for (const auto& implements : program.classes.implementations()) {
    const auto* declaration = program.classes.find(implements.class_name);
    if (declaration == nullptr) {
      throw SemanticError(Diagnostic{implements.location, "implements references unknown class"});
    }
    if (declaration->parameters.size() != implements.arguments.size()) {
      throw SemanticError(Diagnostic{implements.location, "implements class argument count mismatch"});
    }
    std::unordered_map<std::string, const Type*> substitutions;
    for (std::size_t index = 0; index < declaration->parameters.size(); ++index) {
      substitutions.emplace(declaration->parameters[index].name, implements.arguments[index].get());
    }
    std::unordered_set<std::string> no_parameters;
    for (const auto& argument : implements.arguments) {
      validate_type_reference(*argument, program.types, no_parameters);
    }
    for (const auto& required_method : declaration->methods) {
      bool found = false;
      for (const auto& method : implements.methods) {
        if (method.name == required_method.name) {
          found = true;
          break;
        }
      }
      if (!found) {
        throw SemanticError(Diagnostic{implements.location, "implements is missing class method"});
      }
    }
    for (const auto& method : implements.methods) {
      bool found = false;
      for (const auto& required_method : declaration->methods) {
        if (method.name == required_method.name) {
          found = true;
          break;
        }
      }
      if (!found) {
        throw SemanticError(Diagnostic{method.location, "implements method is not declared by class"});
      }
      validate_signature_types(method, program.types, program.classes, no_parameters);
      for (const auto& required_method : declaration->methods) {
        if (method.name == required_method.name &&
            !method_matches_implementation(required_method, method, substitutions)) {
          throw SemanticError(Diagnostic{method.location, "implements method signature does not match class method"});
        }
      }
    }
  }
  return program;
}

}  // namespace termis
