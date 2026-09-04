#pragma once

#include "reader.hpp"
#include "type.hpp"

#include <memory>
#include <stdexcept>
#include <string>
#include <optional>
#include <unordered_map>
#include <vector>

namespace termis {

class SemanticError final : public std::runtime_error {
 public:
  explicit SemanticError(Diagnostic diagnostic);

  const Diagnostic& diagnostic() const;

 private:
  Diagnostic diagnostic_;
};

enum class SemanticKind {
  type_declaration,
  provide_declaration,
  class_declaration,
  implements_declaration,
  function_declaration,
  extern_function_declaration,
  let_expression,
  do_expression,
  match_expression,
  const_declaration,
  application,
  list_expression,
  atom,
};

struct SemanticNode {
  SemanticKind kind;
  const Form* form;
};

using SemanticNodePtr = std::unique_ptr<SemanticNode>;

struct FunctionParameter {
  std::string name;
  TypePtr type;
};

struct TypeRequirement {
  std::string class_name;
  SourceLocation location;
  std::vector<TypePtr> arguments;
};

struct FunctionSignatureDeclaration {
  std::string name;
  SourceLocation location;
  std::vector<TypeParameter> type_parameters;
  std::vector<FunctionParameter> parameters;
  TypePtr result;
  std::vector<TypeRequirement> requirements;
  std::optional<std::string> documentation;
};

struct ProvideDeclaration {
  std::string name;
  SourceLocation location;
  std::vector<FunctionSignatureDeclaration> functions;
};

class ProvideEnvironment {
 public:
  void declare(ProvideDeclaration declaration);

  const ProvideDeclaration* find(std::string_view name) const;
  const std::vector<ProvideDeclaration>& declarations() const;
  std::size_t size() const;

 private:
  std::vector<ProvideDeclaration> declarations_;
  std::unordered_map<std::string, std::size_t> declaration_indexes_;
};

struct ClassDeclaration {
  std::string name;
  SourceLocation location;
  std::vector<TypeParameter> parameters;
  std::vector<FunctionSignatureDeclaration> methods;
};

struct ImplementsDeclaration {
  std::string class_name;
  SourceLocation location;
  std::vector<TypePtr> arguments;
  std::vector<FunctionSignatureDeclaration> methods;
};

class ClassEnvironment {
 public:
  void declare(ClassDeclaration declaration);
  void declare(ImplementsDeclaration declaration);

  const ClassDeclaration* find(std::string_view name) const;
  const std::vector<ClassDeclaration>& declarations() const;
  const std::vector<ImplementsDeclaration>& implementations() const;
  std::size_t size() const;

 private:
  std::vector<ClassDeclaration> declarations_;
  std::vector<ImplementsDeclaration> implementations_;
  std::unordered_map<std::string, std::size_t> declaration_indexes_;
};

struct Program {
  std::vector<SemanticNodePtr> forms;
  TypeEnvironment types;
  ProvideEnvironment provides;
  ClassEnvironment classes;
};

Program analyze_forms(const std::vector<FormPtr>& forms);

}  // namespace termis
