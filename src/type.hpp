#pragma once

#include "reader.hpp"

#include <cstdint>
#include <memory>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace termis {

class TypeError final : public std::runtime_error {
 public:
  explicit TypeError(Diagnostic diagnostic);

  const Diagnostic& diagnostic() const;

 private:
  Diagnostic diagnostic_;
};

enum class PrimitiveType {
  i8,
  i16,
  i32,
  i64,
  u8,
  u16,
  u32,
  u64,
  isize,
  usize,
  f32,
  f64,
  bool_,
  unit,
  void_,
};

enum class TypeKind {
  primitive,
  name,
  pointer,
  array,
  slice,
  function,
  product,
  sum,
  union_,
  application,
};

struct Type;
using TypePtr = std::unique_ptr<Type>;

struct Field {
  std::string name;
  TypePtr type;
};

struct SumAlternative {
  std::string name;
  std::vector<TypePtr> payload;
};

struct Type {
  TypeKind kind = TypeKind::name;
  SourceLocation location;

  PrimitiveType primitive = PrimitiveType::unit;
  std::string name;

  TypePtr element;
  TypePtr result;
  std::vector<TypePtr> arguments;
  std::vector<Field> fields;
  std::vector<SumAlternative> alternatives;

  std::int64_t array_size = 0;
};

struct TypeParameter {
  std::string name;
  SourceLocation location;
};

struct TypeDeclaration {
  std::string name;
  SourceLocation location;
  std::vector<TypeParameter> parameters;
  TypePtr body;
};

class TypeEnvironment {
 public:
  void declare(TypeDeclaration declaration);

  const TypeDeclaration* find(std::string_view name) const;
  const std::vector<TypeDeclaration>& declarations() const;
  std::size_t size() const;

 private:
  std::vector<TypeDeclaration> declarations_;
  std::unordered_map<std::string, std::size_t> declaration_indexes_;
};

struct MonomorphizedType {
  std::string key;
  TypePtr type;
};

class MonomorphizationRegistry {
 public:
  const MonomorphizedType& intern(const Type& application, const TypeEnvironment& environment);

  const std::vector<MonomorphizedType>& instantiations() const;
  std::size_t size() const;

 private:
  std::vector<MonomorphizedType> instantiations_;
  std::unordered_map<std::string, std::size_t> instantiation_indexes_;
};

TypePtr parse_type(const Form& form);
TypeDeclaration parse_type_declaration(const Form& form);
TypePtr clone_type(const Type& type);
TypePtr instantiate_type_application(const Type& application, const TypeEnvironment& environment);
std::string type_to_string(const Type& type);
void validate_type_reference(const Type& type,
                             const TypeEnvironment& environment,
                             const std::unordered_set<std::string>& parameters);
void validate_type_references(const TypeEnvironment& environment);

}  // namespace termis
