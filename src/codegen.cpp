#include "codegen.hpp"

#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/CodeGen.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/TargetParser/Host.h>
#include <llvm/TargetParser/Triple.h>

#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

namespace termis {
namespace {

struct Value {
  llvm::Type* type = nullptr;
  llvm::Value* value = nullptr;
};

struct FunctionSignature {
  std::string name;
  std::string link_name;
  std::vector<std::pair<std::string, llvm::Type*>> parameters;
  llvm::Type* return_type = nullptr;
  bool external = false;
  std::size_t body_index = 4;
};

struct ArmResult {
  Value value;
  llvm::BasicBlock* block = nullptr;
};

const Symbol* as_symbol(const Form& form) {
  return std::get_if<Symbol>(&form.kind);
}

const List* as_list(const Form& form) {
  return std::get_if<List>(&form.kind);
}

const IntegerLiteral* as_integer(const Form& form) {
  return std::get_if<IntegerLiteral>(&form.kind);
}

const StringLiteral* as_string(const Form& form) {
  return std::get_if<StringLiteral>(&form.kind);
}

bool is_unit(const Form& form) {
  return std::holds_alternative<UnitLiteral>(form.kind);
}

std::optional<bool> bool_pattern_value(const Form& form) {
  const auto* symbol = as_symbol(form);
  if (symbol == nullptr) {
    return std::nullopt;
  }
  if (symbol->name == "true") {
    return true;
  }
  if (symbol->name == "false") {
    return false;
  }
  return std::nullopt;
}

const Form& element(const List& list, std::size_t index) {
  return *list.elements[index];
}

[[noreturn]] void fail(SourceLocation location, std::string message) {
  throw CodegenError(Diagnostic{location, std::move(message)});
}

std::string symbol_name(const Form& form, std::string message) {
  const auto* symbol = as_symbol(form);
  if (symbol == nullptr) {
    fail(form.location, std::move(message));
  }
  return symbol->name;
}

llvm::Type* primitive_llvm_type(llvm::LLVMContext& context,
                                const llvm::Module& module,
                                PrimitiveType primitive,
                                SourceLocation location) {
  switch (primitive) {
    case PrimitiveType::bool_:
      return llvm::Type::getInt1Ty(context);
    case PrimitiveType::i8:
    case PrimitiveType::u8:
      return llvm::Type::getInt8Ty(context);
    case PrimitiveType::i16:
    case PrimitiveType::u16:
      return llvm::Type::getInt16Ty(context);
    case PrimitiveType::i32:
    case PrimitiveType::u32:
      return llvm::Type::getInt32Ty(context);
    case PrimitiveType::i64:
    case PrimitiveType::u64:
      return llvm::Type::getInt64Ty(context);
    case PrimitiveType::isize:
    case PrimitiveType::usize:
      return module.getDataLayout().getIntPtrType(context);
    case PrimitiveType::unit:
    case PrimitiveType::void_:
      return llvm::Type::getVoidTy(context);
    case PrimitiveType::f32:
    case PrimitiveType::f64:
      fail(location, "LLVM emission does not support floating-point values yet");
  }
}

llvm::Type* llvm_type_for_type(llvm::LLVMContext& context,
                               const llvm::Module& module,
                               const Type& type,
                               const TypeEnvironment& types,
                               SourceLocation location,
                               std::unordered_set<std::string>& resolving) {
  switch (type.kind) {
    case TypeKind::primitive:
      return primitive_llvm_type(context, module, type.primitive, location);

    case TypeKind::name: {
      const auto* declaration = types.find(type.name);
      if (declaration == nullptr) {
        fail(location, "unknown type name");
      }
      if (!declaration->parameters.empty()) {
        fail(location, "generic type requires type arguments");
      }
      if (!resolving.insert(type.name).second) {
        fail(location, "recursive type alias cannot be lowered to LLVM primitive type");
      }
      auto* llvm_type = llvm_type_for_type(context, module, *declaration->body, types, location, resolving);
      resolving.erase(type.name);
      return llvm_type;
    }

    case TypeKind::application: {
      const auto instantiated = instantiate_type_application(type, types);
      return llvm_type_for_type(context, module, *instantiated, types, location, resolving);
    }

    case TypeKind::pointer:
      return llvm::PointerType::get(context, 0);

    case TypeKind::array:
    case TypeKind::slice:
    case TypeKind::function:
    case TypeKind::product:
    case TypeKind::sum:
    case TypeKind::union_:
      fail(location, "LLVM emission currently supports only primitive types in function signatures");
  }
}

llvm::Type* llvm_type_for(llvm::LLVMContext& context,
                          const llvm::Module& module,
                          const Form& form,
                          const TypeEnvironment& types) {
  const auto type = parse_type(form);
  std::unordered_set<std::string> resolving;
  return llvm_type_for_type(context, module, *type, types, form.location, resolving);
}

bool is_stable_signature(const Form& form) {
  const auto* fields = as_list(form);
  if (fields == nullptr || fields->elements.empty()) {
    return false;
  }
  const auto* first_field = as_list(element(*fields, 0));
  if (first_field == nullptr || first_field->elements.empty()) {
    return false;
  }
  const auto* first_head = as_symbol(element(*first_field, 0));
  return first_head != nullptr &&
         (first_head->name == "type-params" || first_head->name == "params" || first_head->name == "return" ||
          first_head->name == "requires" || first_head->name == "documentation");
}

void parse_parameter_list(llvm::LLVMContext& context,
                          const llvm::Module& module,
                          const Form& form,
                          const TypeEnvironment& types,
                          std::vector<std::pair<std::string, llvm::Type*>>& parameters,
                          std::string_view diagnostic_prefix) {
  const auto* parameter_list = as_list(form);
  if (parameter_list == nullptr) {
    fail(form.location, std::string(diagnostic_prefix) + " parameters must be a list");
  }
  for (const auto& parameter_form : parameter_list->elements) {
    const auto* parameter = as_list(*parameter_form);
    if (parameter == nullptr || parameter->elements.size() != 2) {
      fail(parameter_form->location, std::string(diagnostic_prefix) + " parameter requires name and type");
    }
    parameters.push_back({
        symbol_name(element(*parameter, 0), std::string(diagnostic_prefix) + " parameter name must be a symbol"),
        llvm_type_for(context, module, element(*parameter, 1), types),
    });
  }
}

void parse_stable_signature(llvm::LLVMContext& context,
                            const llvm::Module& module,
                            const Form& form,
                            const TypeEnvironment& types,
                            FunctionSignature& signature,
                            std::string_view diagnostic_prefix) {
  const auto* fields = as_list(form);
  if (fields == nullptr) {
    fail(form.location, std::string(diagnostic_prefix) + " signature must be a list");
  }

  bool found_params = false;
  bool found_type_params = false;
  bool found_return = false;
  bool found_requires = false;
  for (const auto& field_form : fields->elements) {
    const auto* field = as_list(*field_form);
    if (field == nullptr || field->elements.empty()) {
      fail(field_form->location, std::string(diagnostic_prefix) + " signature field must be a list");
    }
    const auto head = symbol_name(element(*field, 0), std::string(diagnostic_prefix) + " signature field name must be a symbol");

    if (head == "type-params") {
      if (found_type_params || field->elements.size() != 2) {
        fail(field_form->location, std::string(diagnostic_prefix) + " type-params field expects one list");
      }
      const auto* type_parameters = as_list(element(*field, 1));
      if (type_parameters == nullptr) {
        fail(element(*field, 1).location, std::string(diagnostic_prefix) + " type-params field value must be a list");
      }
      if (!type_parameters->elements.empty()) {
        fail(field_form->location, "LLVM emission does not support generic function signatures yet");
      }
      found_type_params = true;
      continue;
    }

    if (head == "params") {
      if (found_params || field->elements.size() != 2) {
        fail(field_form->location, std::string(diagnostic_prefix) + " params field expects one list");
      }
      parse_parameter_list(context, module, element(*field, 1), types, signature.parameters, diagnostic_prefix);
      found_params = true;
      continue;
    }

    if (head == "return") {
      if (found_return || field->elements.size() != 2) {
        fail(field_form->location, std::string(diagnostic_prefix) + " return field expects one type");
      }
      signature.return_type = llvm_type_for(context, module, element(*field, 1), types);
      found_return = true;
      continue;
    }

    if (head == "requires") {
      if (found_requires || field->elements.size() != 2) {
        fail(field_form->location, std::string(diagnostic_prefix) + " requires field expects one list");
      }
      const auto* requirements = as_list(element(*field, 1));
      if (requirements == nullptr) {
        fail(element(*field, 1).location, std::string(diagnostic_prefix) + " requires field value must be a list");
      }
      if (!requirements->elements.empty()) {
        fail(field_form->location, "LLVM emission does not support type requirements yet");
      }
      found_requires = true;
      continue;
    }

    if (head == "documentation") {
      if (field->elements.size() != 2 || as_string(element(*field, 1)) == nullptr) {
        fail(field_form->location, std::string(diagnostic_prefix) + " documentation field expects one string");
      }
      continue;
    }

    fail(field_form->location, "unknown function signature field");
  }

  if (!found_params) {
    fail(form.location, std::string(diagnostic_prefix) + " signature requires params field");
  }
  if (!found_return) {
    fail(form.location, std::string(diagnostic_prefix) + " signature requires return field");
  }
  if (!found_requires) {
    fail(form.location, std::string(diagnostic_prefix) + " signature requires requires field");
  }
}

FunctionSignature parse_signature(llvm::LLVMContext& context,
                                  const llvm::Module& module,
                                  const Form& form,
                                  const TypeEnvironment& types) {
  const auto* list = as_list(form);
  if (list == nullptr || list->elements.size() < 4) {
    fail(form.location, "function declaration requires name, signature, and body");
  }
  if (symbol_name(element(*list, 0), "function head must be a symbol") != "fn") {
    fail(element(*list, 0).location, "expected function declaration");
  }

  FunctionSignature signature;
  signature.name = symbol_name(element(*list, 1), "function name must be a symbol");
  signature.link_name = signature.name;
  if (is_stable_signature(element(*list, 2))) {
    parse_stable_signature(context, module, element(*list, 2), types, signature, "function");
    signature.body_index = 3;
    return signature;
  }

  if (list->elements.size() < 5) {
    fail(form.location, "function declaration requires name, parameters, return type, and body");
  }

  signature.return_type = llvm_type_for(context, module, element(*list, 3), types);
  parse_parameter_list(context, module, element(*list, 2), types, signature.parameters, "function");

  return signature;
}

FunctionSignature parse_extern_signature(llvm::LLVMContext& context,
                                         const llvm::Module& module,
                                         const Form& form,
                                         const TypeEnvironment& types) {
  const auto* list = as_list(form);
  if (list == nullptr || list->elements.size() < 4 || list->elements.size() > 6) {
    fail(form.location, "extern function declaration expects 4, 5, or 6 forms");
  }
  if (symbol_name(element(*list, 0), "extern declaration head must be a symbol") != "extern") {
    fail(element(*list, 0).location, "expected extern declaration");
  }
  if (symbol_name(element(*list, 1), "extern declaration kind must be a symbol") != "fn") {
    fail(element(*list, 1).location, "extern declaration currently supports only fn");
  }

  FunctionSignature signature;
  signature.name = symbol_name(element(*list, 2), "extern function name must be a symbol");
  signature.link_name = signature.name;
  signature.external = true;
  if (is_stable_signature(element(*list, 3))) {
    parse_stable_signature(context, module, element(*list, 3), types, signature, "extern function");
    if (list->elements.size() == 5) {
      const auto* link_name = as_string(element(*list, 4));
      if (link_name == nullptr) {
        fail(element(*list, 4).location, "extern function link name must be a string");
      }
      signature.link_name = link_name->value;
    }
    if (list->elements.size() == 6) {
      fail(form.location, "extern function declaration expects 4 or 5 forms with stable signature");
    }
    return signature;
  }

  if (list->elements.size() != 5 && list->elements.size() != 6) {
    fail(form.location, "extern function declaration expects 5 or 6 forms");
  }
  signature.return_type = llvm_type_for(context, module, element(*list, 4), types);
  parse_parameter_list(context, module, element(*list, 3), types, signature.parameters, "extern function");

  if (list->elements.size() == 6) {
    const auto* link_name = as_string(element(*list, 5));
    if (link_name == nullptr) {
      fail(element(*list, 5).location, "extern function link name must be a string");
    }
    signature.link_name = link_name->value;
  }

  return signature;
}

llvm::Function* declare_function(llvm::Module& module, const FunctionSignature& signature) {
  if (auto* function = module.getFunction(signature.link_name)) {
    return function;
  }

  std::vector<llvm::Type*> parameter_types;
  for (const auto& parameter : signature.parameters) {
    parameter_types.push_back(parameter.second);
  }
  auto* function_type = llvm::FunctionType::get(signature.return_type, parameter_types, false);
  auto* function = llvm::Function::Create(function_type,
                                          llvm::Function::ExternalLinkage,
                                          signature.link_name,
                                          module);
  if (!signature.external && signature.link_name != signature.name) {
    function->setName(signature.name);
  }
  return function;
}

class FunctionEmitter {
 public:
  FunctionEmitter(llvm::LLVMContext& context,
                  llvm::Module& module,
                  llvm::IRBuilder<>& builder,
                  const std::unordered_map<std::string, FunctionSignature>& functions,
                  FunctionSignature signature)
      : context_(context),
        module_(module),
        builder_(builder),
        functions_(functions),
        signature_(std::move(signature)) {}

  void emit(const Form& declaration) {
    const auto* list = as_list(declaration);
    auto* function = declare_function(module_, signature_);
    auto parameter = function->arg_begin();
    for (const auto& signature_parameter : signature_.parameters) {
      parameter->setName(signature_parameter.first);
      variables_.emplace(signature_parameter.first, Value{signature_parameter.second, &*parameter});
      ++parameter;
    }

    auto* entry = llvm::BasicBlock::Create(context_, "entry", function);
    builder_.SetInsertPoint(entry);

    Value result{llvm::Type::getVoidTy(context_), nullptr};
    for (std::size_t index = signature_.body_index; index < list->elements.size(); ++index) {
      result = emit_expression(element(*list, index));
    }

    if (signature_.return_type->isVoidTy()) {
      builder_.CreateRetVoid();
    } else {
      require_type(result, signature_.return_type, declaration.location);
      builder_.CreateRet(result.value);
    }
  }

 private:
  Value emit_expression(const Form& form) {
    if (const auto* integer = as_integer(form)) {
      auto* type = llvm::Type::getInt64Ty(context_);
      return Value{type, llvm::ConstantInt::get(type, integer->value, true)};
    }
    if (const auto* string = as_string(form)) {
      auto* value = builder_.CreateGlobalString(string->value);
      return Value{value->getType(), value};
    }
    if (is_unit(form)) {
      return Value{llvm::Type::getVoidTy(context_), nullptr};
    }
    if (const auto* symbol = as_symbol(form)) {
      if (symbol->name == "true") {
        return Value{llvm::Type::getInt1Ty(context_), llvm::ConstantInt::getTrue(context_)};
      }
      if (symbol->name == "false") {
        return Value{llvm::Type::getInt1Ty(context_), llvm::ConstantInt::getFalse(context_)};
      }
      const auto found = variables_.find(symbol->name);
      if (found == variables_.end()) {
        fail(form.location, "unknown local or parameter");
      }
      return found->second;
    }

    const auto* list = as_list(form);
    if (list == nullptr || list->elements.empty()) {
      fail(form.location, "expected expression");
    }

    const auto head = symbol_name(element(*list, 0), "expression head must be a symbol");
    if (head == "let") {
      return emit_let(form, *list);
    }
    if (head == "do") {
      return emit_do(form, *list);
    }
    if (head == "match") {
      return emit_match(form, *list);
    }
    if (head == "+" || head == "-" || head == "*" || head == "/") {
      return emit_arithmetic(form, *list, head);
    }
    if (head == "=" || head == "!=" || head == "<" || head == "<=" || head == ">" || head == ">=") {
      return emit_comparison(form, *list, head);
    }
    return emit_call(form, *list, head);
  }

  Value emit_let(const Form& form, const List& list) {
    if (list.elements.size() < 3) {
      fail(form.location, "let expression requires bindings and a body");
    }
    const auto* bindings = as_list(element(list, 1));
    if (bindings == nullptr) {
      fail(element(list, 1).location, "let bindings must be a list");
    }

    auto previous = variables_;
    for (const auto& binding_form : bindings->elements) {
      const auto* binding = as_list(*binding_form);
      if (binding == nullptr || binding->elements.size() != 2) {
        fail(binding_form->location, "let binding requires name and value");
      }
      const auto name = symbol_name(element(*binding, 0), "let binding name must be a symbol");
      variables_[name] = emit_expression(element(*binding, 1));
    }

    Value result{llvm::Type::getVoidTy(context_), nullptr};
    for (std::size_t index = 2; index < list.elements.size(); ++index) {
      result = emit_expression(element(list, index));
    }
    variables_ = std::move(previous);
    return result;
  }

  Value emit_do(const Form& form, const List& list) {
    if (list.elements.size() < 2) {
      fail(form.location, "do expression requires at least one body form");
    }
    Value result{llvm::Type::getVoidTy(context_), nullptr};
    for (std::size_t index = 1; index < list.elements.size(); ++index) {
      result = emit_expression(element(list, index));
    }
    return result;
  }

  Value emit_match(const Form& form, const List& list) {
    if (list.elements.size() < 3) {
      fail(form.location, "match expression requires a value and at least one arm");
    }

    const auto scrutinee = emit_expression(element(list, 1));
    auto* function = builder_.GetInsertBlock()->getParent();
    auto* done_block = llvm::BasicBlock::Create(context_, "match.end", function);
    auto* next_test_block = llvm::BasicBlock::Create(context_, "match.test", function);
    builder_.CreateBr(next_test_block);

    bool bool_has_true = false;
    bool bool_has_false = false;
    if (scrutinee.type->isIntegerTy(1)) {
      for (std::size_t index = 2; index < list.elements.size(); ++index) {
        const auto* arm = as_list(element(list, index));
        if (arm != nullptr && arm->elements.size() == 2) {
          const auto value = bool_pattern_value(element(*arm, 0));
          if (value == true) {
            bool_has_true = true;
          } else if (value == false) {
            bool_has_false = true;
          }
        }
      }
    }
    const bool bool_exhaustive = scrutinee.type->isIntegerTy(1) && bool_has_true && bool_has_false;

    std::vector<ArmResult> arm_results;
    llvm::Type* result_type = nullptr;

    for (std::size_t index = 2; index < list.elements.size(); ++index) {
      const auto* arm = as_list(element(list, index));
      if (arm == nullptr || arm->elements.size() != 2) {
        fail(element(list, index).location, "match arm requires a pattern and expression");
      }

      auto* body_block = llvm::BasicBlock::Create(context_, "match.arm", function);
      const auto has_next_arm = index + 1 < list.elements.size();
      auto* following_test_block = has_next_arm
                                       ? llvm::BasicBlock::Create(context_, "match.test", function)
                                       : nullptr;
      auto* final_exhaustive_block = !has_next_arm && bool_exhaustive ? body_block : following_test_block;

      builder_.SetInsertPoint(next_test_block);
      const auto binding = emit_pattern_test(scrutinee, element(*arm, 0), body_block, final_exhaustive_block);

      builder_.SetInsertPoint(body_block);
      auto previous = variables_;
      if (binding.has_value()) {
        variables_[*binding] = scrutinee;
      }
      const auto arm_value = emit_expression(element(*arm, 1));
      variables_ = std::move(previous);

      if (result_type == nullptr) {
        result_type = arm_value.type;
      } else {
        require_type(arm_value, result_type, element(*arm, 1).location);
      }

      auto* arm_block = builder_.GetInsertBlock();
      builder_.CreateBr(done_block);
      arm_results.push_back(ArmResult{arm_value, arm_block});

      next_test_block = following_test_block;
    }

    if (next_test_block != nullptr) {
      builder_.SetInsertPoint(next_test_block);
      fail(form.location, "match expression must end with a catch-all arm");
    }

    builder_.SetInsertPoint(done_block);
    if (result_type == nullptr || result_type->isVoidTy()) {
      return Value{llvm::Type::getVoidTy(context_), nullptr};
    }

    auto* phi = builder_.CreatePHI(result_type, static_cast<unsigned>(arm_results.size()));
    for (const auto& result : arm_results) {
      phi->addIncoming(result.value.value, result.block);
    }
    return Value{result_type, phi};
  }

  std::optional<std::string> emit_pattern_test(const Value& scrutinee,
                                               const Form& pattern,
                                               llvm::BasicBlock* body_block,
                                               llvm::BasicBlock* next_block) {
    if (const auto* symbol = as_symbol(pattern)) {
      if (symbol->name == "_") {
        builder_.CreateBr(body_block);
        return std::nullopt;
      }
      if (symbol->name == "true" || symbol->name == "false") {
        require_type(scrutinee, llvm::Type::getInt1Ty(context_), pattern.location);
        auto* expected = symbol->name == "true" ? llvm::ConstantInt::getTrue(context_)
                                                : llvm::ConstantInt::getFalse(context_);
        auto* condition = builder_.CreateICmpEQ(scrutinee.value, expected);
        emit_pattern_branch(pattern.location, condition, body_block, next_block);
        return std::nullopt;
      }

      builder_.CreateBr(body_block);
      return symbol->name;
    }

    if (const auto* integer = as_integer(pattern)) {
      auto* type = llvm::Type::getInt64Ty(context_);
      require_type(scrutinee, type, pattern.location);
      auto* expected = llvm::ConstantInt::get(type, integer->value, true);
      auto* condition = builder_.CreateICmpEQ(scrutinee.value, expected);
      emit_pattern_branch(pattern.location, condition, body_block, next_block);
      return std::nullopt;
    }

    if (is_unit(pattern)) {
      require_type(scrutinee, llvm::Type::getVoidTy(context_), pattern.location);
      builder_.CreateBr(body_block);
      return std::nullopt;
    }

    fail(pattern.location, "unsupported match pattern");
  }

  void emit_pattern_branch(SourceLocation location,
                           llvm::Value* condition,
                           llvm::BasicBlock* body_block,
                           llvm::BasicBlock* next_block) {
    if (next_block == nullptr) {
      fail(location, "match expression must end with a catch-all arm");
    }
    builder_.CreateCondBr(condition, body_block, next_block);
  }

  Value emit_arithmetic(const Form& form, const List& list, std::string_view op) {
    if (list.elements.size() != 3) {
      fail(form.location, "arithmetic expression requires two operands");
    }
    const auto left = emit_expression(element(list, 1));
    const auto right = emit_expression(element(list, 2));
    require_type(right, left.type, element(list, 2).location);
    if (!left.type->isIntegerTy(64) && !left.type->isIntegerTy(32)) {
      fail(form.location, "arithmetic currently supports i32 and i64 values");
    }

    llvm::Value* result = nullptr;
    if (op == "+") {
      result = builder_.CreateAdd(left.value, right.value);
    } else if (op == "-") {
      result = builder_.CreateSub(left.value, right.value);
    } else if (op == "*") {
      result = builder_.CreateMul(left.value, right.value);
    } else {
      result = builder_.CreateSDiv(left.value, right.value);
    }
    return Value{left.type, result};
  }

  Value emit_comparison(const Form& form, const List& list, std::string_view op) {
    if (list.elements.size() != 3) {
      fail(form.location, "comparison expression requires two operands");
    }
    const auto left = emit_expression(element(list, 1));
    const auto right = emit_expression(element(list, 2));
    require_type(right, left.type, element(list, 2).location);

    llvm::CmpInst::Predicate predicate;
    if (op == "=") {
      predicate = llvm::CmpInst::ICMP_EQ;
    } else if (op == "!=") {
      predicate = llvm::CmpInst::ICMP_NE;
    } else if (op == "<") {
      predicate = llvm::CmpInst::ICMP_SLT;
    } else if (op == "<=") {
      predicate = llvm::CmpInst::ICMP_SLE;
    } else if (op == ">") {
      predicate = llvm::CmpInst::ICMP_SGT;
    } else {
      predicate = llvm::CmpInst::ICMP_SGE;
    }
    return Value{llvm::Type::getInt1Ty(context_), builder_.CreateICmp(predicate, left.value, right.value)};
  }

  Value emit_call(const Form& form, const List& list, const std::string& name) {
    const auto found = functions_.find(name);
    if (found == functions_.end()) {
      fail(form.location, "unknown function");
    }
    const auto& signature = found->second;
    if (list.elements.size() - 1 != signature.parameters.size()) {
      fail(form.location, "function call argument count mismatch");
    }

    std::vector<llvm::Value*> arguments;
    for (std::size_t index = 1; index < list.elements.size(); ++index) {
      auto argument = emit_expression(element(list, index));
      require_type(argument, signature.parameters[index - 1].second, element(list, index).location);
      arguments.push_back(argument.value);
    }

    auto* function = module_.getFunction(name);
    if (function == nullptr) {
      function = module_.getFunction(signature.link_name);
    }
    if (function == nullptr) {
      fail(form.location, "unknown function");
    }
    auto* call = builder_.CreateCall(function, arguments);
    return Value{signature.return_type, signature.return_type->isVoidTy() ? nullptr : call};
  }

  void require_type(const Value& value, llvm::Type* expected, SourceLocation location) const {
    if (value.type != expected) {
      fail(location, "expression type mismatch");
    }
  }

  llvm::LLVMContext& context_;
  llvm::Module& module_;
  llvm::IRBuilder<>& builder_;
  const std::unordered_map<std::string, FunctionSignature>& functions_;
  FunctionSignature signature_;
  std::unordered_map<std::string, Value> variables_;
};

}  // namespace

CodegenError::CodegenError(Diagnostic diagnostic)
    : std::runtime_error(diagnostic.message), diagnostic_(std::move(diagnostic)) {}

const Diagnostic& CodegenError::diagnostic() const {
  return diagnostic_;
}

namespace {

llvm::TargetMachine& native_target_machine() {
  static const bool initialized = [] {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    return true;
  }();
  (void)initialized;

  llvm::Triple triple(llvm::sys::getDefaultTargetTriple());
#ifdef TERMIS_MACOS_DEPLOYMENT_TARGET
  if (triple.isMacOSX()) {
    triple.setOSName("macosx" TERMIS_MACOS_DEPLOYMENT_TARGET);
  }
#endif
  const auto target_triple = triple.str();
  std::string target_error;
  const auto* target = llvm::TargetRegistry::lookupTarget(target_triple, target_error);
  if (target == nullptr) {
    fail(SourceLocation{}, "unable to find LLVM target: " + target_error);
  }

  llvm::TargetOptions options;
  static std::unique_ptr<llvm::TargetMachine> target_machine{
      target->createTargetMachine(triple, "generic", "", options, std::nullopt)};
  if (target_machine == nullptr) {
    fail(SourceLocation{}, "unable to create LLVM target machine");
  }
  return *target_machine;
}

std::unique_ptr<llvm::Module> emit_module(llvm::LLVMContext& context,
                                          const Program& program,
                                          llvm::TargetMachine* target_machine = nullptr) {
  auto module = std::make_unique<llvm::Module>("termis", context);
  if (target_machine != nullptr) {
    module->setTargetTriple(target_machine->getTargetTriple());
    module->setDataLayout(target_machine->createDataLayout());
  }
  llvm::IRBuilder<> builder(context);

  std::unordered_map<std::string, FunctionSignature> functions;
  for (const auto& node : program.forms) {
    switch (node->kind) {
      case SemanticKind::function_declaration: {
        auto signature = parse_signature(context, *module, *node->form, program.types);
        if (functions.contains(signature.name)) {
          fail(node->form->location, "function redefines existing function");
        }
        functions.emplace(signature.name, std::move(signature));
        break;
      }

      case SemanticKind::extern_function_declaration: {
        auto signature = parse_extern_signature(context, *module, *node->form, program.types);
        if (functions.contains(signature.name)) {
          fail(node->form->location, "function redefines existing function");
        }
        functions.emplace(signature.name, std::move(signature));
        break;
      }

      case SemanticKind::type_declaration:
      case SemanticKind::provide_declaration:
      case SemanticKind::class_declaration:
      case SemanticKind::implements_declaration:
      case SemanticKind::let_expression:
      case SemanticKind::do_expression:
      case SemanticKind::match_expression:
      case SemanticKind::const_declaration:
      case SemanticKind::application:
      case SemanticKind::list_expression:
      case SemanticKind::atom:
        break;
    }
  }

  for (const auto& [_, signature] : functions) {
    declare_function(*module, signature);
  }

  for (const auto& node : program.forms) {
    switch (node->kind) {
      case SemanticKind::function_declaration: {
        const auto signature = parse_signature(context, *module, *node->form, program.types);
        FunctionEmitter emitter(context, *module, builder, functions, functions.at(signature.name));
        emitter.emit(*node->form);
        break;
      }

      case SemanticKind::extern_function_declaration:
      case SemanticKind::type_declaration:
      case SemanticKind::provide_declaration:
      case SemanticKind::class_declaration:
      case SemanticKind::implements_declaration:
        break;

      case SemanticKind::let_expression:
      case SemanticKind::do_expression:
      case SemanticKind::match_expression:
      case SemanticKind::const_declaration:
      case SemanticKind::application:
      case SemanticKind::list_expression:
      case SemanticKind::atom:
        fail(node->form->location, "LLVM emission currently supports only type, extern, and function declarations");
    }
  }

  std::string verifier_errors;
  llvm::raw_string_ostream verifier_stream(verifier_errors);
  if (llvm::verifyModule(*module, &verifier_stream)) {
    verifier_stream.flush();
    fail(SourceLocation{}, "generated invalid LLVM IR: " + verifier_errors);
  }

  return module;
}

}  // namespace

std::string emit_llvm_ir(const Program& program) {
  llvm::LLVMContext context;
  const auto module = emit_module(context, program);

  std::string ir;
  llvm::raw_string_ostream stream(ir);
  module->print(stream, nullptr);
  stream.flush();
  return ir;
}

std::string emit_object_file(const Program& program) {
  llvm::LLVMContext context;
  auto& target_machine = native_target_machine();
  auto module = emit_module(context, program, &target_machine);

  llvm::SmallVector<char, 0> object_buffer;
  llvm::raw_svector_ostream stream(object_buffer);
  llvm::legacy::PassManager pass_manager;
  if (target_machine.addPassesToEmitFile(pass_manager, stream, nullptr, llvm::CodeGenFileType::ObjectFile)) {
    fail(SourceLocation{}, "LLVM target does not support object emission");
  }
  pass_manager.run(*module);

  return std::string(object_buffer.begin(), object_buffer.end());
}

}  // namespace termis
