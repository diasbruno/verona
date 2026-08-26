#include "codegen.hpp"
#include "layout.hpp"
#include "reader.hpp"
#include "semantic.hpp"
#include "type.hpp"

#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <optional>
#include <sstream>
#include <string>
#include <string_view>
#include <system_error>
#include <vector>

namespace {

constexpr std::string_view version = "0.1.0";

void print_help(std::ostream& out) {
  out << "Usage: termisc [OPTIONS] <input.termis>\n"
      << "\n"
      << "Options:\n"
      << "  -h, --help           Show this help message\n"
      << "  -I, --module-path <path>\n"
      << "                       Load every .termis file from path before input\n"
      << "  -o, --output <path>  Write the compiled binary to path\n"
      << "  --dump-llvm          Emit LLVM IR to stdout after validation\n"
      << "  --version            Show compiler version\n";
}

void print_version(std::ostream& out) {
  out << "termisc " << version << '\n';
}

std::string shell_quote(std::string_view value) {
  std::string quoted = "'";
  for (const char character : value) {
    if (character == '\'') {
      quoted += "'\\''";
    } else {
      quoted += character;
    }
  }
  quoted += "'";
  return quoted;
}

std::filesystem::path make_temporary_object_path() {
  const auto now = std::chrono::steady_clock::now().time_since_epoch().count();
  const auto directory = std::filesystem::temp_directory_path();
  for (int attempt = 0; attempt < 100; ++attempt) {
    auto path = directory / ("termis-" + std::to_string(now) + "-" +
                             std::to_string(attempt) + ".o");
    if (!std::filesystem::exists(path)) {
      return path;
    }
  }
  return directory / ("termis-" + std::to_string(now) + ".o");
}

bool link_object_to_binary(std::string_view object,
                           const std::filesystem::path& output_path,
                           std::ostream& err) {
  const auto object_path = make_temporary_object_path();
  {
    std::ofstream object_file{object_path, std::ios::binary};
    if (!object_file) {
      err << "termisc: unable to write temporary object file: " << object_path << '\n';
      return false;
    }
    object_file.write(object.data(), static_cast<std::streamsize>(object.size()));
  }

  const auto command = "clang++ " + shell_quote(object_path.string()) + " -o " +
                       shell_quote(output_path.string());
  const int status = std::system(command.c_str());

  std::error_code remove_error;
  std::filesystem::remove(object_path, remove_error);

  if (status != 0) {
    err << "termisc: linker failed while producing " << output_path << '\n';
    return false;
  }
  return true;
}

std::optional<std::string> read_file(const std::filesystem::path& path) {
  std::ifstream input{path};
  if (!input) {
    return std::nullopt;
  }

  std::ostringstream buffer;
  buffer << input.rdbuf();
  return buffer.str();
}

const termis::Symbol* as_symbol(const termis::Form& form) {
  return std::get_if<termis::Symbol>(&form.kind);
}

const termis::List* as_list(const termis::Form& form) {
  return std::get_if<termis::List>(&form.kind);
}

std::optional<std::string> directive_module_name(const termis::Form& form,
                                                 std::string_view directive) {
  const auto* list = as_list(form);
  if (list == nullptr || list->elements.size() < 2) {
    return std::nullopt;
  }
  const auto* head = as_symbol(*list->elements[0]);
  if (head == nullptr || head->name != directive) {
    return std::nullopt;
  }
  const auto* name = as_symbol(*list->elements[1]);
  if (name == nullptr) {
    return std::nullopt;
  }
  return name->name;
}

bool append_module_forms(std::vector<termis::FormPtr>& destination,
                         std::vector<termis::FormPtr> source,
                         std::ostream& err) {
  for (auto& form : source) {
    const auto* list = as_list(*form);
    if (auto imported = directive_module_name(*form, "import")) {
      if (list->elements.size() != 2) {
        err << "termisc: import expects exactly one module name\n";
        return false;
      }
      continue;
    }
    if (auto module = directive_module_name(*form, "module")) {
      auto* module_list = std::get_if<termis::List>(&form->kind);
      if (module_list == nullptr || module_list->elements.size() < 2) {
        err << "termisc: module expects a module name\n";
        return false;
      }
      std::vector<termis::FormPtr> body;
      body.reserve(module_list->elements.size() - 2);
      for (std::size_t index = 2; index < module_list->elements.size(); ++index) {
        body.push_back(std::move(module_list->elements[index]));
      }
      if (!append_module_forms(destination, std::move(body), err)) {
        return false;
      }
      continue;
    }
    destination.push_back(std::move(form));
  }
  return true;
}

bool append_source_file(std::vector<termis::FormPtr>& destination,
                        const std::filesystem::path& path,
                        std::ostream& err) {
  const auto source = read_file(path);
  if (!source.has_value()) {
    err << "termisc: unable to open module path file: " << path << '\n';
    return false;
  }
  return append_module_forms(destination, termis::read_forms(*source), err);
}

bool append_module_path(std::vector<termis::FormPtr>& destination,
                        const std::filesystem::path& path,
                        std::ostream& err) {
  std::error_code status_error;
  const auto status = std::filesystem::status(path, status_error);
  if (status_error || !std::filesystem::exists(status)) {
    err << "termisc: module path does not exist: " << path << '\n';
    return false;
  }

  if (std::filesystem::is_regular_file(status)) {
    return append_source_file(destination, path, err);
  }
  if (!std::filesystem::is_directory(status)) {
    err << "termisc: module path is not a file or directory: " << path << '\n';
    return false;
  }

  std::vector<std::filesystem::path> files;
  for (std::filesystem::recursive_directory_iterator iterator(path, status_error), end;
       !status_error && iterator != end;
       iterator.increment(status_error)) {
    const auto& entry = *iterator;
    if (entry.is_regular_file(status_error) && entry.path().extension() == ".termis") {
      files.push_back(entry.path());
    }
  }
  if (status_error) {
    err << "termisc: unable to read module path: " << path << '\n';
    return false;
  }

  std::sort(files.begin(), files.end());
  for (const auto& file : files) {
    if (!append_source_file(destination, file, err)) {
      return false;
    }
  }
  return true;
}

}  // namespace

int main(int argc, char** argv) {
  if (argc == 1) {
    print_help(std::cerr);
    return EXIT_FAILURE;
  }

  std::string_view input_path;
  std::optional<std::filesystem::path> output_path;
  std::vector<std::filesystem::path> module_paths;
  bool dump_llvm = false;

  for (int i = 1; i < argc; ++i) {
    const std::string_view arg(argv[i]);

    if (arg == "-h" || arg == "--help") {
      print_help(std::cout);
      return EXIT_SUCCESS;
    }

    if (arg == "--version") {
      print_version(std::cout);
      return EXIT_SUCCESS;
    }

    if (arg == "--dump-llvm") {
      dump_llvm = true;
      continue;
    }

    if (arg == "-I" || arg == "--module-path") {
      if (i + 1 >= argc) {
        std::cerr << "termisc: " << arg << " requires a path\n";
        return EXIT_FAILURE;
      }
      module_paths.emplace_back(argv[++i]);
      continue;
    }

    if (arg == "-o" || arg == "--output") {
      if (i + 1 >= argc) {
        std::cerr << "termisc: " << arg << " requires a path\n";
        return EXIT_FAILURE;
      }
      output_path = argv[++i];
      continue;
    }

    if (!arg.empty() && arg.front() == '-') {
      std::cerr << "termisc: unknown option: " << arg << '\n';
      return EXIT_FAILURE;
    }

    if (!input_path.empty()) {
      std::cerr << "termisc: expected one input file\n";
      return EXIT_FAILURE;
    }

    input_path = arg;
  }

  if (input_path.empty()) {
    std::cerr << "termisc: expected an input file\n";
    return EXIT_FAILURE;
  }

  const auto input_source = read_file(std::string(input_path));
  if (!input_source.has_value()) {
    std::cerr << "termisc: unable to open input file: " << input_path << '\n';
    return EXIT_FAILURE;
  }

  try {
    std::vector<termis::FormPtr> forms;
    for (const auto& module_path : module_paths) {
      if (!append_module_path(forms, module_path, std::cerr)) {
        return EXIT_FAILURE;
      }
    }
    if (!append_module_forms(forms, termis::read_forms(*input_source), std::cerr)) {
      return EXIT_FAILURE;
    }

    const auto program = termis::analyze_forms(forms);
    termis::LayoutEngine layout_engine(program.types);
    std::size_t concrete_layouts = 0;
    for (const auto& declaration : program.types.declarations()) {
      if (declaration.parameters.empty()) {
        (void)layout_engine.compute(*declaration.body);
        ++concrete_layouts;
      }
    }

    if (dump_llvm) {
      std::cout << termis::emit_llvm_ir(program);
    } else {
      (void)forms;
      (void)concrete_layouts;
      const auto object = termis::emit_object_file(program);
      const auto binary_path = output_path.value_or("a.out");
      if (!link_object_to_binary(object, binary_path, std::cerr)) {
        return EXIT_FAILURE;
      }
      std::cout << "wrote " << binary_path << '\n';
    }
  } catch (const termis::ReadError& error) {
    const auto& diagnostic = error.diagnostic();
    std::cerr << input_path << ':' << diagnostic.location.line << ':'
              << diagnostic.location.column << ": reader error: " << diagnostic.message << '\n';
    return EXIT_FAILURE;
  } catch (const termis::SemanticError& error) {
    const auto& diagnostic = error.diagnostic();
    std::cerr << input_path << ':' << diagnostic.location.line << ':'
              << diagnostic.location.column << ": semantic error: " << diagnostic.message << '\n';
    return EXIT_FAILURE;
  } catch (const termis::TypeError& error) {
    const auto& diagnostic = error.diagnostic();
    std::cerr << input_path << ':' << diagnostic.location.line << ':'
              << diagnostic.location.column << ": type error: " << diagnostic.message << '\n';
    return EXIT_FAILURE;
  } catch (const termis::LayoutError& error) {
    const auto& diagnostic = error.diagnostic();
    std::cerr << input_path << ':' << diagnostic.location.line << ':'
              << diagnostic.location.column << ": layout error: " << diagnostic.message << '\n';
    return EXIT_FAILURE;
  } catch (const termis::CodegenError& error) {
    const auto& diagnostic = error.diagnostic();
    std::cerr << input_path << ':' << diagnostic.location.line << ':'
              << diagnostic.location.column << ": codegen error: " << diagnostic.message << '\n';
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
