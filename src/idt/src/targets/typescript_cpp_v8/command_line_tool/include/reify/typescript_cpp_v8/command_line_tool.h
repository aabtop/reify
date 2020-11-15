#ifndef _REIFY_TYPESCRIPT_CPP_V8_COMMAND_LINE_TOOL_H_
#define _REIFY_TYPESCRIPT_CPP_V8_COMMAND_LINE_TOOL_H_

#include <chrono>
#include <functional>
#include <string>
#include <variant>

#include "CLI/CLI.hpp"
#include "reify/typescript_cpp_v8.h"

namespace reify {
namespace typescript_cpp_v8 {

struct CommandLineToolParameters {
  std::string app_name;
  std::string app_description;
  // Defines which type declarations are visible to user TypeScript code.  This
  // is usually taken straight from the IDT generated output to ensure that the
  // TypeScript declarations match up with the CPP declarations.
  std::vector<reify::CompilerEnvironment::InputModule> typescript_input_modules;
  std::function<void(CLI::App*)> add_options_hook;
};

struct CommandLineToolParseResult {
  std::optional<reify::MountedHostFolderFilesystem> virtual_filesystem;
  std::optional<reify::CompilerEnvironment> compile_env;
  std::shared_ptr<reify::CompiledModule> compiled_module;
  std::optional<reify::RuntimeEnvironment> runtime_env;
  const reify::CompiledModule::ExportedSymbol* entry_point_symbol;

  // How long it took to compile the user TypeScript.
  std::chrono::microseconds compile_time;
};

std::variant<int, std::unique_ptr<CommandLineToolParseResult>>
CommandLineToolParse(const CommandLineToolParameters& params, int argc,
                     char* argv[]);

}  // namespace typescript_cpp_v8
}  // namespace reify

#endif  // _REIFY_TYPESCRIPT_CPP_V8_COMMAND_LINE_TOOL_H_
