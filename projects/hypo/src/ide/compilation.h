#ifndef _IDE_COMPILATION_H_
#define _IDE_COMPILATION_H_

#include <filesystem>
#include <map>
#include <optional>
#include <variant>

#include "reify/typescript_cpp_v8.h"
#include "src/ide/thread_checker.h"

using CompileError = std::string;
using CompileResult =
    std::variant<CompileError, std::shared_ptr<reify::CompiledModule>>;
CompileResult CompileFile(
    const std::vector<reify::CompilerEnvironment::InputModule>&
        typescript_input_modules,
    const std::filesystem::path& filepath);
CompileResult CompileContents(
    const std::vector<reify::CompilerEnvironment::InputModule>&
        typescript_input_modules,
    const std::string& contents);

#endif  // _IDE_COMPILATION_H_
