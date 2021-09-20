import lib_es2015_collection from 'raw-loader!typescript/lib/lib.es2015.collection.d.ts'
import lib_es2015_core from 'raw-loader!typescript/lib/lib.es2015.core.d.ts'
import lib_es2015_generator from 'raw-loader!typescript/lib/lib.es2015.generator.d.ts'
import lib_es2015_iterable from 'raw-loader!typescript/lib/lib.es2015.iterable.d.ts'
import lib_es2015_symbol from 'raw-loader!typescript/lib/lib.es2015.symbol.d.ts'
import lib_es5 from 'raw-loader!typescript/lib/lib.es5.d.ts'
import lib_es2019_array from 'raw-loader!typescript/lib/lib.es2019.array.d.ts'
import lib_es2019_object from 'raw-loader!typescript/lib/lib.es2019.object.d.ts'
import lib_es2019_string from 'raw-loader!typescript/lib/lib.es2019.string.d.ts'
import lib_es2019_symbol from 'raw-loader!typescript/lib/lib.es2019.symbol.d.ts'
import * as ts from 'typescript';

// Injected by the embedder.
declare function externalGetSourceFile(path: string): string;
declare function externalFileExists(path: string): boolean;

function getSystemFileContent(
  sourceMap: { [key: string]: ts.SourceFile }, path: string): ts.SourceFile |
  null {
  const trimmedPath = path.trim();
  if (trimmedPath in sourceMap) {
    return sourceMap[trimmedPath];
  } else {
    return null;
  }
}

const LIB_MODULES = [
  ['/lib.es5.d.ts', lib_es5],
  ['/lib.es2015.core.d.ts', lib_es2015_core],
  ['/lib.es2015.symbol.d.ts', lib_es2015_symbol],
  ['/lib.es2015.iterable.d.ts', lib_es2015_iterable],
  ['/lib.es2015.generator.d.ts', lib_es2015_generator],
  ['/lib.es2019.array.d.ts', lib_es2019_array],
  ['/lib.es2019.object.d.ts', lib_es2019_object],
  ['/lib.es2019.string.d.ts', lib_es2019_string],
  ['/lib.es2019.symbol.d.ts', lib_es2019_symbol],
];

const LIB_SOURCE_FILES = LIB_MODULES.map(x => {
  return ts.createSourceFile(x[0], x[1], ts.ScriptTarget.Latest);
});

// We don't make use of the default lib and instead just set it to blank.
const defaultLibFilename = '/lib.d.ts';
const defaultLibSourceFile =
  ts.createSourceFile(defaultLibFilename, '', ts.ScriptTarget.Latest);

export type ModuleExport = {
  symbol_name: string; type_string: string;
  location: { path: string; line: number; column: number; };
};

export type TranspilationOutput = {
  // Mapping from a JS file path to file contents.
  js_modules: { [key: string]: string };

  // Mapping from declarations
  declaration_files: { [key: string]: string };

  // The file path of the primary module, e.g. the transpiled input source
  // module.  This can be used to index into |js_modules| above.
  primary_module: string;

  module_exports: ModuleExport[];
};

export type TranspileError = {
  path: string; line: number; column: number; message: string;
};

export type TranspileResults = {
  // True if there were no errors.
  success: boolean;
  // Contains the error message if |success| is false, otherwise it is invalid.
  error?: TranspileError;
  // Contains the transpilation results if |success| is true, otherwise it is
  // invalid.
  output?: TranspilationOutput;
};

function GetDiagnosticsMessage(diagnostic: string | ts.DiagnosticMessageChain |
  undefined): string {
  if (typeof diagnostic === 'string') {
    return diagnostic;
  } else if (diagnostic === undefined) {
    return ''
  } else {
    return diagnostic.messageText;
  }
}

function GetDiagnosticsError(diagnostic: ts.Diagnostic): TranspileError {
  let transpile_error: TranspileError = {
    path: '',
    line: 0,
    column: 0,
    message: GetDiagnosticsMessage(diagnostic.messageText),
  };
  if (diagnostic.file) {
    let { line, character } =
      diagnostic.file.getLineAndCharacterOfPosition(diagnostic.start!);
    transpile_error.line = line;
    transpile_error.column = character;
    transpile_error.path = diagnostic.file.fileName;
  }
  return transpile_error;
}

export function TranspileModule(
  path: string, text: string, systemModules: { [key: string]: string; },
  generate_declarations: boolean): TranspileResults {

  if (!path.includes('.')) {
    return {
      success: false,
      error: { message: 'Path must have a `.ts` extension.', line: 0, column: 0, path: path },
    };
  }

  let results: TranspilationOutput = {
    js_modules: {},
    declaration_files: {},
    primary_module: path.split('.').slice(0, -1).join('.') + '.js',
    module_exports: [],
  };

  let input_source = ts.createSourceFile(path, text, ts.ScriptTarget.Latest);
  let sourceMap: { [key: string]: ts.SourceFile } = {
    [defaultLibFilename]: defaultLibSourceFile,
  };
  for (var i = 0; i < LIB_MODULES.length; ++i) {
    sourceMap[LIB_MODULES[i][0]] = LIB_SOURCE_FILES[i];
  }
  for (var key in systemModules) {
    sourceMap[key] =
      ts.createSourceFile(key, systemModules[key], ts.ScriptTarget.Latest);
  }

  const DECLARATIONS_DIRECTORY = 'declarations';

  let options: ts.CompilerOptions = {
    module: ts.ModuleKind.ES2015,
    target: ts.ScriptTarget.ES2015,
    lib: LIB_MODULES.map(x => x[0]),
    strict: true,
    noErrorTruncation: true,
    // This makes it so that TSC gives a reasonable error when the user tries
    // to compile an empty file.  Without this, TSC would throw an exception
    // instead.
    isolatedModules: true,
    baseUrl: '/',
  };
  if (generate_declarations) {
    options.declaration = generate_declarations;
    options.declarationDir = DECLARATIONS_DIRECTORY;
  }

  const host: ts.CompilerHost = {
    fileExists: filePath => {
      return (getSystemFileContent(sourceMap, filePath) != null) ||
        filePath.trim() === path.trim() ||
        externalFileExists(filePath.trim());
    },
    directoryExists: dirPath => true,
    getCurrentDirectory: () => '/',
    getDirectories: path => {
      return [];
    },
    getCanonicalFileName: fileName => fileName,
    getNewLine: () => '\n',
    getDefaultLibFileName: () => defaultLibFilename,
    getSourceFile: filePath => {
      const systemContent = getSystemFileContent(sourceMap, filePath);
      if (systemContent != null) {
        return systemContent;
      } else if (filePath.trim() === path.trim()) {
        return input_source;
      } else {
        return ts.createSourceFile(
          filePath, externalGetSourceFile(filePath.trim()),
          ts.ScriptTarget.Latest);
      }
    },
    readFile: filePath => {
      return undefined;
    },
    useCaseSensitiveFileNames: () => true,
    writeFile: (name, contents) => {
      const DECLARATIONS_PREFIX = DECLARATIONS_DIRECTORY + '/';
      if (name.startsWith(DECLARATIONS_PREFIX)) {
        results.declaration_files[name.substring(DECLARATIONS_PREFIX.length)] =
          contents;
      } else {
        results.js_modules[name] = contents;
      }
    }
  };
  const program = ts.createProgram({ options, rootNames: [path], host });

  let diagnostics = ts.getPreEmitDiagnostics(program);
  if (diagnostics.length > 0) {
    let message = '';
    return {
      success: false,
      error: GetDiagnosticsError(diagnostics[0]),
    };
  } else {
    program.emit();
    if (Object.keys(results.js_modules).length == 0) {
      return {
        success: false,
        error: { message: 'No output generated.', line: 0, column: 0, path: path },
      };
    } else {
      let sourceFile = program.getSourceFile(path)!;

      let checker = program.getTypeChecker();
      let module_symbol = checker.getSymbolAtLocation(sourceFile);
      if (module_symbol === undefined) {
        return {
          success: false,
          error: { message: 'Error looking up module symbol for source file.', line: 0, column: 0, path: path },
        };
      }
      const moduleExports = checker.getExportsOfModule(module_symbol);
      results.module_exports = moduleExports.map(exportedSymbol => {
        const symbol = exportedSymbol;

        const declaration = exportedSymbol.declarations![0];
        const sourceFile = declaration.getSourceFile();
        const { fileName } = sourceFile;
        const { line, character } =
          sourceFile.getLineAndCharacterOfPosition(declaration.getStart());
        const type_string = checker.typeToString(
          checker.getTypeOfSymbolAtLocation(exportedSymbol, declaration));

        return {
          symbol_name: exportedSymbol.getName(),
          type_string: type_string,
          location: { path: fileName, line: line, column: character }
        };
      });

      return {
        success: true,
        output: results,
      };
    }
  }
}
