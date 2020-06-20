import lib_es2015_core from 'raw-loader!./../node_modules/typescript/lib/lib.es2015.core.d.ts'
import lib_es5 from 'raw-loader!./../node_modules/typescript/lib/lib.es5.d.ts'
import reify_root_d_ts from 'raw-loader!./../resources/reify_root.asset.d.ts'
import * as ts from 'typescript';

const defaultLibFilename = '/lib.d.ts';
const defaultLibSourceFile = ts.createSourceFile(
    defaultLibFilename, lib_es5 + lib_es2015_core + reify_root_d_ts,
    ts.ScriptTarget.Latest);

export type ModuleExport = {
  symbol_name: string; type_string: string;
  location: {path: string; line: number; column: number;};
};

export type TranspilationOutput = {
  // Mapping from a JS file path to file contents.
  js_modules: {[key: string]: string};
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

function GetDiagnosticsMessage(diagnostic: string|ts.DiagnosticMessageChain|
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
    let {line, character} =
        diagnostic.file.getLineAndCharacterOfPosition(diagnostic.start!);
    transpile_error.line = line;
    transpile_error.column = character;
    transpile_error.path = diagnostic.file.fileName;
  }
  return transpile_error;
}

export function TranspileModule(
    path: string, text: string,
    systemModules: {[key: string]: string;}): TranspileResults {
  let results: TranspilationOutput = {
    js_modules: {},
    primary_module: path.split('.').slice(0, -1).join('.') + '.js',
    module_exports: [],
  };

  let sourceMap: {[key: string]: ts.SourceFile} = {
    [path]: ts.createSourceFile(path, text, ts.ScriptTarget.Latest),
    [defaultLibFilename]: defaultLibSourceFile,
  };
  for (var key in systemModules) {
    sourceMap[key] =
        ts.createSourceFile(key, systemModules[key], ts.ScriptTarget.Latest);
  }
  const options: ts.CompilerOptions = {
    module: ts.ModuleKind.ES2015,
    strictNullChecks: true,
    noImplicitReturns: true,
    noImplicitThis: true,
    noImplicitAny: true,
    noErrorTruncation: true,
  };
  const host: ts.CompilerHost = {
    fileExists: filePath => {
      console.log('fileExists: ' + filePath);
      return filePath in sourceMap;
    },
    directoryExists: dirPath => {
      console.log('directoryExists: ' + dirPath);
      // return true;
      return dirPath === '/';
    },
    getCurrentDirectory: () => '/',
    getDirectories: path => {
      console.log('getDirectories: ' + path);
      return [];
    },
    getCanonicalFileName: fileName => fileName,
    getNewLine: () => '\n',
    getDefaultLibFileName: () => defaultLibFilename,
    getSourceFile: filePath => {
      console.log('getSourceFile: ' + filePath);
      console.log('found: ' + filePath in sourceMap)
      return sourceMap[filePath];
    },
    readFile: filePath => {
      console.log('readFile: ' + filePath);
      return undefined;
    },
    useCaseSensitiveFileNames: () => true,
    writeFile: (name, text) => {
      console.log('writeFile---');
      console.log('name: ' + name);
      console.log('text: ' + text);
      results.js_modules[name] = text;
    }
  };
  const program = ts.createProgram({options, rootNames: [path], host});

  let diagnostics = ts.getPreEmitDiagnostics(program);
  if (diagnostics.length > 0) {
    console.log(diagnostics[0]);
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
        error: {message: 'No output generated.', line: 0, column: 0, path: ''},
      };
    } else {
      let sourceFile = program.getSourceFile(path)!;

      let checker = program.getTypeChecker();
      let module_symbol = checker.getSymbolAtLocation(sourceFile)!;
      const moduleExports = checker.getExportsOfModule(module_symbol);
      results.module_exports = moduleExports.map(exportedSymbol => {
        const symbol = exportedSymbol;

        const declaration = exportedSymbol.declarations![0];
        const sourceFile = declaration.getSourceFile();
        const {fileName} = sourceFile;
        const {line, character} =
            sourceFile.getLineAndCharacterOfPosition(declaration.getStart());
        const type_string = checker.typeToString(
            checker.getTypeOfSymbolAtLocation(exportedSymbol, declaration));

        return {
          symbol_name: exportedSymbol.getName(),
          type_string: type_string,
          location: {path: fileName, line: line, column: character}
        };
      });

      return {
        success: true,
        output: results,
      };
    }
  }
}
