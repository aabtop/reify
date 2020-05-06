import lib_es2015_core from 'raw-loader!./../node_modules/typescript/lib/lib.es2015.d.ts'
import lib_es5 from 'raw-loader!./../node_modules/typescript/lib/lib.es5.d.ts'
import * as ts from 'typescript';

const defaultLibFilename = '/lib.d.ts';
const defaultLibSourceFile = ts.createSourceFile(
    defaultLibFilename, lib_es5 + lib_es2015_core, ts.ScriptTarget.Latest);

export type TranspilationOutput = {
  // Mapping from a JS file path to file contents.
  js_modules: {[key: string]: string};
  // The file path of the primary module, e.g. the transpiled input source
  // module.  This can be used to index into |js_modules| above.
  primary_module: string;
};

export type TranspileResults = {
  // True if there were no errors.
  success: boolean;
  // Contains the error message if |success| is false, otherwise it is invalid.
  error_msg: string;
  // Contains the transpilation results if |success| is true, otherwise it is
  // invalid.
  output: TranspilationOutput;
};

function GetDiagnosticsMessage(diagnostic: string|ts.DiagnosticMessageChain|
                               undefined): string {
  if (typeof diagnostic === 'string') {
    return diagnostic;
  } else if (diagnostic === undefined) {
    return '';
  } else {
    return diagnostic.messageText;
  }
}


export function TranspileModule(
    text: string, systemModules: {[key: string]: string;}): TranspileResults {
  const dummyFilePath = 'dummy_input_file_path.ts';
  let results: TranspilationOutput = {
    js_modules: {},
    primary_module: 'dummy_input_file_path.js'
  };

  let sourceMap: {[key: string]: ts.SourceFile} = {
    [dummyFilePath]:
        ts.createSourceFile(dummyFilePath, text, ts.ScriptTarget.Latest),
    [defaultLibFilename]: defaultLibSourceFile,
  };
  for (var key in systemModules) {
    sourceMap[key] =
        ts.createSourceFile(key, systemModules[key], ts.ScriptTarget.Latest);
  }
  console.log('sourceMap:');
  console.log('/lib.d.ts' in sourceMap);
  const options: ts.CompilerOptions = {
    module: ts.ModuleKind.ES2015,
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
  const program = ts.createProgram({options, rootNames: [dummyFilePath], host});

  let diagnostics = ts.getPreEmitDiagnostics(program);
  if (diagnostics.length > 0) {
    let message = '';
    return {
      success: false,
      error_msg: GetDiagnosticsMessage(diagnostics[0].messageText),
      output: results
    };
  } else {
    program.emit();
    if (Object.keys(results.js_modules).length == 0) {
      return {
        success: false,
        error_msg: 'No output generated.',
        output: results
      };
    } else {
      return {success: true, error_msg: '', output: results};
    }
  }
}
