import lib_es2015_core from 'raw-loader!./../node_modules/typescript/lib/lib.es2015.d.ts'
import lib_es5 from 'raw-loader!./../node_modules/typescript/lib/lib.es5.d.ts'
import * as ts from 'typescript';

const defaultLibContent = lib_es5 + lib_es2015_core;

export type TranspileResults = {
  success: boolean;
  result: string;  // Either the source code in the case of success, or the
                   // error message in the case of an error.
};

function GetDiagnosticsMessage(diagnostic: string|ts.DiagnosticMessageChain|
                               undefined): String {
  if (typeof diagnostic === 'string') {
    return diagnostic;
  } else if (diagnostic === undefined) {
    return '';
  } else {
    return diagnostic.messageText;
  }
}


export function TranspileModule(text: string) {
  const dummyFilePath = '/workspaces/reify/src/test_data/error.ts';
  const textAst =
      ts.createSourceFile(dummyFilePath, text, ts.ScriptTarget.Latest);

  const defaultLibAst = ts.createSourceFile(
      '/lib.d.ts', defaultLibContent, ts.ScriptTarget.Latest);

  let writtenFiles: {[key: string]: string} = {};
  const options: ts.CompilerOptions = {};
  const host: ts.CompilerHost = {
    fileExists: filePath => filePath === dummyFilePath,
    directoryExists: dirPath => dirPath === '/',
    getCurrentDirectory: () => '/',
    getDirectories: path => [],
    getCanonicalFileName: fileName => fileName,
    getNewLine: () => '\n',
    getDefaultLibFileName: () => '/lib.d.ts',
    getSourceFile: filePath => filePath === dummyFilePath ?
        textAst :
        filePath === '/lib.d.ts' ? defaultLibAst : undefined,
    readFile: filePath => filePath === dummyFilePath ? text : undefined,
    useCaseSensitiveFileNames: () => true,
    writeFile: (name, text) => writtenFiles[name] = text
  };
  const program = ts.createProgram({options, rootNames: [dummyFilePath], host});

  let diagnostics = ts.getPreEmitDiagnostics(program);
  if (diagnostics.length > 0) {
    let message = '';
    return {
      success: false,
      result: GetDiagnosticsMessage(diagnostics[0].messageText)
    };
  } else {
    program.emit();
    if (Object.keys(writtenFiles).length != 1) {
      return {success: false, result: 'Expected only one file to be output.'};
    } else {
      return {success: true, result: Object.values(writtenFiles)[0]};
    }
  }
}
