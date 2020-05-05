import * as ts from 'typescript';

export type TranspileResults = {
  success: boolean;
  result: string;  // Either the source code in the case of success, or the
                   // error message in the case of an error.
};

export function TranspileModule(source: string): TranspileResults {
  let result = ts.transpileModule(
      source, {compilerOptions: {module: ts.ModuleKind.CommonJS}});

  return {success: true, result: result.outputText};
}
