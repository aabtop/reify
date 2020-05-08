module Main where

import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO

import           IdtProcessing
import           IdtStandardPipeline
import           TargetCppV8
import           TargetTypeScript
import           TargetCppImmutableRefCounted

import           ReifyIdt

cppV8Namespace = ReifyIdt.namespace ++ "_v8"

cppImmutableRefCountedFileName = "reify_cpp_immut_ref_counted_interface.h"
typescriptFileName = "reify_ts_interface.ts"
cppV8HFileName = "reify_cpp_v8_interface.h"
cppV8CCFileName = "reify_cpp_v8_interface.cc"

main = do
  args <- getArgs
  case args of
    [outputDirectory] ->
      let buildTargetToFile filename = buildTargetToFilePath
            (joinPath [outputDirectory, filename])
            ReifyIdt.reifyIdt
      in  do
            buildTargetToFile
              cppV8HFileName
              (toCppV8SourceCodeH cppV8Namespace
                                  cppImmutableRefCountedFileName
                                  ReifyIdt.namespace
              )
            buildTargetToFile
              cppV8CCFileName
              (toCppV8SourceCodeCC cppV8Namespace
                                   cppV8HFileName
                                   cppImmutableRefCountedFileName
                                   ReifyIdt.namespace
              )
            buildTargetToFile
              cppImmutableRefCountedFileName
              (toCppImmutableRefCountedSourceCode ReifyIdt.namespace)
            buildTargetToFile typescriptFileName toTypeScriptSourceCode
    _ -> do
      hPutStrLn stderr "Usage: Expected a single output directory parameter."
      exitFailure

