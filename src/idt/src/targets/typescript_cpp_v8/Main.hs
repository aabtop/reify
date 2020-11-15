module Main where

import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO

import           IdtProcessing
import           IdtStandardPipeline
import           TargetCppV8

import           ReifyInputInterface

main = do
  args <- getArgs
  case args of
    [namespace, outputDirectory] ->
      let buildTargetToFile filename = buildTargetToFilePath
            (joinPath [outputDirectory, filename])
            ReifyInputInterface.idt
          cppV8Namespace = namespace ++ "_v8"
          cppV8HFileName = namespace ++ ".h"
          cppV8CCFileName = namespace ++ ".cc"
          cppImmutableRefCountedFileName = (namespace ++ ".h")
      in  do
            createDirectoryIfMissing True outputDirectory
            buildTargetToFile
              cppV8HFileName
              (toCppV8SourceCodeH cppV8Namespace
                                  cppImmutableRefCountedFileName
                                  namespace
              )
            buildTargetToFile
              cppV8CCFileName
              (toCppV8SourceCodeCC cppV8Namespace
                                   cppV8HFileName
                                   cppImmutableRefCountedFileName
                                   namespace
              )
    _ -> do
      hPutStrLn stderr "Usage: Expected two parameters, [namespace, outputDirectory]."
      hPutStrLn stderr ("Args: " ++ (show args))
      exitFailure

