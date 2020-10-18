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
          cppV8HFileName = "reify_cpp_v8_interface.h"
          cppV8CCFileName = "reify_cpp_v8_interface.cc"
          cppImmutableRefCountedFileName = (namespace ++ ".h")
      in  do
            createDirectoryIfMissing True outputDirectory
            hPutStrLn stderr ("cppV8CCFileName: " ++ outputDirectory ++ "/" ++ cppV8CCFileName)
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

