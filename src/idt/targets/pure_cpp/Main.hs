module Main where

import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO

import           IdtProcessing
import           IdtStandardPipeline
import           TargetCppImmutableRefCounted

import           ReifyInputInterface

main = do
  args <- getArgs
  case args of
    [namespace, outputDirectory] ->
      let buildTargetToFile filename = buildTargetToFilePath
            (joinPath [outputDirectory, filename])
            ReifyInputInterface.idt
      in  do
            createDirectoryIfMissing True outputDirectory
            buildTargetToFile
              (namespace ++ ".h")
              (toCppImmutableRefCountedSourceCode namespace)
    _ -> do
      hPutStrLn stderr "Usage: Expected two parameters, [namespace, outputDirectory]."
      hPutStrLn stderr ("Args: " ++ (show args))
      exitFailure

