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
            hPutStrLn stderr ("Usage: Expected two parameters, [" ++ namespace ++ ", " ++ outputDirectory ++ "].")
            buildTargetToFile
              (namespace ++ ".h")
              (toCppImmutableRefCountedSourceCode namespace)
            currDir <- getCurrentDirectory
            hPutStrLn stderr ("Current directory: " ++ currDir)
    _ -> do
      hPutStrLn stderr "Usage: Expected two parameters, [namespace, outputDirectory]."
      hPutStrLn stderr ("Args: " ++ (show args))
      exitFailure

