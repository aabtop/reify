module Main where

import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO

import           IdtProcessing
import           IdtStandardPipeline
import           TargetTypeScript

import           ReifyInputInterface

typescriptFileName = "reify_generated_interface.ts"

main = do
  args <- getArgs
  case args of
    [outputDirectory] ->
      let buildTargetToFile filename = buildTargetToFilePath
            (joinPath [outputDirectory, filename])
            ReifyInputInterface.idt
      in  do
            createDirectoryIfMissing True outputDirectory
            buildTargetToFile typescriptFileName toTypeScriptSourceCode
    _ -> do
      hPutStrLn stderr "Usage: Expected a single output directory parameter."
      hPutStrLn stderr ("Args: " ++ (show args))
      exitFailure

