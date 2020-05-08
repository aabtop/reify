module IdtStandardPipeline
  ( buildTargetToStdio
  , buildTargetToFilePath
  , buildTargetToHandle
  )
where

import           System.Exit
import           System.FilePath
import           System.IO

import           Idt
import           IdtProcessing

buildTargetToStdio :: NamedTypeList -> (DeclarationSequence -> String) -> IO ()
buildTargetToStdio = buildTargetToHandle stdout

buildTargetToFilePath
  :: FilePath -> NamedTypeList -> (DeclarationSequence -> String) -> IO ()
buildTargetToFilePath path idt targetConverter = do
  h <- openFile path WriteMode
  buildTargetToHandle h idt targetConverter
  hClose h

buildTargetToHandle
  :: Handle -> NamedTypeList -> (DeclarationSequence -> String) -> IO ()
buildTargetToHandle handle idt targetConverter =
  case asDeclarationSequence idt of
    Right declarations -> hPutStrLn handle (targetConverter declarations)
    Left (CyclicDependencyError et) -> do
      hPutStrLn
        stderr
        (  "Cyclic dependency on type \""
        ++ name et
        ++ "\".  Consider changing a concrete type to a reference type."
        )
      exitFailure

