module IdtStandardPipeline
  ( buildTargetToStdio
  )
where

import           System.Exit
import           System.IO
import           Idt
import           IdtProcessing

buildTargetToStdio :: NamedTypeList -> (DeclarationSequence -> String) -> IO ()
buildTargetToStdio idt targetConverter =
  case (asDeclarationSequence idt) of
    Right declarations -> putStrLn (targetConverter declarations)
    Left (CyclicDependencyError et) -> do
      hPutStrLn
        stderr
        (  "Cyclic dependency on type \""
        ++ name et
        ++ "\".  Consider changing a concrete type to a reference type."
        )
      exitFailure


