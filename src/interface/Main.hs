module Main where

import           System.IO

import           Idt
import           ReifyIdt
import           IdtProcessing
import           TargetHaskell
import           TargetTypeScript

declarationSequence = asDeclarationSequence reifyIdt

main :: IO ()
main = case declarationSequence of
  Right declarations -> putStrLn (toTypeScriptSourceCode declarations)
  Left (CyclicDependencyError et) -> hPutStrLn
    stderr
    (  "Cyclic dependency on type \""
    ++ name et
    ++ "\".  Consider changing a concrete type to a reference type."
    )

