module Main where

import           Text.Groom
import           System.IO

import           Idt
import           IdtIdt
import           IdtProcessing
import           TargetHaskell

declarationSequence = asDeclarationSequence idtIdt

main :: IO ()
main = case declarationSequence of
  Right declarations -> putStrLn (toHaskellSourceCode declarations)
  Left (CyclicDependencyError et) -> hPutStrLn
    stderr
    (  "Cyclic dependency on type \""
    ++ name et
    ++ "\".  Consider changing a concrete type to a reference type."
    )

