module Main where

import           IdtProcessing
import           IdtStandardPipeline
import           TargetHaskell

import           ReifyIdt

main = buildTargetToStdio reifyIdt toHaskellSourceCode
