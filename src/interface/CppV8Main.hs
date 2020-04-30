module Main where

import           IdtProcessing
import           IdtStandardPipeline
import           TargetCppV8

import           ReifyIdt

main = buildTargetToStdio reifyIdt (toCppV8SourceCode "reify")
