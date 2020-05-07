module Main where

import           IdtProcessing
import           IdtStandardPipeline
import           TargetCppImmutableRefCounted

import           ReifyIdt

main = buildTargetToStdio reifyIdt (toCppImmutableRefCountedSourceCode "reify")
