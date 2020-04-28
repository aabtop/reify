module Main where

import           IdtProcessing
import           IdtStandardPipeline
import           TargetTypeScript

import           ReifyIdt

main = buildTargetToStdio reifyIdt toTypeScriptSourceCode
