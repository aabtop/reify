module Main where

import           IdtProcessing
import           IdtStandardPipeline
import           ReifyIdt
import           TargetTypeScript

main = buildTargetToStdio reifyIdt toTypeScriptSourceCode
