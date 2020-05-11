module Main where

import           Data.Aeson
import           Data.Aeson.Types
import           GHC.Generics

import           ReifyInputInterface

data ConfigOutput = ConfigOutput {
  namespace :: String,
  typescriptLibDir :: String
} deriving (Generic, Show)
instance ToJSON ConfigOutput

main = print $ encode $ ConfigOutput
  { Main.namespace        = ReifyInputInterface.namespace
  , Main.typescriptLibDir = ReifyInputInterface.typescriptLibDir
  }

