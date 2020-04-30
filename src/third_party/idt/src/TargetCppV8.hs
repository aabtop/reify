module TargetCppV8
  ( toCppV8SourceCode
  )
where

import Data.Aeson
import Data.FileEmbed
import Text.Megaparsec
import Data.Text
import qualified Data.Text.Lazy as DTL
import Text.Mustache
import Panic

import Idt
import IdtProcessing

toCppV8SourceCode :: DeclarationSequence -> String
toCppV8SourceCode decls =
  let
    topLevelTemplateData = $(embedStringFile "src/CppV8Template.stache.h")
    topLevelTemplate =
      compileMustacheText "topLevelTemplate" (pack topLevelTemplateData)
  in
    case topLevelTemplate of
      Left bundle -> panic (errorBundlePretty bundle)
      Right template -> DTL.unpack $ renderMustache template $ object
        [ "namespace"   .= ("John" :: Text)
        , "declarationSequence" .= ["decl1" :: Text, "decl2"]
        ]
