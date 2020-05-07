module TargetCppV8
  ( toCppV8SourceCode
  )
where

import           Data.Aeson
import           Data.FileEmbed
import           Data.List
import           Text.Megaparsec
import qualified Data.Text                     as DT
import qualified Data.Text.Lazy                as DTL
import           Text.Mustache
import           Panic

import           Idt
import           IdtProcessing

-- brittany @ enumTemplate --exactprint-only
enumTemplate =
  case
      compileMustacheText
        "enumTemplate"
        (DT.pack  $(embedStringFile "src/CppV8Enum.stache.h"))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ structTemplate --exactprint-only
structTemplate =
  case
      compileMustacheText
        "structTemplate"
        (DT.pack $(embedStringFile "src/CppV8Struct.stache.h"))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ topLevelTemplate --exactprint-only
topLevelTemplate =
  case
      compileMustacheText
        "topLevelTemplate"
        (DT.pack $(embedStringFile "src/CppV8.stache.h"))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

typeString :: Type -> String
typeString (Concrete       (NamedType n _)) = n
typeString (Reference      (NamedType n _)) = n
typeString (NamedPrimitive n              ) = case n of
  "string" -> "v8::String"
  "f32"    -> "v8::Number"
  _        -> panic $ "Unsupported primitive type: " ++ n
typeString (List t) = "List<" ++ typeString t ++ ">"
typeString (Tuple l) =
  "Tuple<" ++ intercalate ", " [ typeString x | x <- l ] ++ ">"
typeString (Enum   l) = panic "Enums may only be referenced as named types."
typeString (Struct l) = panic "Structs may only be referenced as named types."


enumTypeNames ts = map (("p" ++) . show) [0 .. length ts - 1]

contructorToStacheObject :: (String, [Type]) -> Value
contructorToStacheObject (n, ts) =
  object
    $ ("__kind" .= DT.pack n)
    : [ DT.pack tn .= DT.pack (typeString t)
      | (tn, t) <- zip (enumTypeNames ts) ts
      ]

constructors :: [(String, [Type])] -> [Value]
constructors = map contructorToStacheObject

member :: (String, Type) -> Value
member (n, t) = object ["name" .= DT.pack n, "type" .= DT.pack (typeString t)]

members :: [(String, Type)] -> [Value]
members = map member

namedTypeDefinition :: Declaration -> String
namedTypeDefinition t = case t of
  ForwardDeclaration (NamedType n (Enum   _)) -> "class " ++ n ++ ";\n"
  ForwardDeclaration (NamedType n (Struct _)) -> "class " ++ n ++ ";\n"
  ForwardDeclaration _ ->
    panic "Only forward declarations of Enum and Structs are supported."
  TypeDeclaration (NamedType n t@(Enum l)) ->
    DTL.unpack $ renderMustache enumTemplate $ object
      ["name" .= n, "constructors" .= constructors l]
  TypeDeclaration (NamedType n t@(Struct l)) ->
    DTL.unpack $ renderMustache structTemplate $ object
      ["name" .= n, "members" .= members l]
  TypeDeclaration (NamedType n t) ->
    "using " ++ n ++ " = " ++ typeString t ++ ";\n"

toCppV8SourceCode :: String -> DeclarationSequence -> String
toCppV8SourceCode namespace decls =
  DTL.unpack $ renderMustache topLevelTemplate $ object
    [ "namespace" .= namespace
    , "declarationSequence" .= [ namedTypeDefinition x | x <- decls ]
    ]
