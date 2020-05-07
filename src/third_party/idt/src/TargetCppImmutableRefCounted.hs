module TargetCppImmutableRefCounted
  ( toCppImmutableRefCountedSourceCode
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
        (DT.pack  $(embedStringFile "src/CppImmutableRefCountedEnum.stache.h"))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ structTemplate --exactprint-only
structTemplate =
  case
      compileMustacheText
        "structTemplate"
        (DT.pack $(embedStringFile "src/CppImmutableRefCountedStruct.stache.h"))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ topLevelTemplate --exactprint-only
topLevelTemplate =
  case
      compileMustacheText
        "topLevelTemplate"
        (DT.pack $(embedStringFile "src/CppImmutableRefCounted.stache.h"))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

typeString :: Type -> String
typeString (Concrete       (NamedType n _)) = n
typeString (Reference      (NamedType n _)) = "std::shared_ptr<" ++ n ++ ">"
typeString (NamedPrimitive n              ) = case n of
  "string" -> "std::string"
  "f32"    -> "float"
  _        -> panic $ "Unsupported primitive type: " ++ n
typeString (List t) = "std::vector<" ++ typeString t ++ ">"
typeString (Tuple l) =
  "std::tuple<" ++ intercalate ", " [ typeString x | x <- l ] ++ ">"
typeString (Enum   l) = panic "Enums may only be referenced as named types."
typeString (Struct l) = panic "Structs may only be referenced as named types."


enumTypeNames ts = map (("p" ++) . show) [0 .. length ts - 1]

contructorToStacheObject :: (String, [Type]) -> Value
contructorToStacheObject (n, ts) =
  object
    $ ("cname" .= DT.pack n)
    : [ DT.pack tn .= DT.pack (typeString t)
      | (tn, t) <- zip (enumTypeNames ts) ts
      ]

enumTypeStrings :: [(String, [Type])] -> [String]
enumTypeStrings = map (\(n, ts) -> typeString $ head ts)

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
      [ "name" .= n
      , "constructors" .= constructors l
      , "comma_sep_types" .= DT.pack (intercalate ", " (enumTypeStrings l))
      ]
  TypeDeclaration (NamedType n t@(Struct l)) ->
    DTL.unpack $ renderMustache structTemplate $ object
      ["name" .= n, "members" .= members l]
  TypeDeclaration (NamedType n t) ->
    "using " ++ n ++ " = " ++ typeString t ++ ";\n"

toCppImmutableRefCountedSourceCode :: String -> DeclarationSequence -> String
toCppImmutableRefCountedSourceCode namespace decls =
  DTL.unpack $ renderMustache topLevelTemplate $ object
    [ "namespace" .= namespace
    , "declarationSequence" .= [ namedTypeDefinition x | x <- decls ]
    ]
