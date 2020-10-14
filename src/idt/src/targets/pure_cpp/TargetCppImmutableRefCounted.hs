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
        (DT.pack  $(makeRelativeToProject "templates/CppImmutableRefCountedEnum.stache.h" >>= embedStringFile))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ taggedUnionTemplate --exactprint-only
taggedUnionTemplate =
  case
      compileMustacheText
        "taggedUnionTemplate"
        (DT.pack
           $(makeRelativeToProject "templates/CppImmutableRefCountedTaggedUnion.stache.h" >>= embedStringFile))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ structTemplate --exactprint-only
structTemplate =
  case
      compileMustacheText
        "structTemplate"
        (DT.pack $(makeRelativeToProject "templates/CppImmutableRefCountedStruct.stache.h" >>= embedStringFile))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ topLevelTemplate --exactprint-only
topLevelTemplate =
  case
      compileMustacheText
        "topLevelTemplate"
        (DT.pack $(makeRelativeToProject "templates/CppImmutableRefCounted.stache.h" >>= embedStringFile))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

typeString :: Type -> String
typeString (Concrete (NamedType n _ _)) = n
typeString (Reference (NamedType n _ _)) = "std::shared_ptr<const " ++ n ++ ">"
typeString (NamedPrimitive n) = case n of
  "string"  -> "std::string"
  "f32"     -> "float"
  "i32"     -> "int"
  "boolean" -> "bool"
  _         -> panic $ "Unsupported primitive type: " ++ n
typeString (List t) = "std::vector<" ++ typeString t ++ ">"
typeString (Tuple l) =
  "std::tuple<" ++ intercalate ", " [ typeString x | x <- l ] ++ ">"
typeString (FixedSizeArray t s) =
  "std::array<" ++ typeString t ++ ", " ++ show s ++ ">"
typeString (Struct l) = panic "Structs may only be referenced as named types."
typeString (Enum   l) = panic "Enums may only be referenced as named types."
typeString (TaggedUnion l) =
  panic "TaggedUnions may only be referenced as named types."


enumTypeNames ts = map (("p" ++) . show) [0 .. length ts - 1]

contructorToStacheObject :: (String, String, [Type]) -> Value
contructorToStacheObject (n, _, ts) = object
  [ "cname" .= DT.pack n
  , "params"
    .= zipWith
         (\tn t ->
           object ["name" .= DT.pack tn, "type" .= DT.pack (typeString t)]
         )
         (enumTypeNames ts)
         ts
  ]

constructorNames :: [(String, String, [Type])] -> [String]
constructorNames = map (\(x, _, _) -> x)

constructors :: [(String, String, [Type])] -> [Value]
constructors = map contructorToStacheObject

member :: (String, String, Type) -> Value
member (n, c, t) = object
  [ "name" .= DT.pack n
  , "comment" .= DT.pack c
  , "type" .= DT.pack (typeString t)
  ]

members :: [(String, String, Type)] -> [Value]
members = map member

namedTypeDefinition :: Declaration -> String
namedTypeDefinition t = case t of
  ForwardDeclaration (NamedType n _ (Struct      _)) -> "class " ++ n ++ ";\n"
  ForwardDeclaration (NamedType n _ (Enum        _)) -> "class " ++ n ++ ";\n"
  ForwardDeclaration (NamedType n _ (TaggedUnion _)) -> "class " ++ n ++ ";\n"
  ForwardDeclaration _ ->
    panic "Only forward declarations of Enum and Structs are supported."
  TypeDeclaration (NamedType n c t@(Struct l)) ->
    DTL.unpack $ renderMustache structTemplate $ object
      ["name" .= n, "comment" .= c, "members" .= members l]
  TypeDeclaration (NamedType n c t@(Enum l)) -> if isSimpleEnum l
    then
      "// "
      ++ c
      ++ "\n"
      ++ "enum class "
      ++ n
      ++ " {\n"
      ++ unlines (map (\(en, ec, _) -> "  // " ++ ec ++ "\n  " ++ en ++ ",") l)
      ++ "};\n"
    else DTL.unpack $ renderMustache enumTemplate $ object
      [ "constructors" .= constructors l
      , "tagged_union_def" .= renderMustache
        taggedUnionTemplate
        (object
          [ "name" .= DT.pack n
          , "comment" .= DT.pack c
          , "comma_sep_types" .= DT.pack (intercalate ", " (constructorNames l))
          ]
        )
      ]
  TypeDeclaration (NamedType n c t@(TaggedUnion ts)) ->
    DTL.unpack $ renderMustache taggedUnionTemplate $ object
      [ "name" .= DT.pack n
      , "comment" .= DT.pack c
      , "comma_sep_types"
        .= DT.pack (intercalate ", " [ typeString t | t <- ts ])
      ]
  TypeDeclaration (NamedType n c t) ->
    "// " ++ c ++ "\n" ++ "using " ++ n ++ " = " ++ typeString t ++ ";\n"

toCppImmutableRefCountedSourceCode :: String -> DeclarationSequence -> String
toCppImmutableRefCountedSourceCode namespace decls =
  DTL.unpack $ renderMustache topLevelTemplate $ object
    [ "namespace" .= namespace
    , "declarationSequence" .= [ namedTypeDefinition x | x <- decls ]
    ]
