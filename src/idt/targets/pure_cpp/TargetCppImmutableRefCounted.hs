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

typeString :: Bool -> Type -> String
typeString _ (Concrete (NamedType n _ _)) = n
typeString enableHashes (Reference (NamedType n _ _)) =
  if enableHashes then
    "::reify::CachedHashReference<" ++ n ++ ">"
  else
    "std::shared_ptr<const " ++ n ++ ">"
typeString _ (NamedPrimitive n) = case n of
  "string"  -> "std::string"
  "f32"     -> "float"
  "i32"     -> "int"
  "boolean" -> "bool"
  _         -> panic $ "Unsupported primitive type: " ++ n
typeString enableHashes (List t) = "std::vector<" ++ typeString enableHashes t ++ ">"
typeString enableHashes (Tuple l) =
  "std::tuple<" ++ intercalate ", " [ typeString enableHashes x | x <- l ] ++ ">"
typeString enableHashes (FixedSizeArray t s) =
  "std::array<" ++ typeString enableHashes t ++ ", " ++ show s ++ ">"
typeString _ (Struct l) = panic "Structs may only be referenced as named types."
typeString _ (Enum   l) = panic "Enums may only be referenced as named types."
typeString _ (TaggedUnion l) =
  panic "TaggedUnions may only be referenced as named types."


enumTypeNames ts = map (("p" ++) . show) [0 .. length ts - 1]

contructorToStacheObject :: Bool -> (String, String, [Type]) -> Value
contructorToStacheObject enableHashes (n, _, ts) = object
  [ "cname" .= DT.pack n
  , "params"
    .= zipWith
         (\tn t ->
           object ["name" .= DT.pack tn, "type" .= DT.pack (typeString enableHashes t)]
         )
         (enumTypeNames ts)
         ts
  ]

constructorNames :: [(String, String, [Type])] -> [String]
constructorNames = map (\(x, _, _) -> x)

constructors :: Bool -> [(String, String, [Type])] -> [Value]
constructors enableHashes = map (contructorToStacheObject enableHashes)

member :: Bool -> (String, String, Type) -> Value
member enableHashes (n, c, t) = object
  [ "name" .= DT.pack n
  , "comment" .= DT.pack c
  , "type" .= DT.pack (typeString enableHashes t)
  ]

members :: Bool -> [(String, String, Type)] -> [Value]
members enableHashes = map (member enableHashes)

namedTypeDefinition :: Bool -> String -> Declaration -> String
namedTypeDefinition enableHashes namespace t = case t of
  ForwardDeclaration (NamedType n _ (Struct      _)) ->
    "struct " ++ n ++ ";\n"
  ForwardDeclaration (NamedType n _ (Enum        _)) -> "class " ++ n ++ ";\n"
  ForwardDeclaration (NamedType n _ (TaggedUnion _)) -> "class " ++ n ++ ";\n"
  ForwardDeclaration _ ->
    panic "Only forward declarations of Enum and Structs are supported."
  TypeDeclaration (NamedType n c t@(Struct l)) ->
    DTL.unpack $ renderMustache structTemplate $ object
      ["name" .= n, "comment" .= c, "members" .= members enableHashes l, "enable_hashes" .= enableHashes, "no_enable_hashes" .= not enableHashes, "namespace" .= namespace]
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
      ++ "\n"
      ++ if enableHashes then
           "inline void AddObjectToHash(blake3_hasher* hasher, "++ n ++ " input) {\n"
           ++ "  blake3_hasher_update(hasher, \n"
           ++ "  reinterpret_cast<const uint8_t*>(&input), sizeof(input));\n"
           ++ "}\n"
         else
           ""
    else DTL.unpack $ renderMustache enumTemplate $ object
      [ "constructors" .= constructors enableHashes l
      , "tagged_union_def" .= renderMustache
        taggedUnionTemplate
        (object
          [ "name" .= DT.pack n
          , "comment" .= DT.pack c
          , "comma_sep_types" .= DT.pack (intercalate ", " (constructorNames l))
          , "namespace" .= DT.pack namespace
          ]
        )
      , "enable_hashes" .= enableHashes
      , "namespace" .= namespace
      ]
  TypeDeclaration (NamedType n c t@(TaggedUnion ts)) ->
    DTL.unpack $ renderMustache taggedUnionTemplate $ object
      [ "name" .= DT.pack n
      , "comment" .= DT.pack c
      , "comma_sep_types"
        .= DT.pack (intercalate ", " [ typeString enableHashes t | t <- ts ])
      , "enable_hashes" .= enableHashes
      , "namespace" .= namespace
      ]
  TypeDeclaration (NamedType n c t) ->
    "// " ++ c ++ "\n" ++ "using " ++ n ++ " = " ++ typeString enableHashes t ++ ";\n"

toCppImmutableRefCountedSourceCode :: String -> Bool -> DeclarationSequence -> String
toCppImmutableRefCountedSourceCode namespace enableHashes decls =
  DTL.unpack $ renderMustache topLevelTemplate $ object
    [ "namespace" .= namespace
    , "declarationSequence" .= [ namedTypeDefinition enableHashes namespace x | x <- decls ]
    , "enable_hashes" .= enableHashes
    , "no_enable_hashes" .= not enableHashes
    ]
