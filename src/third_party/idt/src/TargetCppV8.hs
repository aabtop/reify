module TargetCppV8
  ( toCppV8SourceCodeH
  , toCppV8SourceCodeCC
  )
where

import           Data.Aeson
import           Data.FileEmbed
import           Data.List
import           Data.Maybe
import           Text.Megaparsec
import qualified Data.Text                     as DT
import qualified Data.Text.Lazy                as DTL
import           Text.Mustache
import           Panic

import           Idt
import           IdtProcessing

-- brittany @ enumHTemplate --exactprint-only
enumHTemplate =
  case
      compileMustacheText
        "enumHTemplate"
        (DT.pack  $(embedStringFile "src/CppV8Enum.stache.h"))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ enumCCTemplate --exactprint-only
enumCCTemplate =
  case
      compileMustacheText
        "enumCCTemplate"
        (DT.pack  $(embedStringFile "src/CppV8Enum.stache.cc"))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ simpleEnumHTemplate --exactprint-only
simpleEnumHTemplate =
  case
      compileMustacheText
        "simpleEnumHTemplate"
        (DT.pack $(embedStringFile "src/CppV8SimpleEnum.stache.h"))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ taggedUnionHTemplate --exactprint-only
taggedUnionHTemplate =
  case
      compileMustacheText
        "taggedUnionHTemplate"
        (DT.pack  $(embedStringFile "src/CppV8TaggedUnion.stache.h"))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ taggedUnionCCTemplate --exactprint-only
taggedUnionCCTemplate =
  case
      compileMustacheText
        "taggedUnionCCTemplate"
        (DT.pack  $(embedStringFile "src/CppV8TaggedUnion.stache.cc"))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ structHTemplate --exactprint-only
structHTemplate =
  case
      compileMustacheText
        "structHTemplate"
        (DT.pack $(embedStringFile "src/CppV8Struct.stache.h"))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ structCCTemplate --exactprint-only
structCCTemplate =
  case
      compileMustacheText
        "structCCTemplate"
        (DT.pack $(embedStringFile "src/CppV8Struct.stache.cc"))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ topLevelHTemplate --exactprint-only
topLevelHTemplate =
  case
      compileMustacheText
        "topLevelHTemplate"
        (DT.pack $(embedStringFile "src/CppV8.stache.h"))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ topLevelCCTemplate --exactprint-only
topLevelCCTemplate =
  case
      compileMustacheText
        "topLevelCCTemplate"
        (DT.pack $(embedStringFile "src/CppV8.stache.cc"))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

typeString :: Type -> String
typeString (Concrete       (NamedType n _)) = n
typeString (Reference      (NamedType n _)) = "Ref<" ++ n ++ ">"
typeString (NamedPrimitive n              ) = case n of
  "string"  -> "v8::String"
  "f32"     -> "F32"
  "i32"     -> "I32"
  "boolean" -> "v8::Boolean"
  _         -> panic $ "Unsupported primitive type: " ++ n
typeString (List t) = "List<" ++ typeString t ++ ">"
typeString (Tuple l) =
  "Tuple<" ++ intercalate ", " [ typeString x | x <- l ] ++ ">"
typeString (FixedSizeArray t s) =
  "FixedSizeArray<" ++ typeString t ++ ", " ++ show s ++ ">"
typeString (Struct l) = panic "Structs may only be referenced as named types."
typeString (Enum   l) = panic "Enums may only be referenced as named types."
typeString (TaggedUnion l) =
  panic "TaggedUnions may only be referenced as named types."


enumTypeNames ts = map (("p" ++) . show) [0 .. length ts - 1]

unionMemberToStacheObject :: (String, [(String, Type)]) -> Value
unionMemberToStacheObject (k, ts) =
  let params =
          [ object ["name" .= DT.pack tn, "type" .= DT.pack (typeString t)]
          | (tn, t) <- ts
          ]
  in  object
        (  ["__kind" .= DT.pack k, "params" .= params]
        ++ [ "firstParam" .= head params | (not . null) params ]
        )

enumMembers :: [(String, [Type])] -> [Value]
enumMembers es =
  [ unionMemberToStacheObject (n, zip (enumTypeNames ts) ts) | (n, ts) <- es ]

taggedUnionTypeName :: Type -> String
taggedUnionTypeName t = case t of
  Concrete (NamedType n _) -> n
  Reference (NamedType n _) -> n
  _ -> panic "TaggedUnions may only be passed named types."

taggedUnionMembers :: [Type] -> [Value]
taggedUnionMembers ts =
  [ unionMemberToStacheObject (taggedUnionTypeName t, [("p0", t)]) | t <- ts ]

member :: (String, Type) -> Value
member (n, t) =
  object ["memberName" .= DT.pack n, "type" .= DT.pack (typeString t)]

members :: [(String, Type)] -> [Value]
members = map member

templateObjectForDecl :: String -> String -> NamedType -> Value
templateObjectForDecl namespace immRefCntNamespace (NamedType n t) =
  object
    $  [ "name" .= n
       , "namespace" .= namespace
       , "immRefCntNamespace" .= immRefCntNamespace
       ]
    ++ case t of
         Struct      l -> ["members" .= members l]
         Enum        l -> ["unionMembers" .= enumMembers l]
         TaggedUnion l -> ["unionMembers" .= taggedUnionMembers l]
         _             -> panic "Unexpected object in templateObjectForDecl."

namedTypeDefinition :: String -> String -> Declaration -> String
namedTypeDefinition namespace immRefCntNamespace t = case t of
  ForwardDeclaration (NamedType n (Struct      _)) -> "class " ++ n ++ ";\n"
  ForwardDeclaration (NamedType n (Enum        _)) -> "class " ++ n ++ ";\n"
  ForwardDeclaration (NamedType n (TaggedUnion _)) -> "class " ++ n ++ ";\n"
  ForwardDeclaration _ ->
    panic
      "Only forward declarations of Enum, Structs and TaggedUnions are supported."
  TypeDeclaration nt@(NamedType _ (Struct _)) ->
    DTL.unpack $ renderMustache structHTemplate $ templateObject nt
  TypeDeclaration nt@(NamedType n (Enum cs)) -> if isSimpleEnum cs
    then DTL.unpack $ renderMustache simpleEnumHTemplate $ templateObject nt
    else DTL.unpack $ renderMustache enumHTemplate $ templateObject nt
  TypeDeclaration nt@(NamedType _ (TaggedUnion _)) ->
    DTL.unpack $ renderMustache taggedUnionHTemplate $ templateObject nt
  TypeDeclaration (NamedType n t) ->
    "using " ++ n ++ " = " ++ typeString t ++ ";\n"
  where templateObject = templateObjectForDecl namespace immRefCntNamespace

toCppV8SourceCodeH
  :: String -> String -> String -> DeclarationSequence -> String
toCppV8SourceCodeH namespace immutableRefCountedHeaderFile immRefCntNamespace decls
  = DTL.unpack $ renderMustache topLevelHTemplate $ object
    [ "namespace" .= namespace
    , "immutableRefCountedHeaderFile" .= immutableRefCountedHeaderFile
    , "immRefCntNamespace" .= immRefCntNamespace
    , "declarationSequence"
      .= [ namedTypeDefinition namespace immRefCntNamespace x | x <- decls ]
    ]

constructorDefinition :: Declaration -> Maybe Value
constructorDefinition (TypeDeclaration nt@(NamedType n t@(Struct l))) =
  Just $ object ["name" .= DT.pack n]
constructorDefinition _ = Nothing

convertToImmRefCntFunctions :: String -> String -> Declaration -> Maybe String
convertToImmRefCntFunctions namespace immRefCntNamespace decl = case decl of
  (TypeDeclaration nt@(NamedType _ (Struct _))) ->
    Just $ DTL.unpack $ renderMustache structCCTemplate $ templateObject nt
  (TypeDeclaration nt@(NamedType _ (Enum cs))) -> if not . isSimpleEnum $ cs
    then Just $ DTL.unpack $ renderMustache enumCCTemplate $ templateObject nt
    else Nothing
  (TypeDeclaration nt@(NamedType _ (TaggedUnion _))) ->
    Just $ DTL.unpack $ renderMustache taggedUnionCCTemplate $ templateObject nt
  _ -> Nothing
  where templateObject = templateObjectForDecl namespace immRefCntNamespace

toCppV8SourceCodeCC
  :: String -> String -> String -> String -> DeclarationSequence -> String
toCppV8SourceCodeCC namespace v8HeaderFile immutableRefCountedHeaderFile immRefCntNamespace decls
  = DTL.unpack $ renderMustache topLevelCCTemplate $ object
    [ "namespace" .= namespace
    , "v8HeaderFile" .= v8HeaderFile
    , "immutableRefCountedHeaderFile" .= immutableRefCountedHeaderFile
    , "immRefCntNamespace" .= immRefCntNamespace
    , "constructorDefinitions" .= mapMaybe constructorDefinition decls
    , "convertToImmRefCntFunctions"
      .= mapMaybe (convertToImmRefCntFunctions namespace immRefCntNamespace)
                  decls
    ]
