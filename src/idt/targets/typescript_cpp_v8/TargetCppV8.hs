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
        (DT.pack  $(makeRelativeToProject "templates/CppV8Enum.stache.h" >>= embedStringFile))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ enumCCTemplate --exactprint-only
enumCCTemplate =
  case
      compileMustacheText
        "enumCCTemplate"
        (DT.pack  $(makeRelativeToProject "templates/CppV8Enum.stache.cc" >>= embedStringFile))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ simpleEnumHTemplate --exactprint-only
simpleEnumHTemplate =
  case
      compileMustacheText
        "simpleEnumHTemplate"
        (DT.pack $(makeRelativeToProject "templates/CppV8SimpleEnum.stache.h" >>= embedStringFile))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ taggedUnionHTemplate --exactprint-only
taggedUnionHTemplate =
  case
      compileMustacheText
        "taggedUnionHTemplate"
        (DT.pack  $(makeRelativeToProject "templates/CppV8TaggedUnion.stache.h" >>= embedStringFile))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ taggedUnionCCTemplate --exactprint-only
taggedUnionCCTemplate =
  case
      compileMustacheText
        "taggedUnionCCTemplate"
        (DT.pack  $(makeRelativeToProject "templates/CppV8TaggedUnion.stache.cc" >>= embedStringFile))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ structHTemplate --exactprint-only
structHTemplate =
  case
      compileMustacheText
        "structHTemplate"
        (DT.pack $(makeRelativeToProject "templates/CppV8Struct.stache.h" >>= embedStringFile))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ structCCTemplate --exactprint-only
structCCTemplate =
  case
      compileMustacheText
        "structCCTemplate"
        (DT.pack $(makeRelativeToProject "templates/CppV8Struct.stache.cc" >>= embedStringFile))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ topLevelHTemplate --exactprint-only
topLevelHTemplate =
  case
      compileMustacheText
        "topLevelHTemplate"
        (DT.pack $(makeRelativeToProject "templates/CppV8.stache.h" >>= embedStringFile))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

-- brittany @ topLevelCCTemplate --exactprint-only
topLevelCCTemplate =
  case
      compileMustacheText
        "topLevelCCTemplate"
        (DT.pack $(makeRelativeToProject "templates/CppV8.stache.cc" >>= embedStringFile))
    of
      Left  bundle   -> panic (errorBundlePretty bundle)
      Right template -> template

typeString :: Type -> String
typeString (Concrete       (NamedType n _ _)) = n
typeString (Reference      (NamedType n _ _)) = "Ref<" ++ n ++ ">"
typeString (NamedPrimitive n                ) = case n of
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

unionMemberToStacheObject :: (String, [(String, Type, Type)]) -> Value
unionMemberToStacheObject (k, ts) =
  let params =
          [ object ["name" .= DT.pack tn, "type" .= DT.pack (typeString t), "topLevelType" .= DT.pack (typeString topLevelType)]
          | (tn, t, topLevelType) <- ts
          ]
  in  object
        (  ["__kind" .= DT.pack k, "params" .= params]
        ++ [ "firstParam" .= head params | (not . null) params ]
        )

enumMembers :: [(String, String, [Type])] -> [Value]
enumMembers es =
  -- This topLevelParam shit makes no sense for enumMembers.
  [ unionMemberToStacheObject (n, zip3 (enumTypeNames ts) ts ts)
  | (n, _, ts) <- es
  ]

taggedUnionTypeName :: Type -> String
taggedUnionTypeName t = case t of
  Concrete (NamedType n _ _) -> n
  Reference (NamedType n _ _) -> n
  _ -> panic "TaggedUnions may only be passed named types."

taggedUnionMember :: Type -> Type -> Value
taggedUnionMember topLevelType t = unionMemberToStacheObject (taggedUnionTypeName t, [("p0", t, topLevelType)])

taggedUnionMembers :: [Type] -> [Value]
taggedUnionMembers ts =
  [taggedUnionMember t t | t <- ts]

recursiveTaggedUnionMember :: Maybe Type -> Bool -> Type -> [Value]
recursiveTaggedUnionMember topLevelType leavesOnly t = 
  let topLevelTypeOr n = fromMaybe n topLevelType
      recursiveCall parentType topLevelType ts =
        (if leavesOnly then [] else [taggedUnionMember (topLevelTypeOr parentType) parentType]) ++ (recursiveTaggedUnionMembers topLevelType leavesOnly ts)
  in
    case t of
      Reference (NamedType n _ (TaggedUnion l)) -> recursiveCall t (Just (topLevelTypeOr t)) l
      Concrete (NamedType n _ (TaggedUnion l)) -> recursiveCall t (Just (topLevelTypeOr t)) l
      _ -> [taggedUnionMember (topLevelTypeOr t) t]

recursiveTaggedUnionMembers :: Maybe Type -> Bool -> [Type] -> [Value]
recursiveTaggedUnionMembers topLevelType leavesOnly ts = concat [recursiveTaggedUnionMember topLevelType leavesOnly t | t <- ts]

member :: (String, String, Type) -> Value
member (n, _, t) =
  object ["memberName" .= DT.pack n, "type" .= DT.pack (typeString t)]

members :: [(String, String, Type)] -> [Value]
members = map member

templateObjectForDecl :: String -> String -> NamedType -> Value
templateObjectForDecl namespace immRefCntNamespace (NamedType n _ t) =
  object
    $  [ "name" .= n
       , "namespace" .= namespace
       , "immRefCntNamespace" .= immRefCntNamespace
       ]
    ++ case t of
         Struct      l -> ["members" .= members l]
         Enum        l -> ["unionMembers" .= enumMembers l]
         TaggedUnion l -> [ "unionMembers" .= taggedUnionMembers l
                          , "recursiveUnionMembersLeavesOnly" .= recursiveTaggedUnionMembers Nothing True l
                          , "recursiveUnionMembers" .= recursiveTaggedUnionMembers Nothing False l
                          ]
         _             -> panic "Unexpected object in templateObjectForDecl."

wrapWithNamespace :: String -> String -> String
wrapWithNamespace namespace text = "namespace " ++ namespace ++ " {\n" ++ text ++ "}  // namespace " ++ namespace ++ "\n"

namedTypeDefinition :: String -> String -> Declaration -> String
namedTypeDefinition namespace immRefCntNamespace t = case t of
  ForwardDeclaration (NamedType n _ (Struct      _)) -> wrapWithNamespace namespace ("class " ++ n ++ ";\n")
  ForwardDeclaration (NamedType n _ (Enum        _)) -> wrapWithNamespace namespace ("class " ++ n ++ ";\n")
  ForwardDeclaration (NamedType n _ (TaggedUnion _)) -> wrapWithNamespace namespace ("class " ++ n ++ ";\n")
  ForwardDeclaration _ ->
    panic
      "Only forward declarations of Enum, Structs and TaggedUnions are supported."
  TypeDeclaration nt@(NamedType _ _ (Struct _)) ->
    DTL.unpack $ renderMustache structHTemplate $ templateObject nt
  TypeDeclaration nt@(NamedType n _ (Enum cs)) -> if isSimpleEnum cs
    then DTL.unpack $ renderMustache simpleEnumHTemplate $ templateObject nt
    else DTL.unpack $ renderMustache enumHTemplate $ templateObject nt
  TypeDeclaration nt@(NamedType _ _ (TaggedUnion _)) ->
    DTL.unpack $ renderMustache taggedUnionHTemplate $ templateObject nt
  TypeDeclaration (NamedType n _ t) ->
    wrapWithNamespace namespace ("using " ++ n ++ " = " ++ typeString t ++ ";\n")
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
constructorDefinition (TypeDeclaration nt@(NamedType n _ t@(Struct l))) =
  Just $ object ["name" .= DT.pack n]
constructorDefinition _ = Nothing

convertToImmRefCntFunctions :: String -> String -> Declaration -> Maybe String
convertToImmRefCntFunctions namespace immRefCntNamespace decl = case decl of
  (TypeDeclaration nt@(NamedType _ _ (Struct _))) ->
    Just $ DTL.unpack $ renderMustache structCCTemplate $ templateObject nt
  (TypeDeclaration nt@(NamedType _ _ (Enum cs))) -> if not . isSimpleEnum $ cs
    then Just $ DTL.unpack $ renderMustache enumCCTemplate $ templateObject nt
    else Nothing
  (TypeDeclaration nt@(NamedType _ _ (TaggedUnion _))) ->
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
