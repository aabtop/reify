module TargetTypeScript
  ( toTypeScriptSourceCode
  )
where

import           Data.List
import           Panic

import           Idt
import           IdtProcessing

tsdocComment :: String -> String
tsdocComment x = "/** " ++ x ++ " */"

enumConstructorFunctionString :: String -> (String, String, [Type]) -> String
enumConstructorFunctionString enumName (cn, _, ts) =
  let structConstructors = ("__kind: " ++ "'" ++ cn ++ "'")
        : map (\x -> x ++ ": " ++ x) (enumTypeNames ts)
  in  "export function "
        ++ cn
        ++ "("
        ++ intercalate ", " (enumTypesToParameters ts)
        ++ "): "
        ++ enumName
        ++ " {\n  return { "
        ++ intercalate ", " structConstructors
        ++ "};\n}"

enumConstructorTypeString :: (String, String, [Type]) -> String
enumConstructorTypeString (cn, _, ts) =
  let structMemberDefs =
          ("__kind: " ++ "'" ++ cn ++ "'")
            : zipWith (\n t -> n ++ ": " ++ typeString t) (enumTypeNames ts) ts
  in  "{" ++ intercalate "; " structMemberDefs ++ "}"

enumTypeNames ts = map (("p" ++) . show) [0 .. length ts - 1]

enumTypesToParameters :: [Type] -> [String]
enumTypesToParameters ts =
  map (\(n, t) -> n ++ ": " ++ typeString t) (zip (enumTypeNames ts) ts)

enumEntryString :: (String, [Type]) -> String
enumEntryString (n, ts) =
  "{__kind: '"
    ++ n
    ++ "'; "
    ++ unwords (map (++ ";") (enumTypesToParameters ts))
    ++ " }"

simpleEnumString :: String -> String -> [(String, String)] -> String
simpleEnumString n c ens =
  tsdocComment c ++ "\n"
    ++ "export const enum "
    ++ n
    ++ " {\n"
    ++ unlines [ "  " ++ tsdocComment ec ++ "\n  " ++ en ++ "," | (en, ec) <- ens ]
    ++ "}\n"

enumString :: String -> String -> [(String, String, [Type])] -> String
enumString n c cs = if isSimpleEnum cs
  then simpleEnumString n c (map (\(en, ec, _) -> (en, ec)) cs)
  else
    concatMap (\co -> enumConstructorFunctionString n co ++ "\n") cs
    ++ tsdocComment c
    ++ "\n"
    ++ "export type "
    ++ n
    ++ " = "
    ++ intercalate " | " (map enumConstructorTypeString cs)
    ++ ";\n"

taggedUnionTypeName :: Type -> String
taggedUnionTypeName t = case t of
  Concrete  (NamedType n _ (Struct      _)) -> n
  Concrete  (NamedType n _ (TaggedUnion _)) -> n
  Reference (NamedType n _ (Struct      _)) -> n
  Reference (NamedType n _ (TaggedUnion _)) -> n
  _ ->
    panic
      "TaggedUnions may only be passed named types of Structs and other TaggedUnions."

typeString :: Type -> String
typeString (Concrete       (NamedType n _ (Struct _))) = n ++ "Params"
typeString (Concrete       (NamedType n _ _         )) = n
typeString (Reference      (NamedType n _ _         )) = n
typeString (NamedPrimitive n                         ) = case n of
  "string"  -> "string"
  "f32"     -> "number"
  "i32"     -> "number"
  "boolean" -> "boolean"
  _         -> n
typeString (List  t) = typeString t ++ "[]"
typeString (Tuple l) = "[" ++ intercalate ", " (map typeString l) ++ "]"
typeString (FixedSizeArray t s) | s <= 32   = typeString (Tuple $ replicate s t)
                                | otherwise = typeString (List t)
typeString (Struct l) =
  "{ "
    ++ concatMap
         (\(n, c, t) ->
           "\n  " ++ tsdocComment c ++ "\n  " ++ n ++ ": " ++ typeString t ++ ";"
         )
         l
    ++ "\n}"
typeString (Enum l) = panic "Enums may only be referenced as named types."
typeString (TaggedUnion ts) =
  panic "TaggedUnions may only be referenced as named types."

namedTypeDefinition :: Declaration -> String
namedTypeDefinition t = case t of
  ForwardDeclaration _                          -> ""
  TypeDeclaration    (NamedType n c t@(Enum l)) -> enumString n c l
  TypeDeclaration (NamedType n c t@(TaggedUnion tuts)) ->
    tsdocComment c ++ "\n"
      ++ "export type "
      ++ n
      ++ " = "
      ++ intercalate " | " (map taggedUnionTypeName tuts)
      ++ ";\n"
  TypeDeclaration (NamedType n c t@(Struct l)) ->
    (  tsdocComment c ++ "\n"
      ++ "export type "
      ++ n
      ++ "Params = "
      ++ typeString t
      ++ ";\n"
      )
      ++ (  "interface "
         ++ n
         ++ "WithKind extends "
         ++ n
         ++ "Params { __kind: '"
         ++ n
         ++ "'; }\n"
         )
      ++ (  "export interface "
         ++ n
         ++ " extends Readonly<"
         ++ n
         ++ "WithKind> {}\n"
         )
      ++ (  tsdocComment c ++ "\n"
         ++ "export function "
         ++ n
         ++ "(x: "
         ++ typeString t
         ++ "): "
         ++ n
         ++ " {\n"
         ++ (  "  return withInternalField(withKind(x, '"
            ++ n
            ++ "')) as "
            ++ n
            ++ ";\n"
            )
         ++ "};\n"
         )
  TypeDeclaration (NamedType n c t) ->
    tsdocComment c ++ "\n" ++ "export type " ++ n ++ " = " ++ typeString t ++ ";\n"

preamble =
  "\
\declare function withInternalField(x: Object): Object;\n\
\\n\
\interface __KindObject {\n\
\  __kind: string;\n\
\}\n\
\\n\
\function withKind<T extends object>(x: T, kind: string): T & __KindObject {\n\
\  if (x.hasOwnProperty('__kind')) {\n\
\    return x as T & __KindObject;\n\
\  } else {\n\
\    return Object.assign({__kind: kind}, x);\n\
\  }\n\
\}\n\
\\n"

toTypeScriptSourceCode :: DeclarationSequence -> String
toTypeScriptSourceCode decls =
  preamble ++ intercalate "\n" (map namedTypeDefinition decls)
