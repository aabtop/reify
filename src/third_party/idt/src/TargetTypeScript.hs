module TargetTypeScript
  ( toTypeScriptSourceCode
  )
where

import           Data.List
import           Panic

import           Idt
import           IdtProcessing

enumConstructorFunctionString :: String -> (String, String, [Type]) -> String
enumConstructorFunctionString enumName (cn, tn, ts) =
  let structConstructors = ("__kind: " ++ "'" ++ tn ++ "'")
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
enumConstructorTypeString (cn, tn, ts) =
  let structMemberDefs =
          ("__kind: " ++ "'" ++ tn ++ "'")
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

taggedUnionTypeName :: Type -> String
taggedUnionTypeName t = case t of
  Concrete  (NamedType n (Struct      _)) -> n
  Concrete  (NamedType n (TaggedUnion _)) -> n
  Reference (NamedType n (Struct      _)) -> n
  Reference (NamedType n (TaggedUnion _)) -> n
  _ ->
    panic
      "TaggedUnions may only be passed named types of Structs and other TaggedUnions."

typeString :: Type -> String
typeString (Concrete       (NamedType n (Struct _))) = n ++ "Params"
typeString (Concrete       (NamedType n _         )) = n
typeString (Reference      (NamedType n _         )) = n
typeString (NamedPrimitive n                       ) = case n of
  "string" -> "string"
  "f32"    -> "number"
  "i32"    -> "number"
  _        -> n
typeString (List  t) = typeString t ++ "[]"
typeString (Tuple l) = "[" ++ intercalate ", " (map typeString l) ++ "]"
typeString (FixedSizeArray t s) | s <= 32   = typeString (Tuple $ replicate s t)
                                | otherwise = typeString (List t)
typeString (Struct l) =
  "{ " ++ concatMap (\(n, t) -> n ++ ": " ++ typeString t ++ "; ") l ++ " }"
typeString (Enum l) = panic "Enums may only be referenced as named types."
typeString (TaggedUnion ts) =
  panic "TaggedUnions may only be referenced as named types."

enumString :: String -> [(String, String, [Type])] -> String
enumString n cs =
  concatMap (\c -> enumConstructorFunctionString n c ++ "\n") cs
    ++ "export type "
    ++ n
    ++ " = "
    ++ intercalate " | " (map enumConstructorTypeString cs)
    ++ ";\n"

namedTypeDefinition :: Declaration -> String
namedTypeDefinition t = case t of
  ForwardDeclaration _ -> ""
  TypeDeclaration (NamedType n t@(Enum l)) ->
    enumString n [ (cn, cn, cts) | (cn, cts) <- l ]
  TypeDeclaration (NamedType n t@(TaggedUnion tuts)) ->
    "export type "
      ++ n
      ++ " = "
      ++ intercalate " | " (map taggedUnionTypeName tuts)
      ++ ";\n"
  TypeDeclaration (NamedType n t@(Struct l)) ->
    ("export type " ++ n ++ "Params = " ++ typeString t ++ ";\n")
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
      ++ (  "export function "
         ++ n
         ++ "(x: "
         ++ n
         ++ "Params): "
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
  TypeDeclaration (NamedType n t) ->
    "export type " ++ n ++ " = " ++ typeString t ++ ";\n"

preamble =
  "\
\interface __KindObject {\
\  __kind: string;\
\}\
\\
\function withKind<T>(x: T, kind: string): T & __KindObject {\
\  if (x.hasOwnProperty('__kind')) {\
\    return x as T & __KindObject;\
\  } else {\
\    return Object.assign({__kind: kind}, x);\
\  }\
\}"

toTypeScriptSourceCode :: DeclarationSequence -> String
toTypeScriptSourceCode decls =
  preamble ++ intercalate "\n" (map namedTypeDefinition decls)
