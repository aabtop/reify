module TargetTypeScript
  ( toTypeScriptSourceCode
  )
where

import           Data.List
import           Idt
import           IdtProcessing

enumConstructorString :: String -> (String, [Type]) -> String
enumConstructorString enumName (n, ts) =
  let structConstructors = ("__kind: " ++ "\"" ++ n ++ "\"")
        : map (\x -> x ++ ": " ++ x) (enumTypeNames ts)
  in  "export function "
        ++ n
        ++ "("
        ++ intercalate ", " (enumTypesToParameters ts)
        ++ "): "
        ++ enumName
        ++ "{\n  return { "
        ++ intercalate ", " structConstructors
        ++ "};\n}"

enumTypeNames ts = map (("p" ++) . show) [0 .. length ts - 1]

enumTypesToParameters :: [Type] -> [String]
enumTypesToParameters ts =
  map (\(n, t) -> n ++ ": " ++ typeString t) (zip (enumTypeNames ts) ts)

enumString :: (String, [Type]) -> String
enumString (n, ts) =
  "{__kind: \""
    ++ n
    ++ "\"; "
    ++ unwords (map (++ ";") (enumTypesToParameters ts))
    ++ " }"

typeString :: Type -> String
typeString (Concrete       (NamedType n _)) = n
typeString (Reference      (NamedType n _)) = n
typeString (NamedPrimitive n              ) = case n of
  "String" -> "string"
  "float"  -> "number"
  _        -> n
typeString (List  t) = typeString t ++ "[]"
typeString (Tuple l) = "[" ++ intercalate ", " (map typeString l) ++ "]"
typeString (Enum  l) = intercalate " | " (map enumString l)
typeString (Struct l) =
  "{ " ++ concatMap (\(n, t) -> n ++ ": " ++ typeString t ++ "; ") l ++ " }"

namedTypeDefinition :: Declaration -> String
namedTypeDefinition t = case t of
  ForwardDeclaration _ -> ""
  TypeDeclaration (NamedType n t@(Enum l)) ->
    concatMap (\x -> enumConstructorString n x ++ "\n") l
      ++ ("export type " ++ n ++ " = " ++ typeString t ++ ";\n")
  TypeDeclaration (NamedType n t) ->
    "export type " ++ n ++ " = " ++ typeString t ++ ";\n"

toTypeScriptSourceCode :: DeclarationSequence -> String
toTypeScriptSourceCode decls =
  "namespace reify {\n"
    ++ intercalate "\n" (map namedTypeDefinition decls)
    ++ "\n}  // namespace reify"
