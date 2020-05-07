module TargetHaskell
  ( toHaskellSourceCode
  )
where

import           Data.List
import           Panic

import           Idt
import           IdtProcessing

typeString :: Type -> String
typeString (Concrete       (NamedType n _)) = n
typeString (Reference      (NamedType n _)) = n
typeString (NamedPrimitive n              ) = case n of
  "string" -> "String"
  "f32"    -> "float"
  "i32"    -> "Int"
  _        -> n
typeString (List t) = "[" ++ typeString t ++ "]"
typeString (Tuple l) = "(" ++ intercalate ", " (map typeString l) ++ ")"
typeString (FixedSizeArray t s) = typeString (List t)
typeString (Struct l) =
  "{ "
    ++ intercalate ", " (map (\(n, t) -> n ++ " :: " ++ typeString t) l)
    ++ " }"
typeString (Enum l) = intercalate
  " | "
  (map (\(n, p) -> n ++ " " ++ unwords (map typeString p)) l)
typeString (TaggedUnion l) =
  panic "TaggedUnions may only be referenced as named types."

taggedUnionConstructor :: String -> Type -> String
taggedUnionConstructor tun t = case t of
  Concrete  nt -> processNamedType nt
  Reference nt -> processNamedType nt
  _            -> panic "TaggedUnions may only be passed named types."
  where processNamedType (NamedType n _) = n ++ "As" ++ tun ++ typeString t

namedTypeDefinition :: Declaration -> String
namedTypeDefinition t = case t of
  ForwardDeclaration _ -> ""
  TypeDeclaration (NamedType n t@(Struct l)) ->
    "data " ++ n ++ " = " ++ n ++ " " ++ typeString t
  TypeDeclaration (NamedType n t@(Enum l)) ->
    "data " ++ n ++ " = " ++ typeString t
  TypeDeclaration (NamedType n (TaggedUnion l)) ->
    "data " ++ n ++ " = " ++ intercalate " | "
                                         (map (taggedUnionConstructor n) l)
  TypeDeclaration (NamedType n t) -> "type " ++ n ++ " = " ++ typeString t

toHaskellSourceCode :: DeclarationSequence -> String
toHaskellSourceCode decls = intercalate "\n" (map namedTypeDefinition decls)
