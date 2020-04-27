module TargetHaskell
  ( toHaskellSourceCode
  )
where

import           Data.List

import           Idt
import           IdtProcessing

typeString :: Type -> String
typeString (Concrete       (NamedType n _)) = n
typeString (Reference      (NamedType n _)) = n
typeString (NamedPrimitive n              ) = n
typeString (List           t              ) = "[" ++ typeString t ++ "]"
typeString (Tuple l) = "(" ++ intercalate ", " (map typeString l) ++ ")"
typeString (Enum l) =
  intercalate " | " (map (\(n, p) -> n ++ " " ++ unwords (map typeString p)) l)
typeString (Struct l) =
  "{ "
    ++ intercalate ", " (map (\(n, t) -> n ++ " :: " ++ typeString t) l)
    ++ " }"

namedTypeDefinition :: Declaration -> String
namedTypeDefinition t = case t of
  ForwardDeclaration _ -> ""
  TypeDeclaration (NamedType n t@(Enum l)) ->
    "data " ++ n ++ " = " ++ typeString t
  TypeDeclaration (NamedType n t@(Struct l)) ->
    "data " ++ n ++ " = " ++ n ++ " " ++ typeString t
  TypeDeclaration (NamedType n t) -> "type " ++ n ++ " = " ++ typeString t

toHaskellSourceCode :: DeclarationSequence -> String
toHaskellSourceCode decls = intercalate "\n" (map namedTypeDefinition decls)
