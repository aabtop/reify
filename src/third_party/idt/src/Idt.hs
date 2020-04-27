module Idt
  ( Type(..)
  , NamedType(..)
  , NamedTypeList
  )
where

data NamedType = NamedType {name :: String, idt_type :: Type}

data Type =
  Concrete NamedType |
  Reference NamedType |
  NamedPrimitive String |
  List Type |
  Tuple [Type] |
  Enum [(String, [Type])] |
  Struct [(String, Type)]

type NamedTypeList = [NamedType]
