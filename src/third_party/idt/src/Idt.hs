module Idt
  ( Type(..)
  , NamedType(..)
  , NamedTypeList
  )
where

data NamedType = NamedType {name :: String, description :: String, idt_type :: Type}

data Type =
  Concrete NamedType |
  Reference NamedType |
  NamedPrimitive String |
  List Type |
  Tuple [Type] |
  FixedSizeArray Type Int |
  Struct [(String, String, Type)] |
  Enum [(String, String, [Type])] |
  TaggedUnion [Type]

type NamedTypeList = [NamedType]
