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
  FixedSizeArray Type Int |
  Struct [(String, Type)] |
  Enum [(String, [Type])] |
  TaggedUnion [Type]

type NamedTypeList = [NamedType]
