module IdtIdt
  ( idtIdt
  )
where

import           Idt

idtIdt :: NamedTypeList
idtIdt =
  let
    string     = NamedPrimitive "string"
    i32        = NamedPrimitive "i32"
    named_type = Concrete $ NamedType
      "NamedType"
      "A type with a name associated with it."
      (Struct
        [ ("name", "Alias for the type.", string)
        , ( "idt_type"
          , "A reference to the type definition to be aliased."
          , idt_type_ref
          )
        ]
      )
    idt_type = NamedType
      "Type"
      "The definition of a type."
      (Enum
        [ ( "Concrete"
          , "A concrete type, expressing that the type data is part of whatever references it."
          , [named_type]
          )
        , ( "Reference"
          , "A referenced type, expressing that the type data is linked from whatever references it."
          , [named_type]
          )
        , ( "NamedPrimitive"
          , "A primitive type with a name aliased to it."
          , [string]
          )
        , ("List", "A list of objects all of the same type.", [idt_type_ref])
        , ( "Tuple"
          , "An indexable sequence of objects, each with their own types."
          , [List idt_type_ref]
          )
        , ( "FixedSizeArray"
          , "A list of a pre-specified number of objects, all of the same type."
          , [idt_type_ref, i32]
          )
        , ( "Struct"
          , "A sequence of named data members of varying types."
          , [List (Tuple [string, idt_type_ref])]
          )
        , ( "Enum"
          , "A union type that could be any of the set of data constructors, and the described data constructors can only be this enum."
          , [List (Tuple [string, List idt_type_ref])]
          )
        , ( "TaggedUnion"
          , "A union type that could be any of the specified other types."
          , [List idt_type_ref]
          )
        ]
      )
    idt_type_ref = Reference idt_type
    named_type_list =
      NamedType "NamedTypeList" "A list of named types." (List named_type)
  in
    [named_type_list]
