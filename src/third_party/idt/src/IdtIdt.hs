module IdtIdt
  ( idtIdt
  )
where

import           Idt

idtIdt :: NamedTypeList
idtIdt =
  let string     = NamedPrimitive "string"
      i32        = NamedPrimitive "i32"
      named_type = Concrete $ NamedType
        "NamedType"
        (Struct [("name", string), ("idt_type", idt_type_ref)])
      idt_type = NamedType
        "Type"
        (Enum
          [ ("Concrete"      , [named_type])
          , ("Reference"     , [named_type])
          , ("NamedPrimitive", [string])
          , ("List"          , [idt_type_ref])
          , ("Tuple"         , [List idt_type_ref])
          , ("FixedSizeArray", [idt_type_ref, i32])
          , ("Enum", [List (Tuple [string, List idt_type_ref])])
          , ("Struct"        , [List (Tuple [string, idt_type_ref])])
          ]
        )
      idt_type_ref    = Reference idt_type
      named_type_list = NamedType "NamedTypeList" (List named_type)
  in  [named_type_list]
