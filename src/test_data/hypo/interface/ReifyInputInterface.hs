module ReifyInputInterface
  ( idt
  , namespace
  , typescriptLibDir
  )
where

import           Idt

-- Defines the types that are to form the language of communication between
-- C++, V8, and the TypeScript compiler.
idt :: NamedTypeList
idt =
  let
    float = NamedPrimitive "f32"
    int   = NamedPrimitive "i32"
    vec n = NamedType ("Vec" ++ show n) $ FixedSizeArray float n
    cvec = Concrete . vec
    mat m n =
      NamedType ("Matrix" ++ show m ++ show n) $ FixedSizeArray float (m * n)
    cmat m n = Concrete $ mat m n

    circle =
      NamedType "Circle" $ Struct [("radius", float), ("center", cvec 2)]
    circleAsPolygon = NamedType "CircleAsPolygon"
      $ Struct [("circle", Concrete circle), ("num_points", int)]
    rectangle = NamedType "Rectangle" $ Struct
      [("left", float), ("bottom", float), ("right", float), ("top", float)]
    region2 = NamedType "Region2"
      $ TaggedUnion [Reference circleAsPolygon, Reference rectangle]

    extrudeRegion2 = NamedType "ExtrudeRegion2" $ Struct
      [("source", Concrete region2), ("path", Tuple [cvec 3, cvec 3])]
    transformRegion3 = NamedType "TransformRegion3"
      $ Struct [("source", Concrete region3), ("transform", cmat 4 4)]
    region3Union =
      NamedType "Region3Union" $ Struct [("regions", List $ Concrete region3)]
    region3 = NamedType "Region3" $ TaggedUnion
      [ Reference extrudeRegion2
      , Reference transformRegion3
      , Reference region3Union
      ]
  in
    [vec 2, vec 3, mat 4 4, region2, region3]

-- The namespace applied to all interfaces generated from |idt| above.
namespace :: String
namespace = "hypo"

-- The directory containing a "lib.ts" file which defines the interface.
-- Within this file one may import reify_generated_interface to access the
-- TypeScript types generated from the definition of |idt| above.
typescriptLibDir :: String
typescriptLibDir = "./typescript"
