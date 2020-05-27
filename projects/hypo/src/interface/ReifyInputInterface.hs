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
      [("left", float), ("top", float), ("right", float), ("bottom", float)]
    union2 = NamedType "Union2" $ Struct [("regions", List $ Concrete region2)]
    intersection2 =
      NamedType "Intersection2" $ Struct [("regions", List $ Concrete region2)]
    difference2 = NamedType "Difference2"
      $ Struct [("a", Concrete region2), ("b", Concrete region2)]
    region2 = NamedType "Region2" $ TaggedUnion
      [ Reference circleAsPolygon
      , Reference rectangle
      , Reference union2
      , Reference intersection2
      , Reference difference2
      ]

    sphere =
      NamedType "Sphere" $ Struct [("radius", float), ("center", cvec 3)]
    icosahedron =
      NamedType "Icosahedron" $ Struct [("sphere", Concrete sphere)]
    extrudeRegion2 = NamedType "Extrude" $ Struct
      [("source", Concrete region2), ("transforms", Tuple [cmat 4 3, cmat 4 3])]
    transform3 = NamedType "Transform3"
      $ Struct [("source", Concrete region3), ("transform", cmat 4 4)]
    union3 = NamedType "Union3" $ Struct [("regions", List $ Concrete region3)]
    intersection3 =
      NamedType "Intersection3" $ Struct [("regions", List $ Concrete region3)]
    difference3 = NamedType "Difference3"
      $ Struct [("a", Concrete region3), ("b", Concrete region3)]
    region3 = NamedType "Region3" $ TaggedUnion
      [ Reference extrudeRegion2
      , Reference transform3
      , Reference union3
      , Reference intersection3
      , Reference difference3
      , Reference icosahedron
      ]
  in
    [vec 2, vec 3, mat 4 4, mat 4 3, mat 3 3, region2, region3]

-- The namespace applied to all interfaces generated from |idt| above.
namespace :: String
namespace = "hypo"

-- The directory containing a "lib.ts" file which defines the interface.
-- Within this file one may import reify_generated_interface to access the
-- TypeScript types generated from the definition of |idt| above.
typescriptLibDir :: String
typescriptLibDir = "./typescript"
