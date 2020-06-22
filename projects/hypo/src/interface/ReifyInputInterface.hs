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
    f32     = NamedPrimitive "f32"
    i32     = NamedPrimitive "i32"
    boolean = NamedPrimitive "boolean"

    vec n = NamedType ("Vec" ++ show n) $ FixedSizeArray f32 n
    cvec = Concrete . vec
    mat m n =
      NamedType ("Matrix" ++ show m ++ show n) $ FixedSizeArray f32 (m * n)
    cmat m n = Concrete $ mat m n
    polyline n = NamedType ("Polyline" ++ show n) $ List (cvec n)
    cpolyline       = Concrete . polyline

    polygon         = NamedType "Polygon" $ Struct [("path", cpolyline 2)]
    circle = NamedType "Circle" $ Struct [("radius", f32), ("center", cvec 2)]
    circleAsPolygon = NamedType "CircleAsPolygon"
      $ Struct [("circle", Concrete circle), ("num_points", i32)]
    rectangle =
      NamedType "Rectangle" $ Struct [("points", Tuple [cvec 2, cvec 2])]
    transform2 = NamedType "Transform2"
      $ Struct [("source", Concrete region2), ("transform", cmat 3 3)]
    union2 = NamedType "Union2" $ Struct [("regions", List $ Concrete region2)]
    intersection2 =
      NamedType "Intersection2" $ Struct [("regions", List $ Concrete region2)]
    difference2 = NamedType "Difference2"
      $ Struct [("a", Concrete region2), ("b", Concrete region2)]
    minkowskiSum2 =
      NamedType "MinkowskiSum2" $ Struct [("regions", List $ Concrete region2)]
    region2 = NamedType "Region2" $ TaggedUnion
      [ Reference polygon
      , Reference circleAsPolygon
      , Reference rectangle
      , Reference transform2
      , Reference union2
      , Reference intersection2
      , Reference difference2
      , Reference minkowskiSum2
      ]

    triangleList n =
      NamedType ("TriangleList" ++ show n)
        $ Struct
            [ ("vertices" , List $ cvec n)
            , ("triangles", List $ FixedSizeArray i32 3)
            ]
    sphere = NamedType "Sphere" $ Struct [("radius", f32), ("center", cvec 3)]
    octahedron = NamedType "Octahedron" $ Struct [("sphere", Concrete sphere)]
    icosahedron =
      NamedType "Icosahedron" $ Struct [("sphere", Concrete sphere)]
    sphereBased = NamedType "SphereBased" $ TaggedUnion
      [Reference octahedron, Reference icosahedron, Reference subdivideSphere]
    extrudeRegion2 = NamedType "Extrude" $ Struct
      [ ("source"    , Concrete region2)
      , ("transforms", List (cmat 4 3))
      , ("closed"    , boolean)
      ]
    transform3 = NamedType "Transform3"
      $ Struct [("source", Concrete region3), ("transform", cmat 4 4)]
    union3 = NamedType "Union3" $ Struct [("regions", List $ Concrete region3)]
    intersection3 =
      NamedType "Intersection3" $ Struct [("regions", List $ Concrete region3)]
    difference3 = NamedType "Difference3"
      $ Struct [("a", Concrete region3), ("b", Concrete region3)]
    subdivideMethod = NamedType "SubdivideMethod" $ Enum
      [
      --  ( "CatmullClark", []) -- TODO(issue #18): Produces a runtime error.
      -- , ("DooSabin", []),  -- TODO(issue #17): Compile problems in CGAL.
        ("Loop" , [])
      , ("Sqrt3", [])
      ]
    subdivide = NamedType "Subdivide" $ Struct
      [ ("source"    , Concrete region3)
      , ("method"    , Concrete subdivideMethod)
      , ("iterations", i32)
      ]
    subdivideSphere = NamedType "SubdivideSphere"
      $ Struct [("source", Concrete sphereBased), ("iterations", i32)]
    minkowskiSum3 =
      NamedType "MinkowskiSum3" $ Struct [("regions", List $ Concrete region3)]
    region3 = NamedType "Region3" $ TaggedUnion
      [ Reference (triangleList 3)
      , Reference extrudeRegion2
      , Reference transform3
      , Reference union3
      , Reference intersection3
      , Reference difference3
      , Reference icosahedron
      , Reference octahedron
      , Reference subdivide
      , Reference subdivideSphere
      , Reference minkowskiSum3
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
