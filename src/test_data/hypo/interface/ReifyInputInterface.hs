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
    rectangle = NamedType "Rectangle"
      $ Struct [("width", float), ("height", float), ("top_left", cvec 2)]
    mesh2 = NamedType "Mesh2"
      $ TaggedUnion [Reference circleAsPolygon, Reference rectangle]

    extrudeMesh2 = NamedType "ExtrudeMesh2"
      $ Struct [("source", Concrete mesh2), ("path", Tuple [cvec 3, cvec 3])]
    transformMesh3 = NamedType "TransformMesh3"
      $ Struct [("source", Concrete mesh3), ("transform", cmat 4 4)]
    mesh3Union =
      NamedType "Mesh3Union" $ Struct [("meshes", List $ Concrete mesh3)]
    mesh3 = NamedType "Mesh3" $ TaggedUnion
      [Reference extrudeMesh2, Reference transformMesh3, Reference mesh3Union]
  in
    [vec 2, vec 3, mat 4 4, mesh2, mesh3]

-- The namespace applied to all interfaces generated from |idt| above.
namespace :: String
namespace = "hypo"

-- The directory containing a "lib.ts" file which defines the interface.
-- Within this file one may import reify_generated_interface to access the
-- TypeScript types generated from the definition of |idt| above.
typescriptLibDir :: String
typescriptLibDir = "./typescript"
