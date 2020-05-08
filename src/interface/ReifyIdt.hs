module ReifyIdt
  ( reifyIdt
  , namespace
  )
where

import           Idt

reifyIdt :: NamedTypeList
reifyIdt =
  let
    float = NamedPrimitive "f32"
    vec n = NamedType ("Vec" ++ show n) $ FixedSizeArray float n
    cvec = Concrete . vec
    mat m n =
      NamedType ("Matrix" ++ show m ++ show n) $ FixedSizeArray float (m * n)
    cmat m n = Concrete $ mat m n

    circle =
      NamedType "Circle" $ Struct [("radius", float), ("center", cvec 2)]
    rectangle = NamedType "Rectangle"
      $ Struct [("width", float), ("height", float), ("top_left", cvec 2)]
    mesh2 =
      NamedType "Mesh2" $ TaggedUnion [Reference circle, Reference rectangle]

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

namespace :: String
namespace = "reify"
