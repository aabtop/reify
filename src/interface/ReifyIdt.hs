module ReifyIdt
  ( reifyIdt
  )
where

import           Idt

reifyIdt :: NamedTypeList
reifyIdt =
  let
    float = NamedPrimitive "f32"
    vec n = NamedType ("Vec" ++ show n) $ Tuple $ replicate n float
    cvec = Concrete . vec
    mat m n =
      NamedType ("Matrix" ++ show m ++ show n) $ Tuple (replicate (m * n) float)
    cmat m n = Concrete $ mat m n

    circle =
      NamedType "Circle" $ Struct [("radius", float), ("center", cvec 2)]
    rectangle = NamedType "Rectangle"
      $ Struct [("width", float), ("height", float), ("top_left", cvec 2)]
    mesh2 = NamedType "Mesh2" $ Enum
      [ ("CircleAsMesh"   , [Reference circle])
      , ("RectangleAsMesh", [Reference rectangle])
      ]

    extrudeMesh2 = NamedType "ExtrudeMesh2"
      $ Struct [("source", Concrete mesh2), ("path", Tuple [cvec 3, cvec 3])]
    transformMesh3 = NamedType "TransformMesh3"
      $ Struct [("source", Concrete mesh3), ("transform", cmat 4 4)]
    mesh3 = NamedType "Mesh3" $ Enum
      [ ("ExtrudeMesh2AsMesh"  , [Reference extrudeMesh2])
      , ("TransformMesh3AsMesh", [Reference transformMesh3])
      , ("MeshUnion"           , [List $ Concrete mesh3])
      ]
  in
    [vec 2, vec 3, mat 4 4, mesh2, mesh3]
