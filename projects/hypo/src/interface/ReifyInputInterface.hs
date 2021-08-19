module ReifyInputInterface
  ( idt
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

    vec n =
      NamedType
          ("Vec" ++ show n)
          ("A " ++ show n ++ "-dimensional cartesian coordinate vector.")
        $ FixedSizeArray f32 n
    cvec = Concrete . vec
    mat m n =
      NamedType ("Matrix" ++ show m ++ show n)
                ("A " ++ show m ++ "x" ++ show n ++ " matrix.")
        $ FixedSizeArray f32 (m * n)
    cmat m n = Concrete $ mat m n
    polyline n =
      NamedType
          ("Polyline" ++ show n)
          (  "A sequence of connected line segments in "
          ++ show n
          ++ " dimensions."
          )
        $ List (cvec n)
    cpolyline = Concrete . polyline
    box n =
      NamedType
          ("Box" ++ show n)
          (  "Two points describing the extreminites of an axis aligned box in "
          ++ show n
          ++ " dimensions."
          )
        $ Struct
            [ ( "corners"
              , "2 corners defining the axis-aligned box's extremities."
              , FixedSizeArray (cvec n) 2
              )
            ]

    polygon = NamedType "Polygon" "A closed polygon." $ Struct
      [ ( "path"
        , "A polyline describing the boundary of the polygon, with the last described vertex connected to the first vertex."
        , cpolyline 2
        )
      ]
    circle =
      NamedType "Circle" "A circle with arbitrary radius and center-point."
        $ Struct
            [ ("radius", "The circle's radius."           , f32)
            , ("center", "The center-point of the circle.", cvec 2)
            ]
    circleAsPolygon =
      NamedType "CircleAsPolygon" "A circle approximated by line segments."
        $ Struct
            [ ( "circle"
              , "The circle that is being approximated."
              , Concrete circle
              )
            , ( "num_points"
              , "The number of points in the resulting polygon.  The more points, the closer the shape will resemble the circle."
              , i32
              )
            ]
    transform2 =
      NamedType "Transform2" "An affine transformed 2D region." $ Struct
        [ ("source", "The input 2D region to be transformed.", Concrete region2)
        , ( "transform"
          , "The linear affine transform to apply to the region."
          , cmat 3 3
          )
        ]
    union2 =
      NamedType
          "Union2"
          "A region resulting from the union of 1 or more other regions.  A point is in the resulting region if it is in any of the input regions."
        $ Struct
            [ ( "regions"
              , "The list of input regions to union together."
              , List $ Concrete region2
              )
            ]
    intersection2 =
      NamedType
          "Intersection2"
          "A region resulting from the intersection of 1 or more input regions.  A point is in the resulting region if it is in all of the input regions."
        $ Struct
            [ ( "regions"
              , "The list of input regions to intersect with each other."
              , List $ Concrete region2
              )
            ]
    difference2 =
      NamedType
          "Difference2"
          "The difference between 2 regions.  A point is in the resulting set if it is in `a` but not in `b`."
        $ Struct
            [ ( "a"
              , "The positive region, within which a point must exist to also exist in the output region."
              , Concrete region2
              )
            , ( "b"
              , "The negative region, within which if a point exists in `b`, it will not exist in the output region."
              , Concrete region2
              )
            ]
    minkowskiSum2 =
      NamedType
          "MinkowskiSum2"
          "Accumulates the input regions by, for each pair `a` and `b`, taking the union of region `b` translated to every point of region `a`. This is useful for creating rounded corners for example, by taking the Minkowski sum of a region with a circle/sphere."
        $ Struct
            [ ( "regions"
              , "The list of input regions, to be accumulated over the Minkowski Sum operation."
              , List $ Concrete region2
              )
            ]
    region2 =
      NamedType "Region2" "An arbitrary shape representing a 2D area."
        $ TaggedUnion
            [ Reference polygon
            , Reference circleAsPolygon
            , Reference (box 2)
            , Reference transform2
            , Reference union2
            , Reference intersection2
            , Reference difference2
            , Reference minkowskiSum2
            ]

    boundary2 =
      NamedType
          "Boundary2"
          "Returns an object representing the 1D polygonal boundary of a 2D polygonal region. This will be a closed polygon."
        $ Struct
            [ ( "region"
              , "The 2D region to find the boundary of."
              , Concrete region2)]

    triangleList n =
      NamedType ("TriangleList" ++ show n)
                ("A list of " ++ show n ++ "-dimensional triangles.")
        $ Struct
            [ ( "vertices"
              , "The list of vertices, which may be indexed by the list of triangles."
              , List $ cvec n
              )
            , ( "triangles"
              , "The list of triangles, defined by indices into the list of vertices."
              , List $ FixedSizeArray i32 3
              )
            ]
    sphere = NamedType "Sphere" "A 3D sphere." $ Struct
      [ ("radius", "The sphere's radius."           , f32)
      , ("center", "The center-point of the sphere.", cvec 3)
      ]
    octahedron =
      NamedType "Octahedron"
                "The 8-sided platonic solid, approximating the given sphere."
        $ Struct
            [ ( "sphere"
              , "A sphere that the octahedron will approximate."
              , Concrete sphere
              )
            ]
    icosahedron =
      NamedType "Icosahedron"
                "The 20-sided platonic solid, approximating the given sphere."
        $ Struct
            [ ( "sphere"
              , "A sphere that the icosahedron will approximate."
              , Concrete sphere
              )
            ]
    sphereBased =
      NamedType "SphereBased"
                "Represents the set of region3 objects based off of spheres."
        $ TaggedUnion
            [ Reference octahedron
            , Reference icosahedron
            , Reference subdivideSphere
            ]
    extrudeRegion2 =
      NamedType
          "Extrude"
          "Results in a `region3` derived from connecting together the edges of all the `region2` transformed into 3D for the given sequence of transforms. If `closed` is true, the last transform will have its edges connected back to the first transform."
        $ Struct
            [ ( "source"
              , "The `region2` that acts as the source 2D shape."
              , Concrete region2
              )
            , ( "transforms"
              , "The sequence of 2D->3D transforms which position each segment of the resulting `region3`."
              , List (cmat 4 3)
              )
            , ( "closed"
              , "If true, the last segment will be connected back to the first segment."
              , boolean
              )
            ]
    transform3 =
      NamedType "Transform3" "An affine transformed 3D region." $ Struct
        [ ("source", "The input 3D region to be transformed.", Concrete region3)
        , ( "transform"
          , "The linear affine transform to apply to the region."
          , cmat 4 4
          )
        ]
    union3 =
      NamedType
          "Union3"
          "A region resulting from the union of 1 or more other regions.  A point is in the resulting region if it is in any of the input regions."
        $ Struct
            [ ( "regions"
              , "The list of input regions to union together."
              , List $ Concrete region3
              )
            ]
    intersection3 =
      NamedType
          "Intersection3"
          "A region resulting from the intersection of 1 or more input regions.  A point is in the resulting region if it is in all of the input regions."
        $ Struct
            [ ( "regions"
              , "The list of input regions to intersect with each other."
              , List $ Concrete region3
              )
            ]
    difference3 =
      NamedType
          "Difference3"
          "The difference between 2 regions.  A point is in the resulting set if it is in `a` but not in `b`."
        $ Struct
            [ ( "a"
              , "The positive region, within which a point must exist to also exist in the output region."
              , Concrete region3
              )
            , ( "b"
              , "The negative region, within which if a point exists in `b`, it will not exist in the output region."
              , Concrete region3
              )
            ]
    minkowskiSum3 =
      NamedType
          "MinkowskiSum3"
          "Accumulates the input regions by, for each pair `a` and `b`, taking the union of region `b` translated to every point of region `a`. This is useful for creating rounded corners for example, by taking the Minkowski sum of a region with a circle/sphere."
        $ Struct
            [ ( "regions"
              , "The list of input regions, to be accumulated over the Minkowski Sum operation."
              , List $ Concrete region3
              )
            ]
    subdivideMethod =
      NamedType "SubdivideMethod"
                "Describes different methods of subdividing a 3D surface."
        $ Enum
            [
      --  ( "CatmullClark", []) -- TODO(issue #18): Produces a runtime error.
      -- , ("DooSabin", []),  -- TODO(issue #17): Compile problems in CGAL.
              ("Loop" , "The Loop subdivision method." , [])
            , ("Sqrt3", "The Sqrt3 subdivision method.", [])
            ]
    subdivide =
      NamedType
          "Subdivide"
          "Subdivides the surface of a 3D region according to the specified subdivision method. Note that this function is known to be unstable and can cause a crash if given bad inputs."
        $ Struct
            [ ( "source"
              , "The input 3D shape, whose surface will be subdivided and the result converted back into a new `region3`."
              , Concrete region3
              )
            , ( "method"
              , "Describes which algorithm to use for subdividing the surface."
              , Concrete subdivideMethod
              )
            , ( "iterations"
              , "The number of times the subdivision method should be iterated.  The higher the number, the smoother the output region."
              , i32
              )
            ]
    subdivideSphere =
      NamedType
          "SubdivideSphere"
          "A subdivision method optimized for sphere-approximating shapes.  After each subdivision iteration, all resulting poitns will be projected onto the approximated sphere."
        $ Struct
            [ ("source", "The input sphere-like object.", Concrete sphereBased)
            , ( "iterations"
              , "The number of times the subdivision method should be iterated.  The higher the number, the smoother the output region."
              , i32
              )
            ]
    region3 =
      NamedType "Region3" "An arbitrary shape representing a 3D volume."
        $ TaggedUnion
            [ Reference (triangleList 3)
            , Reference (box 3)
            , Reference extrudeRegion2
            , Reference transform3
            , Reference union3
            , Reference intersection3
            , Reference difference3
            , Reference minkowskiSum3
            , Reference icosahedron
            , Reference octahedron
            , Reference subdivide
            , Reference subdivideSphere
            ]
  in
    [vec 2, vec 3, mat 4 4, mat 4 3, mat 3 3, region2, region3, boundary2]

-- The directory containing typescript files which defines the interface.
-- Within these files one may import reify_generated_interface to access the
-- TypeScript types generated from the definition of |idt| above.
typescriptLibDir :: String
typescriptLibDir = "./typescript"
