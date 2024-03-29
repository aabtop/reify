module ReifyInputInterface
  ( idt,
    typescriptLibDir,
  )
where

import Idt

-- Defines the types that are to form the language of communication between
-- C++, V8, and the TypeScript compiler.
idt :: NamedTypeList
idt =
  let f32 = NamedPrimitive "f32"
      i32 = NamedPrimitive "i32"
      boolean = NamedPrimitive "boolean"

      vec n =
        NamedType
          ("Vec" ++ show n)
          ("A " ++ show n ++ "-dimensional cartesian coordinate vector.")
          $ FixedSizeArray f32 n
      cvec = Concrete . vec
      mat m n =
        NamedType
          ("Matrix" ++ show m ++ show n)
          ("A " ++ show m ++ "x" ++ show n ++ " matrix.")
          $ FixedSizeArray f32 (m * n)
      cmat m n = Concrete $ mat m n
      polyline n =
        NamedType
          ("Polyline" ++ show n)
          ( "A sequence of connected line segments in "
              ++ show n
              ++ " dimensions."
          )
          $ List (cvec n)
      cpolyline = Concrete . polyline
      box n =
        NamedType
          ("Box" ++ show n)
          ( "Two points describing the extreminites of an axis aligned box in "
              ++ show n
              ++ " dimensions."
          )
          $ Struct
            [ ( "corners",
                "2 corners defining the axis-aligned box's extremities.",
                FixedSizeArray (cvec n) 2
              )
            ]

      sRGB = NamedType "sRGB" "A color in the sRGB colorspace." $ cvec 3
      sRGBA = NamedType "sRGBA" "A color in the sRGB colorspace, with alpha." $ cvec 4

      polygon =
        NamedType "Polygon" "A closed polygon." $
          Struct
            [ ( "path",
                "A polyline describing the boundary of the polygon, with the last described vertex connected to the first vertex.",
                cpolyline 2
              )
            ]
      circle =
        NamedType "Circle" "A circle with arbitrary radius and center-point." $
          Struct
            [ ("radius", "The circle's radius.", f32),
              ("center", "The center-point of the circle.", cvec 2)
            ]
      circleAsPolygon =
        NamedType "CircleAsPolygon" "A circle approximated by line segments." $
          Struct
            [ ( "circle",
                "The circle that is being approximated.",
                Concrete circle
              ),
              ( "num_points",
                "The number of points in the resulting polygon.  The more points, the closer the shape will resemble the circle.",
                i32
              )
            ]
      transform2 =
        NamedType "Transform2" "An affine transformed 2D region." $
          Struct
            [ ("source", "The input 2D region to be transformed.", Concrete region2),
              ( "transform",
                "The linear affine transform to apply to the region.",
                cmat 3 3
              )
            ]
      union2 =
        NamedType
          "Union2"
          "A region resulting from the union of 1 or more other regions.  A point is in the resulting region if it is in any of the input regions."
          $ Struct
            [ ( "regions",
                "The list of input regions to union together.",
                List $ Concrete region2
              )
            ]
      intersection2 =
        NamedType
          "Intersection2"
          "A region resulting from the intersection of 1 or more input regions.  A point is in the resulting region if it is in all of the input regions."
          $ Struct
            [ ( "regions",
                "The list of input regions to intersect with each other.",
                List $ Concrete region2
              )
            ]
      difference2 =
        NamedType
          "Difference2"
          "The difference between 2 regions.  A point is in the resulting set if it is in `a` but not in `b`."
          $ Struct
            [ ( "a",
                "The positive region, within which a point must exist to also exist in the output region.",
                Concrete region2
              ),
              ( "b",
                "The negative region, within which if a point exists in `b`, it will not exist in the output region.",
                Concrete region2
              )
            ]
      minkowskiSum2 =
        NamedType
          "MinkowskiSum2"
          "Accumulates the input regions by, for each pair `a` and `b`, taking the union of region `b` translated to every point of region `a`. This is useful for creating rounded corners for example, by taking the Minkowski sum of a region with a circle/sphere."
          $ Struct
            [ ( "regions",
                "The list of input regions, to be accumulated over the Minkowski Sum operation.",
                List $ Concrete region2
              )
            ]
      widenBoundary2 =
        NamedType
          "WidenBoundary2"
          "Widens a 1D boundary of a 2D region by a given width such that it become a region again. Each joint will be offset in the direction of the average of the normals of each two adjacent line segments."
          $ Struct
            [ ( "boundary",
                "The boundary to widen.",
                Concrete boundary2
              ),
              ( "width",
                "How many units to widen the boundary by.",
                f32
              )
            ]
      region2 =
        NamedType "Region2" "An arbitrary shape representing a 2D area." $
          TaggedUnion
            [ Reference polygon,
              Reference circleAsPolygon,
              Reference (box 2),
              Reference transform2,
              Reference union2,
              Reference intersection2,
              Reference difference2,
              Reference minkowskiSum2,
              Reference widenBoundary2
            ]

      boundaryOfRegion2 =
        NamedType
          "BoundaryOfRegion2"
          "Returns an object representing the 1D polygonal boundary of a 2D polygonal region. This will be a closed polygon."
          $ Struct
            [ ( "region",
                "The 2D region to find the boundary of.",
                Concrete region2
              )
            ]
      boundary2 =
        NamedType "Boundary2" "A set of closed polylines that envelope a 2D region." $
          TaggedUnion
            [ Reference boundaryOfRegion2
            ]

      svgElements =
        NamedType "SvgElements" "A sequence of elements, where the ordering represents the draw order (in back-to-front order)." $
          Struct
            [ ( "elements",
                "The elements that define this element sequence.",
                List $ Concrete svgElement
              )
            ]
      svgElement =
        NamedType "SvgElement" "An object representing a single SVG element." $
          TaggedUnion
            [ Reference svgPathElement
            ]
      svgPathElement =
        NamedType
          "SvgPathElement"
          "A representation of the SVG `path` element, used to represent a polygon."
          $ TaggedUnion
            [ Concrete svgPathElementFromRegion2,
              Concrete svgPathElementFromBoundary2
            ]
      svgPathElementFromRegion2 =
        NamedType
          "SvgPathElementFromRegion2"
          "Returns a SVG `path` element based on a 2D region."
          $ Struct
            [ ( "region",
                "The 2D region representing the fill region for the SVG path.",
                Concrete region2
              ),
              ("fill", "The fill style for the region.", Concrete svgFillStyle)
            ]
      svgFillStyle =
        NamedType
          "SvgFillStyle"
          "Defines the fill pattern used for a 2D area, e.g. a fill color."
          $ TaggedUnion
            [ Concrete svgSolidColor
            ]
      svgSolidColor =
        NamedType
          "SvgSolidColor"
          "Defines a solid color to be associated with SVG elements."
          $ Struct
            [ ( "color",
                "The color, which includes an alpha component.",
                Concrete sRGBA
              )
            ]
      svgPathElementFromBoundary2 =
        NamedType
          "SvgPathElementFromBoundary2"
          "Returns a SVG `path` element based on a 2D boundary."
          $ Struct
            [ ( "boundary",
                "The 2D boundary representing the border for the SVG path.",
                Concrete boundary2
              ),
              ("stroke", "The stroke style for drawing along the boundary.", Concrete svgStrokeStyle),
              ("width", "The width of the stroke for rendering the boundary.", Concrete svgWidth)
            ]
      svgStrokeStyle =
        NamedType
          "SvgStrokeStyle"
          "Defines the stroke pattern used for the a 2D boundary edge, e.g. a fill color."
          $ TaggedUnion
            [ Concrete svgSolidColor
            ]
      svgWidth =
        NamedType
          "SvgWidth"
          "Defines a width in SVG, in absolute value or percentages."
          $ TaggedUnion
            [ Concrete svgAbsolute,
              -- Interpret width relative to Hypo's arbitrary coordinate system.
              Concrete svgInfinitesimal
            ]

      svgAbsolute =
        NamedType
          "SvgAbsolute"
          "Defines a scalar absolute value in the given type of units."
          $ Struct
            [ ("value", "The scalar value in the specified units.", f32),
              ("units", "The type of units that the value represents.", Concrete svgScalarUnitType)
            ]
      svgScalarUnitType =
        NamedType
          "SvgScalarUnitType"
          "Defines a type of scalar measurement, e.g. `px` for pixels."
          $ Enum
            [ ("px", "Pixels", [])
            ]
      svgInfinitesimal =
        NamedType
          "SvgInfinitesimal"
          "Defines a distance that approaches zero. Not a standards-defined unit."
          $ Struct []

      triangleList n =
        NamedType
          ("TriangleList" ++ show n)
          ("A list of " ++ show n ++ "-dimensional triangles.")
          $ Struct
            [ ( "vertices",
                "The list of vertices, which may be indexed by the list of triangles.",
                List $ cvec n
              ),
              ( "triangles",
                "The list of triangles, defined by indices into the list of vertices.",
                List $ FixedSizeArray i32 3
              )
            ]
      sphere =
        NamedType "Sphere" "A 3D sphere." $
          Struct
            [ ("radius", "The sphere's radius.", f32),
              ("center", "The center-point of the sphere.", cvec 3)
            ]
      octahedron =
        NamedType
          "Octahedron"
          "The 8-sided platonic solid, approximating the given sphere."
          $ Struct
            [ ( "sphere",
                "A sphere that the octahedron will approximate.",
                Concrete sphere
              )
            ]
      icosahedron =
        NamedType
          "Icosahedron"
          "The 20-sided platonic solid, approximating the given sphere."
          $ Struct
            [ ( "sphere",
                "A sphere that the icosahedron will approximate.",
                Concrete sphere
              )
            ]
      sphereBased =
        NamedType
          "SphereBased"
          "Represents the set of region3 objects based off of spheres."
          $ TaggedUnion
            [ Reference octahedron,
              Reference icosahedron,
              Reference subdivideSphere
            ]
      extrudeRegion2 =
        NamedType
          "Extrude"
          "Results in a `region3` derived from connecting together the edges of all the `region2` transformed into 3D for the given sequence of transforms. If `closed` is true, the last transform will have its edges connected back to the first transform."
          $ Struct
            [ ( "source",
                "The `region2` that acts as the source 2D shape.",
                Concrete region2
              ),
              ( "transforms",
                "The sequence of 2D->3D transforms which position each segment of the resulting `region3`.",
                List (cmat 4 3)
              ),
              ( "closed",
                "If true, the last segment will be connected back to the first segment.",
                boolean
              )
            ]
      transform3 =
        NamedType "Transform3" "An affine transformed 3D region." $
          Struct
            [ ("source", "The input 3D region to be transformed.", Concrete region3),
              ( "transform",
                "The linear affine transform to apply to the region.",
                cmat 4 4
              )
            ]
      union3 =
        NamedType
          "Union3"
          "A region resulting from the union of 1 or more other regions.  A point is in the resulting region if it is in any of the input regions."
          $ Struct
            [ ( "regions",
                "The list of input regions to union together.",
                List $ Concrete region3
              )
            ]
      intersection3 =
        NamedType
          "Intersection3"
          "A region resulting from the intersection of 1 or more input regions.  A point is in the resulting region if it is in all of the input regions."
          $ Struct
            [ ( "regions",
                "The list of input regions to intersect with each other.",
                List $ Concrete region3
              )
            ]
      difference3 =
        NamedType
          "Difference3"
          "The difference between 2 regions.  A point is in the resulting set if it is in `a` but not in `b`."
          $ Struct
            [ ( "a",
                "The positive region, within which a point must exist to also exist in the output region.",
                Concrete region3
              ),
              ( "b",
                "The negative region, within which if a point exists in `b`, it will not exist in the output region.",
                Concrete region3
              )
            ]
      minkowskiSum3 =
        NamedType
          "MinkowskiSum3"
          "Accumulates the input regions by, for each pair `a` and `b`, taking the union of region `b` translated to every point of region `a`. This is useful for creating rounded corners for example, by taking the Minkowski sum of a region with a circle/sphere."
          $ Struct
            [ ( "regions",
                "The list of input regions, to be accumulated over the Minkowski Sum operation.",
                List $ Concrete region3
              )
            ]
      subdivideMethod =
        NamedType
          "SubdivideMethod"
          "Describes different methods of subdividing a 3D surface."
          $ Enum
            [ --  ( "CatmullClark", []) -- TODO(issue #18): Produces a runtime error.
              -- , ("DooSabin", []),  -- TODO(issue #17): Compile problems in CGAL.
              ("Loop", "The Loop subdivision method.", []),
              ("Sqrt3", "The Sqrt3 subdivision method.", [])
            ]
      subdivide =
        NamedType
          "Subdivide"
          "Subdivides the surface of a 3D region according to the specified subdivision method. Note that this function is known to be unstable and can cause a crash if given bad inputs."
          $ Struct
            [ ( "source",
                "The input 3D shape, whose surface will be subdivided and the result converted back into a new `region3`.",
                Concrete region3
              ),
              ( "method",
                "Describes which algorithm to use for subdividing the surface.",
                Concrete subdivideMethod
              ),
              ( "iterations",
                "The number of times the subdivision method should be iterated.  The higher the number, the smoother the output region.",
                i32
              )
            ]
      subdivideSphere =
        NamedType
          "SubdivideSphere"
          "A subdivision method optimized for sphere-approximating shapes.  After each subdivision iteration, all resulting poitns will be projected onto the approximated sphere."
          $ Struct
            [ ("source", "The input sphere-like object.", Concrete sphereBased),
              ( "iterations",
                "The number of times the subdivision method should be iterated.  The higher the number, the smoother the output region.",
                i32
              )
            ]
      region3 =
        NamedType "Region3" "An arbitrary shape representing a 3D volume." $
          TaggedUnion
            [ Reference (triangleList 3),
              Reference (box 3),
              Reference extrudeRegion2,
              Reference transform3,
              Reference union3,
              Reference intersection3,
              Reference difference3,
              Reference minkowskiSum3,
              Reference icosahedron,
              Reference octahedron,
              Reference subdivide,
              Reference subdivideSphere
            ]

      mesh3 =
        NamedType "Mesh3" "A set of connected polygons representing a piecewise linear 2D manifold in 3D. Polygon/edge/vertex connectivity information is preserved and stored." $
          TaggedUnion
            [Reference closedMesh3]

      closedMesh3 =
        NamedType "ClosedMesh3" "A Mesh3 instance, but it is closed, i.e. it encloses a volume." $
          TaggedUnion
            [Reference meshFromRegion3]

      meshFromRegion3 =
        NamedType
          "MeshFromRegion3"
          "Returns a closed mesh representing the surface of the volume region."
          $ Struct
            [ ("region", "The region whose boundary will be extracted.", Concrete region3)
            ]

      triangleSoup3 =
        NamedType "TriangleSoup3" "An unstructured collection of triangles and vertices, which is fast to render and export, but since it doesn't store any connectivity information it's not as flexible as other mesh formats." $
          TaggedUnion
            [ Reference triangleSoupFromMesh3,
              Reference triangleSoupFromRegion3,
              Reference affineTransformTriangleSoup3,
              Reference triangleSoupWithColor3
            ]

      triangleSoupFromMesh3 =
        NamedType "TriangleSoupFromMesh3" "Converts a Mesh3 into an unstructured TriangleSoup3, which is just a set of triangles and vertices, with out any structural or connectivity information." $
          Struct
            [("mesh", "The Mesh3 which is to be converted into a triangle soup.", Concrete mesh3)]

      triangleSoupFromRegion3 =
        NamedType "TriangleSoupFromRegion3" "Converts a Region3's boundary into an unstructured TriangleSoup3, which is just a set of triangles and vertices, with out any structural or connectivity information." $
          Struct
            [("region", "The Region3 which is to be converted into a triangle soup.", Concrete region3)]

      affineTransformTriangleSoup3 =
        NamedType "AffineTransformTriangleSoup3" "Applies an affine transform to a TriangleSoup3." $
          Struct
            [ ("triangle_soup", "The triangle soup which is to be transformed.", Concrete triangleSoup3),
              ("transform", "The affine transform that is to be applied to the input triangle soup.", cmat 4 4)
            ]

      triangleSoupWithColor3 =
        NamedType "TriangleSoupWithColor3" "Sets a color on the given TriangleSoup3, which it is expected to be rendered with." $
          Struct
            [ ("triangle_soup", "The triangle soup which will have a color set on it.", Concrete triangleSoup3),
              ("color", "The color which the input triangle soup is to be set with.", Concrete sRGB)
            ]

      triangleSoupOrTriangleSoupSet3 =
        NamedType
          "TriangleSoupOrTriangleSoupSet3"
          "A TriangleSoup3 or a TriangleSoupSet3."
          $ TaggedUnion
            [Reference triangleSoup3, Reference triangleSoupSet3]

      triangleSoupSet3 =
        NamedType "TriangleSoupSet3" "A set of triangle soups. Useful for expressing the desire to render a collection of meshes at the same time." $
          Struct
            [("triangle_soups", "The collection of triangle soups.", List $ Concrete triangleSoupOrTriangleSoupSet3)]
   in [vec 2, vec 3, mat 4 4, mat 4 3, mat 3 3, sRGB, region2, region3, boundary2, mesh3, triangleSoup3, triangleSoupSet3, svgElements]

-- The directory containing typescript files which defines the interface.
-- Within these files one may import reify_generated_interface to access the
-- TypeScript types generated from the definition of |idt| above.
typescriptLibDir :: String
typescriptLibDir = "./typescript"
