module Lib
    ( jeep,
      Vec2,
      Vec3,
      Vec4,
      Matrix44,
      Circle (..),
    ) where

type Vec2 = (Float, Float)
type Vec3 = (Float, Float, Float)
type Vec4 = (Float, Float, Float, Float)
type Matrix44 = (Float, Float, Float, Float,Float, Float, Float, Float, Float, Float, Float, Float,Float, Float, Float, Float)

instance (Show a, Show b, Show c, Show d,
          Show e, Show f, Show g, Show h,
          Show i, Show j, Show k, Show l,
          Show m, Show n, Show o, Show p) => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) where
    show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p) = "("++(show a)++", "++(show b)++", "++(show c)++", "++
                              (show d)++", "++(show e)++", "++(show f)++", "++(show g)++", "++(show h)++", "++(show i)++", "++(show j)++", "++(show k)++", "++(show l)++", "++(show m)++", "++(show n)++", "++(show o)++", "++(show p)++")"

data Circle = Circle {
    radius :: Float,
    center :: Vec2
} deriving (Show)

data Rectangle = Rectangle {
    width :: Float,
    height :: Float,
    center ::  Vec2
} deriving (Show)

data Mesh2 = CircleAsMesh Circle | RectangleAsMesh Rectangle deriving (Show)
 
data ExtrudeMesh2 = ExtrudeMesh2 {
    source :: Mesh2,
    path :: (Vec3, Vec3)
} deriving (Show)

data TransformMesh3 = TransformMesh3 {
    source :: Mesh3,
    transform :: Matrix44
} deriving (Show)

data Mesh3 = ExtrudeMesh2AsMesh ExtrudeMesh2 |
             TransformMesh3AsMesh TransformMesh3 |
             MeshUnion [Mesh3]
             deriving (Show)

translate3D :: Mesh3 -> Vec3 -> Mesh3
translate3D source (x,y,z) = TransformMesh3AsMesh TransformMesh3 {
    source = source,
    transform = (1.0, 0.0, 0.0, x,
                 0.0, 1.0, 0.0, y,
                 0.0, 0.0, 1.0, z,
                 0.0, 0.0, 0.0, 1.0)
}

cylinder :: Float -> (Vec3, Vec3) -> Mesh3
cylinder radius path = ExtrudeMesh2AsMesh ExtrudeMesh2 {
    source = CircleAsMesh Circle {
        radius = radius,
        center = (0, 0)
    },
    path = path
}

jeep :: Mesh3
jeep = let
    wheel = cylinder 0.35 ((0, 0, -0.75), (0, 0, 0.75))
    in MeshUnion [
        translate3D wheel (1.0, 0.0, -0.5),
        translate3D wheel (1.0, 0.0, 0.5),
        translate3D wheel (-1.0, 0.0, -0.5),
        translate3D wheel (-1.0, 0.0, 0.5)]
