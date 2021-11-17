/** A 2-dimensional cartesian coordinate vector. */
export declare type Vec2 = [number, number];
/** A 3-dimensional cartesian coordinate vector. */
export declare type Vec3 = [number, number, number];
/** A 4x4 matrix. */
export declare type Matrix44 = [number, number, number, number, number, number, number, number, number, number, number, number, number, number, number, number];
/** A 4x3 matrix. */
export declare type Matrix43 = [number, number, number, number, number, number, number, number, number, number, number, number];
/** A 3x3 matrix. */
export declare type Matrix33 = [number, number, number, number, number, number, number, number, number];
/** A color in the sRGB colorspace. */
export declare type sRGB = Vec3;
/** A sequence of connected line segments in 2 dimensions. */
export declare type Polyline2 = Vec2[];
/** A closed polygon. */
export declare type PolygonParams = {
    /** A polyline describing the boundary of the polygon, with the last described vertex connected to the first vertex. */
    path: Polyline2;
};
interface PolygonWithKind extends PolygonParams {
    __kind: 'Polygon';
}
export interface Polygon extends Readonly<PolygonWithKind> {
}
/** A closed polygon. */
export declare function Polygon(x: {
    /** A polyline describing the boundary of the polygon, with the last described vertex connected to the first vertex. */
    path: Polyline2;
}): Polygon;
/** A circle with arbitrary radius and center-point. */
export declare type CircleParams = {
    /** The circle's radius. */
    radius: number;
    /** The center-point of the circle. */
    center: Vec2;
};
interface CircleWithKind extends CircleParams {
    __kind: 'Circle';
}
export interface Circle extends Readonly<CircleWithKind> {
}
/** A circle with arbitrary radius and center-point. */
export declare function Circle(x: {
    /** The circle's radius. */
    radius: number;
    /** The center-point of the circle. */
    center: Vec2;
}): Circle;
/** A circle approximated by line segments. */
export declare type CircleAsPolygonParams = {
    /** The circle that is being approximated. */
    circle: CircleParams;
    /** The number of points in the resulting polygon.  The more points, the closer the shape will resemble the circle. */
    num_points: number;
};
interface CircleAsPolygonWithKind extends CircleAsPolygonParams {
    __kind: 'CircleAsPolygon';
}
export interface CircleAsPolygon extends Readonly<CircleAsPolygonWithKind> {
}
/** A circle approximated by line segments. */
export declare function CircleAsPolygon(x: {
    /** The circle that is being approximated. */
    circle: CircleParams;
    /** The number of points in the resulting polygon.  The more points, the closer the shape will resemble the circle. */
    num_points: number;
}): CircleAsPolygon;
/** Two points describing the extreminites of an axis aligned box in 2 dimensions. */
export declare type Box2Params = {
    /** 2 corners defining the axis-aligned box's extremities. */
    corners: [Vec2, Vec2];
};
interface Box2WithKind extends Box2Params {
    __kind: 'Box2';
}
export interface Box2 extends Readonly<Box2WithKind> {
}
/** Two points describing the extreminites of an axis aligned box in 2 dimensions. */
export declare function Box2(x: {
    /** 2 corners defining the axis-aligned box's extremities. */
    corners: [Vec2, Vec2];
}): Box2;
/** A region resulting from the union of 1 or more other regions.  A point is in the resulting region if it is in any of the input regions. */
export declare type Union2Params = {
    /** The list of input regions to union together. */
    regions: Region2[];
};
interface Union2WithKind extends Union2Params {
    __kind: 'Union2';
}
export interface Union2 extends Readonly<Union2WithKind> {
}
/** A region resulting from the union of 1 or more other regions.  A point is in the resulting region if it is in any of the input regions. */
export declare function Union2(x: {
    /** The list of input regions to union together. */
    regions: Region2[];
}): Union2;
/** A region resulting from the intersection of 1 or more input regions.  A point is in the resulting region if it is in all of the input regions. */
export declare type Intersection2Params = {
    /** The list of input regions to intersect with each other. */
    regions: Region2[];
};
interface Intersection2WithKind extends Intersection2Params {
    __kind: 'Intersection2';
}
export interface Intersection2 extends Readonly<Intersection2WithKind> {
}
/** A region resulting from the intersection of 1 or more input regions.  A point is in the resulting region if it is in all of the input regions. */
export declare function Intersection2(x: {
    /** The list of input regions to intersect with each other. */
    regions: Region2[];
}): Intersection2;
/** Accumulates the input regions by, for each pair `a` and `b`, taking the union of region `b` translated to every point of region `a`. This is useful for creating rounded corners for example, by taking the Minkowski sum of a region with a circle/sphere. */
export declare type MinkowskiSum2Params = {
    /** The list of input regions, to be accumulated over the Minkowski Sum operation. */
    regions: Region2[];
};
interface MinkowskiSum2WithKind extends MinkowskiSum2Params {
    __kind: 'MinkowskiSum2';
}
export interface MinkowskiSum2 extends Readonly<MinkowskiSum2WithKind> {
}
/** Accumulates the input regions by, for each pair `a` and `b`, taking the union of region `b` translated to every point of region `a`. This is useful for creating rounded corners for example, by taking the Minkowski sum of a region with a circle/sphere. */
export declare function MinkowskiSum2(x: {
    /** The list of input regions, to be accumulated over the Minkowski Sum operation. */
    regions: Region2[];
}): MinkowskiSum2;
/** A set of closed polylines that envelope a 2D region. */
export declare type Boundary2 = BoundaryOfRegion2;
/** Widens a 1D boundary of a 2D region by a given width such that it become a region again. Each joint will be offset in the direction of the average of the normals of each two adjacent line segments. */
export declare type WidenBoundary2Params = {
    /** The boundary to widen. */
    boundary: Boundary2;
    /** How many units to widen the boundary by. */
    width: number;
};
interface WidenBoundary2WithKind extends WidenBoundary2Params {
    __kind: 'WidenBoundary2';
}
export interface WidenBoundary2 extends Readonly<WidenBoundary2WithKind> {
}
/** Widens a 1D boundary of a 2D region by a given width such that it become a region again. Each joint will be offset in the direction of the average of the normals of each two adjacent line segments. */
export declare function WidenBoundary2(x: {
    /** The boundary to widen. */
    boundary: Boundary2;
    /** How many units to widen the boundary by. */
    width: number;
}): WidenBoundary2;
/** An arbitrary shape representing a 2D area. */
export declare type Region2 = Polygon | CircleAsPolygon | Box2 | Transform2 | Union2 | Intersection2 | Difference2 | MinkowskiSum2 | WidenBoundary2;
/** A list of 3-dimensional triangles. */
export declare type TriangleList3Params = {
    /** The list of vertices, which may be indexed by the list of triangles. */
    vertices: Vec3[];
    /** The list of triangles, defined by indices into the list of vertices. */
    triangles: [number, number, number][];
};
interface TriangleList3WithKind extends TriangleList3Params {
    __kind: 'TriangleList3';
}
export interface TriangleList3 extends Readonly<TriangleList3WithKind> {
}
/** A list of 3-dimensional triangles. */
export declare function TriangleList3(x: {
    /** The list of vertices, which may be indexed by the list of triangles. */
    vertices: Vec3[];
    /** The list of triangles, defined by indices into the list of vertices. */
    triangles: [number, number, number][];
}): TriangleList3;
/** Two points describing the extreminites of an axis aligned box in 3 dimensions. */
export declare type Box3Params = {
    /** 2 corners defining the axis-aligned box's extremities. */
    corners: [Vec3, Vec3];
};
interface Box3WithKind extends Box3Params {
    __kind: 'Box3';
}
export interface Box3 extends Readonly<Box3WithKind> {
}
/** Two points describing the extreminites of an axis aligned box in 3 dimensions. */
export declare function Box3(x: {
    /** 2 corners defining the axis-aligned box's extremities. */
    corners: [Vec3, Vec3];
}): Box3;
/** Results in a `region3` derived from connecting together the edges of all the `region2` transformed into 3D for the given sequence of transforms. If `closed` is true, the last transform will have its edges connected back to the first transform. */
export declare type ExtrudeParams = {
    /** The `region2` that acts as the source 2D shape. */
    source: Region2;
    /** The sequence of 2D->3D transforms which position each segment of the resulting `region3`. */
    transforms: Matrix43[];
    /** If true, the last segment will be connected back to the first segment. */
    closed: boolean;
};
interface ExtrudeWithKind extends ExtrudeParams {
    __kind: 'Extrude';
}
export interface Extrude extends Readonly<ExtrudeWithKind> {
}
/** Results in a `region3` derived from connecting together the edges of all the `region2` transformed into 3D for the given sequence of transforms. If `closed` is true, the last transform will have its edges connected back to the first transform. */
export declare function Extrude(x: {
    /** The `region2` that acts as the source 2D shape. */
    source: Region2;
    /** The sequence of 2D->3D transforms which position each segment of the resulting `region3`. */
    transforms: Matrix43[];
    /** If true, the last segment will be connected back to the first segment. */
    closed: boolean;
}): Extrude;
/** A region resulting from the union of 1 or more other regions.  A point is in the resulting region if it is in any of the input regions. */
export declare type Union3Params = {
    /** The list of input regions to union together. */
    regions: Region3[];
};
interface Union3WithKind extends Union3Params {
    __kind: 'Union3';
}
export interface Union3 extends Readonly<Union3WithKind> {
}
/** A region resulting from the union of 1 or more other regions.  A point is in the resulting region if it is in any of the input regions. */
export declare function Union3(x: {
    /** The list of input regions to union together. */
    regions: Region3[];
}): Union3;
/** A region resulting from the intersection of 1 or more input regions.  A point is in the resulting region if it is in all of the input regions. */
export declare type Intersection3Params = {
    /** The list of input regions to intersect with each other. */
    regions: Region3[];
};
interface Intersection3WithKind extends Intersection3Params {
    __kind: 'Intersection3';
}
export interface Intersection3 extends Readonly<Intersection3WithKind> {
}
/** A region resulting from the intersection of 1 or more input regions.  A point is in the resulting region if it is in all of the input regions. */
export declare function Intersection3(x: {
    /** The list of input regions to intersect with each other. */
    regions: Region3[];
}): Intersection3;
/** Accumulates the input regions by, for each pair `a` and `b`, taking the union of region `b` translated to every point of region `a`. This is useful for creating rounded corners for example, by taking the Minkowski sum of a region with a circle/sphere. */
export declare type MinkowskiSum3Params = {
    /** The list of input regions, to be accumulated over the Minkowski Sum operation. */
    regions: Region3[];
};
interface MinkowskiSum3WithKind extends MinkowskiSum3Params {
    __kind: 'MinkowskiSum3';
}
export interface MinkowskiSum3 extends Readonly<MinkowskiSum3WithKind> {
}
/** Accumulates the input regions by, for each pair `a` and `b`, taking the union of region `b` translated to every point of region `a`. This is useful for creating rounded corners for example, by taking the Minkowski sum of a region with a circle/sphere. */
export declare function MinkowskiSum3(x: {
    /** The list of input regions, to be accumulated over the Minkowski Sum operation. */
    regions: Region3[];
}): MinkowskiSum3;
/** A 3D sphere. */
export declare type SphereParams = {
    /** The sphere's radius. */
    radius: number;
    /** The center-point of the sphere. */
    center: Vec3;
};
interface SphereWithKind extends SphereParams {
    __kind: 'Sphere';
}
export interface Sphere extends Readonly<SphereWithKind> {
}
/** A 3D sphere. */
export declare function Sphere(x: {
    /** The sphere's radius. */
    radius: number;
    /** The center-point of the sphere. */
    center: Vec3;
}): Sphere;
/** The 20-sided platonic solid, approximating the given sphere. */
export declare type IcosahedronParams = {
    /** A sphere that the icosahedron will approximate. */
    sphere: SphereParams;
};
interface IcosahedronWithKind extends IcosahedronParams {
    __kind: 'Icosahedron';
}
export interface Icosahedron extends Readonly<IcosahedronWithKind> {
}
/** The 20-sided platonic solid, approximating the given sphere. */
export declare function Icosahedron(x: {
    /** A sphere that the icosahedron will approximate. */
    sphere: SphereParams;
}): Icosahedron;
/** The 8-sided platonic solid, approximating the given sphere. */
export declare type OctahedronParams = {
    /** A sphere that the octahedron will approximate. */
    sphere: SphereParams;
};
interface OctahedronWithKind extends OctahedronParams {
    __kind: 'Octahedron';
}
export interface Octahedron extends Readonly<OctahedronWithKind> {
}
/** The 8-sided platonic solid, approximating the given sphere. */
export declare function Octahedron(x: {
    /** A sphere that the octahedron will approximate. */
    sphere: SphereParams;
}): Octahedron;
/** Represents the set of region3 objects based off of spheres. */
export declare type SphereBased = Octahedron | Icosahedron | SubdivideSphere;
/** A subdivision method optimized for sphere-approximating shapes.  After each subdivision iteration, all resulting poitns will be projected onto the approximated sphere. */
export declare type SubdivideSphereParams = {
    /** The input sphere-like object. */
    source: SphereBased;
    /** The number of times the subdivision method should be iterated.  The higher the number, the smoother the output region. */
    iterations: number;
};
interface SubdivideSphereWithKind extends SubdivideSphereParams {
    __kind: 'SubdivideSphere';
}
export interface SubdivideSphere extends Readonly<SubdivideSphereWithKind> {
}
/** A subdivision method optimized for sphere-approximating shapes.  After each subdivision iteration, all resulting poitns will be projected onto the approximated sphere. */
export declare function SubdivideSphere(x: {
    /** The input sphere-like object. */
    source: SphereBased;
    /** The number of times the subdivision method should be iterated.  The higher the number, the smoother the output region. */
    iterations: number;
}): SubdivideSphere;
/** An arbitrary shape representing a 3D volume. */
export declare type Region3 = TriangleList3 | Box3 | Extrude | Transform3 | Union3 | Intersection3 | Difference3 | MinkowskiSum3 | Icosahedron | Octahedron | Subdivide | SubdivideSphere;
/** Returns a closed mesh representing the surface of the volume region. */
export declare type MeshFromRegion3Params = {
    /** The region whose boundary will be extracted. */
    region: Region3;
};
interface MeshFromRegion3WithKind extends MeshFromRegion3Params {
    __kind: 'MeshFromRegion3';
}
export interface MeshFromRegion3 extends Readonly<MeshFromRegion3WithKind> {
}
/** Returns a closed mesh representing the surface of the volume region. */
export declare function MeshFromRegion3(x: {
    /** The region whose boundary will be extracted. */
    region: Region3;
}): MeshFromRegion3;
/** A Mesh3 instance, but it is closed, i.e. it encloses a volume. */
export declare type ClosedMesh3 = MeshFromRegion3;
/** A set of connected polygons representing a piecewise linear 2D manifold in 3D. Polygon/edge/vertex connectivity information is preserved and stored. */
export declare type Mesh3 = ClosedMesh3;
/** Converts a Mesh3 into an unstructured TriangleSoup3, which is just a set of triangles and vertices, with out any structural or connectivity information. */
export declare type TriangleSoupFromMesh3Params = {
    /** The Mesh3 which is to be converted into a triangle soup. */
    mesh: Mesh3;
};
interface TriangleSoupFromMesh3WithKind extends TriangleSoupFromMesh3Params {
    __kind: 'TriangleSoupFromMesh3';
}
export interface TriangleSoupFromMesh3 extends Readonly<TriangleSoupFromMesh3WithKind> {
}
/** Converts a Mesh3 into an unstructured TriangleSoup3, which is just a set of triangles and vertices, with out any structural or connectivity information. */
export declare function TriangleSoupFromMesh3(x: {
    /** The Mesh3 which is to be converted into a triangle soup. */
    mesh: Mesh3;
}): TriangleSoupFromMesh3;
/** Converts a Region3's boundary into an unstructured TriangleSoup3, which is just a set of triangles and vertices, with out any structural or connectivity information. */
export declare type TriangleSoupFromRegion3Params = {
    /** The Region3 which is to be converted into a triangle soup. */
    region: Region3;
};
interface TriangleSoupFromRegion3WithKind extends TriangleSoupFromRegion3Params {
    __kind: 'TriangleSoupFromRegion3';
}
export interface TriangleSoupFromRegion3 extends Readonly<TriangleSoupFromRegion3WithKind> {
}
/** Converts a Region3's boundary into an unstructured TriangleSoup3, which is just a set of triangles and vertices, with out any structural or connectivity information. */
export declare function TriangleSoupFromRegion3(x: {
    /** The Region3 which is to be converted into a triangle soup. */
    region: Region3;
}): TriangleSoupFromRegion3;
/** An unstructured collection of triangles and vertices, which is fast to render and export, but since it doesn't store any connectivity information it's not as flexible as other mesh formats. */
export declare type TriangleSoup3 = TriangleSoupFromMesh3 | TriangleSoupFromRegion3 | AffineTransformTriangleSoup3 | TriangleSoupWithColor3;
/** A TriangleSoup3 or a TriangleSoupSet3. */
export declare type TriangleSoupOrTriangleSoupSet3 = TriangleSoup3 | TriangleSoupSet3;
/** A set of triangle soups. Useful for expressing the desire to render a collection of meshes at the same time. */
export declare type TriangleSoupSet3Params = {
    /** The collection of triangle soups. */
    triangle_soups: TriangleSoupOrTriangleSoupSet3[];
};
interface TriangleSoupSet3WithKind extends TriangleSoupSet3Params {
    __kind: 'TriangleSoupSet3';
}
export interface TriangleSoupSet3 extends Readonly<TriangleSoupSet3WithKind> {
}
/** A set of triangle soups. Useful for expressing the desire to render a collection of meshes at the same time. */
export declare function TriangleSoupSet3(x: {
    /** The collection of triangle soups. */
    triangle_soups: TriangleSoupOrTriangleSoupSet3[];
}): TriangleSoupSet3;
/** A 4-dimensional cartesian coordinate vector. */
export declare type Vec4 = [number, number, number, number];
/** A color in the sRGB colorspace, with alpha. */
export declare type sRGBA = Vec4;
/** Defines a solid color to be associated with SVG elements. */
export declare type SvgSolidColorParams = {
    /** The color, which includes an alpha component. */
    color: sRGBA;
};
interface SvgSolidColorWithKind extends SvgSolidColorParams {
    __kind: 'SvgSolidColor';
}
export interface SvgSolidColor extends Readonly<SvgSolidColorWithKind> {
}
/** Defines a solid color to be associated with SVG elements. */
export declare function SvgSolidColor(x: {
    /** The color, which includes an alpha component. */
    color: sRGBA;
}): SvgSolidColor;
/** Defines the fill pattern used for a 2D area, e.g. a fill color. */
export declare type SvgFillStyle = SvgSolidColor;
/** Returns a SVG `path` element based on a 2D region. */
export declare type SvgPathElementFromRegion2Params = {
    /** The 2D region representing the fill region for the SVG path. */
    region: Region2;
    /** The fill style for the region. */
    fill: SvgFillStyle;
};
interface SvgPathElementFromRegion2WithKind extends SvgPathElementFromRegion2Params {
    __kind: 'SvgPathElementFromRegion2';
}
export interface SvgPathElementFromRegion2 extends Readonly<SvgPathElementFromRegion2WithKind> {
}
/** Returns a SVG `path` element based on a 2D region. */
export declare function SvgPathElementFromRegion2(x: {
    /** The 2D region representing the fill region for the SVG path. */
    region: Region2;
    /** The fill style for the region. */
    fill: SvgFillStyle;
}): SvgPathElementFromRegion2;
/** Defines the stroke pattern used for the a 2D boundary edge, e.g. a fill color. */
export declare type SvgStrokeStyle = SvgSolidColor;
/** Defines a type of scalar measurement, e.g. `px` for pixels. */
export declare const enum SvgScalarUnitType {
    /** Pixels */
    px = 0
}
/** Defines a scalar absolute value in the given type of units. */
export declare type SvgAbsoluteParams = {
    /** The scalar value in the specified units. */
    value: number;
    /** The type of units that the value represents. */
    units: SvgScalarUnitType;
};
interface SvgAbsoluteWithKind extends SvgAbsoluteParams {
    __kind: 'SvgAbsolute';
}
export interface SvgAbsolute extends Readonly<SvgAbsoluteWithKind> {
}
/** Defines a scalar absolute value in the given type of units. */
export declare function SvgAbsolute(x: {
    /** The scalar value in the specified units. */
    value: number;
    /** The type of units that the value represents. */
    units: SvgScalarUnitType;
}): SvgAbsolute;
/** Defines a distance that approaches zero. Not a standards-defined unit. */
export declare type SvgInfinitesimalParams = {};
interface SvgInfinitesimalWithKind extends SvgInfinitesimalParams {
    __kind: 'SvgInfinitesimal';
}
export interface SvgInfinitesimal extends Readonly<SvgInfinitesimalWithKind> {
}
/** Defines a distance that approaches zero. Not a standards-defined unit. */
export declare function SvgInfinitesimal(x: {}): SvgInfinitesimal;
/** Defines a width in SVG, in absolute value or percentages. */
export declare type SvgWidth = SvgAbsolute | SvgInfinitesimal;
/** Returns a SVG `path` element based on a 2D boundary. */
export declare type SvgPathElementFromBoundary2Params = {
    /** The 2D boundary representing the border for the SVG path. */
    boundary: Boundary2;
    /** The stroke style for drawing along the boundary. */
    stroke: SvgStrokeStyle;
    /** The width of the stroke for rendering the boundary. */
    width: SvgWidth;
};
interface SvgPathElementFromBoundary2WithKind extends SvgPathElementFromBoundary2Params {
    __kind: 'SvgPathElementFromBoundary2';
}
export interface SvgPathElementFromBoundary2 extends Readonly<SvgPathElementFromBoundary2WithKind> {
}
/** Returns a SVG `path` element based on a 2D boundary. */
export declare function SvgPathElementFromBoundary2(x: {
    /** The 2D boundary representing the border for the SVG path. */
    boundary: Boundary2;
    /** The stroke style for drawing along the boundary. */
    stroke: SvgStrokeStyle;
    /** The width of the stroke for rendering the boundary. */
    width: SvgWidth;
}): SvgPathElementFromBoundary2;
/** A representation of the SVG `path` element, used to represent a polygon. */
export declare type SvgPathElement = SvgPathElementFromRegion2 | SvgPathElementFromBoundary2;
/** An object representing a single SVG element. */
export declare type SvgElement = SvgPathElement;
/** A sequence of elements, where the ordering represents the draw order (in back-to-front order). */
export declare type SvgElementsParams = {
    /** The elements that define this element sequence. */
    elements: SvgElement[];
};
interface SvgElementsWithKind extends SvgElementsParams {
    __kind: 'SvgElements';
}
export interface SvgElements extends Readonly<SvgElementsWithKind> {
}
/** A sequence of elements, where the ordering represents the draw order (in back-to-front order). */
export declare function SvgElements(x: {
    /** The elements that define this element sequence. */
    elements: SvgElement[];
}): SvgElements;
/** Applies an affine transform to a TriangleSoup3. */
export declare type AffineTransformTriangleSoup3Params = {
    /** The triangle soup which is to be transformed. */
    triangle_soup: TriangleSoup3;
    /** The affine transform that is to be applied to the input triangle soup. */
    transform: Matrix44;
};
interface AffineTransformTriangleSoup3WithKind extends AffineTransformTriangleSoup3Params {
    __kind: 'AffineTransformTriangleSoup3';
}
export interface AffineTransformTriangleSoup3 extends Readonly<AffineTransformTriangleSoup3WithKind> {
}
/** Applies an affine transform to a TriangleSoup3. */
export declare function AffineTransformTriangleSoup3(x: {
    /** The triangle soup which is to be transformed. */
    triangle_soup: TriangleSoup3;
    /** The affine transform that is to be applied to the input triangle soup. */
    transform: Matrix44;
}): AffineTransformTriangleSoup3;
/** Returns an object representing the 1D polygonal boundary of a 2D polygonal region. This will be a closed polygon. */
export declare type BoundaryOfRegion2Params = {
    /** The 2D region to find the boundary of. */
    region: Region2;
};
interface BoundaryOfRegion2WithKind extends BoundaryOfRegion2Params {
    __kind: 'BoundaryOfRegion2';
}
export interface BoundaryOfRegion2 extends Readonly<BoundaryOfRegion2WithKind> {
}
/** Returns an object representing the 1D polygonal boundary of a 2D polygonal region. This will be a closed polygon. */
export declare function BoundaryOfRegion2(x: {
    /** The 2D region to find the boundary of. */
    region: Region2;
}): BoundaryOfRegion2;
/** The difference between 2 regions.  A point is in the resulting set if it is in `a` but not in `b`. */
export declare type Difference2Params = {
    /** The positive region, within which a point must exist to also exist in the output region. */
    a: Region2;
    /** The negative region, within which if a point exists in `b`, it will not exist in the output region. */
    b: Region2;
};
interface Difference2WithKind extends Difference2Params {
    __kind: 'Difference2';
}
export interface Difference2 extends Readonly<Difference2WithKind> {
}
/** The difference between 2 regions.  A point is in the resulting set if it is in `a` but not in `b`. */
export declare function Difference2(x: {
    /** The positive region, within which a point must exist to also exist in the output region. */
    a: Region2;
    /** The negative region, within which if a point exists in `b`, it will not exist in the output region. */
    b: Region2;
}): Difference2;
/** The difference between 2 regions.  A point is in the resulting set if it is in `a` but not in `b`. */
export declare type Difference3Params = {
    /** The positive region, within which a point must exist to also exist in the output region. */
    a: Region3;
    /** The negative region, within which if a point exists in `b`, it will not exist in the output region. */
    b: Region3;
};
interface Difference3WithKind extends Difference3Params {
    __kind: 'Difference3';
}
export interface Difference3 extends Readonly<Difference3WithKind> {
}
/** The difference between 2 regions.  A point is in the resulting set if it is in `a` but not in `b`. */
export declare function Difference3(x: {
    /** The positive region, within which a point must exist to also exist in the output region. */
    a: Region3;
    /** The negative region, within which if a point exists in `b`, it will not exist in the output region. */
    b: Region3;
}): Difference3;
/** Describes different methods of subdividing a 3D surface. */
export declare const enum SubdivideMethod {
    /** The Loop subdivision method. */
    Loop = 0,
    /** The Sqrt3 subdivision method. */
    Sqrt3 = 1
}
/** Subdivides the surface of a 3D region according to the specified subdivision method. Note that this function is known to be unstable and can cause a crash if given bad inputs. */
export declare type SubdivideParams = {
    /** The input 3D shape, whose surface will be subdivided and the result converted back into a new `region3`. */
    source: Region3;
    /** Describes which algorithm to use for subdividing the surface. */
    method: SubdivideMethod;
    /** The number of times the subdivision method should be iterated.  The higher the number, the smoother the output region. */
    iterations: number;
};
interface SubdivideWithKind extends SubdivideParams {
    __kind: 'Subdivide';
}
export interface Subdivide extends Readonly<SubdivideWithKind> {
}
/** Subdivides the surface of a 3D region according to the specified subdivision method. Note that this function is known to be unstable and can cause a crash if given bad inputs. */
export declare function Subdivide(x: {
    /** The input 3D shape, whose surface will be subdivided and the result converted back into a new `region3`. */
    source: Region3;
    /** Describes which algorithm to use for subdividing the surface. */
    method: SubdivideMethod;
    /** The number of times the subdivision method should be iterated.  The higher the number, the smoother the output region. */
    iterations: number;
}): Subdivide;
/** An affine transformed 2D region. */
export declare type Transform2Params = {
    /** The input 2D region to be transformed. */
    source: Region2;
    /** The linear affine transform to apply to the region. */
    transform: Matrix33;
};
interface Transform2WithKind extends Transform2Params {
    __kind: 'Transform2';
}
export interface Transform2 extends Readonly<Transform2WithKind> {
}
/** An affine transformed 2D region. */
export declare function Transform2(x: {
    /** The input 2D region to be transformed. */
    source: Region2;
    /** The linear affine transform to apply to the region. */
    transform: Matrix33;
}): Transform2;
/** An affine transformed 3D region. */
export declare type Transform3Params = {
    /** The input 3D region to be transformed. */
    source: Region3;
    /** The linear affine transform to apply to the region. */
    transform: Matrix44;
};
interface Transform3WithKind extends Transform3Params {
    __kind: 'Transform3';
}
export interface Transform3 extends Readonly<Transform3WithKind> {
}
/** An affine transformed 3D region. */
export declare function Transform3(x: {
    /** The input 3D region to be transformed. */
    source: Region3;
    /** The linear affine transform to apply to the region. */
    transform: Matrix44;
}): Transform3;
/** Sets a color on the given TriangleSoup3, which it is expected to be rendered with. */
export declare type TriangleSoupWithColor3Params = {
    /** The triangle soup which will have a color set on it. */
    triangle_soup: TriangleSoup3;
    /** The color which the input triangle soup is to be set with. */
    color: sRGB;
};
interface TriangleSoupWithColor3WithKind extends TriangleSoupWithColor3Params {
    __kind: 'TriangleSoupWithColor3';
}
export interface TriangleSoupWithColor3 extends Readonly<TriangleSoupWithColor3WithKind> {
}
/** Sets a color on the given TriangleSoup3, which it is expected to be rendered with. */
export declare function TriangleSoupWithColor3(x: {
    /** The triangle soup which will have a color set on it. */
    triangle_soup: TriangleSoup3;
    /** The color which the input triangle soup is to be set with. */
    color: sRGB;
}): TriangleSoupWithColor3;
export {};
