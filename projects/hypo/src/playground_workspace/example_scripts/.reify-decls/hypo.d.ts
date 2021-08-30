import * as rgi from 'reify_generated_interface';
export * from 'reify_generated_interface';
/** Returns a 3x3 affine matrix representing a 2D translation. */
export declare function Translate2(translation: rgi.Vec2): rgi.Matrix33;
/** Returns a 3x3 affine matrix representing a 2D rotation. */
export declare function Rotate2(angle_in_degrees: number): rgi.Matrix33;
/** Returns a 3x3 affine matrix representing a 2D anisotropic scale. */
export declare function Scale2(x: number, y: number): rgi.Matrix33;
/** Returns a 4x4 affine matrix representing a 3D tranlsation. */
export declare function Translate3(translation: rgi.Vec3): rgi.Matrix44;
/** Returns a 4x4 affine matrix representing a 3D rotation around the x axis. */
export declare function Rotate3X(angle_in_degrees: number): rgi.Matrix44;
/** Returns a 4x4 affine matrix representing a 3D rotation around the y axis. */
export declare function Rotate3Y(angle_in_degrees: number): rgi.Matrix44;
/** Returns a 4x4 affine matrix representing a 3D rotation around the z axis. */
export declare function Rotate3Z(angle_in_degrees: number): rgi.Matrix44;
/** Returns a 4x4 affine matrix representing a 3D anisotropic scale. */
export declare function Scale3(x: number, y: number, z: number): rgi.Matrix44;
/** Returns a 3x3 identity matrix, reprenenting an identity 2D affine transform. */
export declare const Identity2: rgi.Matrix33;
/** Returns a 4x4 identity matrix, reprenenting an identity 3D affine transform. */
export declare const Identity3: rgi.Matrix44;
/** A matrix that transforms a 2D point (x, y) to the 3D point (x, y, 0). */
export declare let EmbedOnZPlane: rgi.Matrix43;
/** Multiplies a 3D vector by an affine transform to produce a new 3D vector. */
export declare function VMul3(a: rgi.Matrix44, b: rgi.Vec3): rgi.Vec3;
/** Multiplies a 2D vector by an affine transform to produce a new 2D vector. */
export declare function VMul2(a: rgi.Matrix33, b: rgi.Vec2): rgi.Vec2;
/** Multiplies two 4x4 matrices together to produce a new 4x4 matrix. */
export declare function MMul4(a: rgi.Matrix44, b: rgi.Matrix44): rgi.Matrix44;
/** Returns the cross product of two 3D vectors. */
export declare function Cross(a: rgi.Vec3, b: rgi.Vec3): rgi.Vec3;
/** Returns the dot product of two 2D vectors. */
export declare function Dot2(a: rgi.Vec2, b: rgi.Vec2): number;
/** Returns the dot product of two 3D vectors. */
export declare function Dot3(a: rgi.Vec3, b: rgi.Vec3): number;
/** Returns the negation of a 2D vector. */
export declare function Negate2(x: rgi.Vec2): rgi.Vec2;
/** Returns the negation of a 3D vector. */
export declare function Negate3(x: rgi.Vec3): rgi.Vec3;
/** Multiplies two 3x3 matrices together to produce a new 3x3 matrix. */
export declare function MMul3(a: rgi.Matrix33, b: rgi.Matrix33): rgi.Matrix33;
/** Multiplies a 4x4 matrix with a 4x3 matrix to produce a 4x3 matrix. */
export declare function MMul443(a: rgi.Matrix44, b: rgi.Matrix43): rgi.Matrix43;
/** Multiplies a 4x3 matrix with a 3x3 matrix to produce a 4x3 matrix. */
export declare function MMul433(a: rgi.Matrix43, b: rgi.Matrix33): rgi.Matrix43;
/** Applies a translation to a 3D region to produce a translated 3D region. */
export declare function TranslatedRegion3(x: {
    source: rgi.Region3;
    translation: rgi.Vec3;
}): rgi.Region3;
/** Embeds the 2D region onto the xy plane and then extrudes it by the given height along the z axis. */
export declare function ExtrudeFromZPlane(x: {
    /** The 2D region to be embedded into the xy plane and extruded. */
    source: rgi.Region2;
    /** The height that the input region should be extruded by from the xy plane. */
    height: number;
}): rgi.Region3;
/** Embeds the 2D region onto the xy plane and then extrudes it by the given height along the z axis, twisting it as it is extruded. */
export declare function TwistExtrudeFromZPlane(x: {
    /** The 2D region to be embedded into the xy plane and extruded. */
    source: rgi.Region2;
    /** The height that the input region should be extruded by from the xy plane. */
    height: number;
    /** The total amount of twist that should be applied uniformly during the extrusion. */
    twist_amount_in_degrees: number;
    /** The number of slices to interpolate the twist at. The higher this number, the smoother the twist. */
    num_slices: number;
}): rgi.Region3;
/** Creates a torus in the xy plane. */
export declare function Torus(x: {
    /** The torus' inner radius in the xy plane. */
    inner_radius: number;
    /** The torus' outer radius in the xy plane. */
    outer_radius: number;
    /** The number of points on the polygonal approximation of a circle in a cross section of the torus. The higher the number, the smoother the torus will appear. */
    num_cross_section_points: number;
    /** The number of radial cross sections on in torus.  The higher the number the smoother the torus will appear. */
    num_slices: number;
}): rgi.Region3;
/** Creates a cylinder along the z axis. */
export declare function Cylinder(x: {
    /** Radius of the cylinder cross section in the xy plane. */
    radius: number;
    /** Height of the cylinder along the z axis. */
    height: number;
    /** Number of points in the polygonal approximation of a circle cross section. */
    num_points: number;
}): rgi.Region3;
/** Returns a geodesic sphere, i.e. a subdivided icosahedron. */
export declare function GeodesicSphere(x: {
    /** Sphere parameters that will be approximated by the geodesic sphere. */
    sphere: rgi.SphereParams;
    /** Number of subdivision iterations. */
    iterations: number;
}): rgi.SubdivideSphere;
