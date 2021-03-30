import * as rgi from 'reify_generated_interface'
export * from 'reify_generated_interface'

const DEGREES_TO_RADIANS = (2 * Math.PI) / 360;

/** Returns a 3x3 affine matrix representing a 2D translation. */
export function Translate2(translation: rgi.Vec2): rgi.Matrix33 {
  return [1.0, 0.0, translation[0], 0.0, 1.0, translation[1], 0.0, 0.0, 1.0];
}

/** Returns a 3x3 affine matrix representing a 2D rotation. */
export function Rotate2(angle_in_degrees: number): rgi.Matrix33 {
  let sinx = Math.sin(angle_in_degrees * DEGREES_TO_RADIANS);
  let cosx = Math.cos(angle_in_degrees * DEGREES_TO_RADIANS);
  return [cosx, -sinx, 0.0, sinx, cosx, 0.0, 0.0, 0.0, 1.0];
}

/** Returns a 3x3 affine matrix representing a 2D anisotropic scale. */
export function Scale2(x: number, y: number): rgi.Matrix33 {
  return [x, 0.0, 0.0, 0.0, y, 0.0, 0.0, 0.0, 1.0];
}

/** Returns a 4x4 affine matrix representing a 3D tranlsation. */
export function Translate3(translation: rgi.Vec3): rgi.Matrix44 {
  return [
    1.0, 0.0, 0.0, translation[0], 0.0, 1.0, 0.0, translation[1], 0.0, 0.0, 1.0,
    translation[2], 0.0, 0.0, 0.0, 1.0
  ];
}

/** Returns a 4x4 affine matrix representing a 3D rotation around the x axis. */
export function Rotate3X(angle_in_degrees: number): rgi.Matrix44 {
  let sinx = Math.sin(angle_in_degrees * DEGREES_TO_RADIANS);
  let cosx = Math.cos(angle_in_degrees * DEGREES_TO_RADIANS);
  return [
    1.0, 0.0, 0.0, 0.0, 0.0, cosx, -sinx, 0.0, 0.0, sinx, cosx, 0.0, 0.0, 0.0,
    0.0, 1.0
  ];
}

/** Returns a 4x4 affine matrix representing a 3D rotation around the y axis. */
export function Rotate3Y(angle_in_degrees: number): rgi.Matrix44 {
  let sinx = Math.sin(angle_in_degrees * DEGREES_TO_RADIANS);
  let cosx = Math.cos(angle_in_degrees * DEGREES_TO_RADIANS);
  return [
    cosx, 0.0, sinx, 0.0, 0.0, 1.0, 0.0, 0.0, -sinx, 0.0, cosx, 0.0, 0.0, 0.0,
    0.0, 1.0
  ];
}

/** Returns a 4x4 affine matrix representing a 3D rotation around the z axis. */
export function Rotate3Z(angle_in_degrees: number): rgi.Matrix44 {
  let sinx = Math.sin(angle_in_degrees * DEGREES_TO_RADIANS);
  let cosx = Math.cos(angle_in_degrees * DEGREES_TO_RADIANS);
  return [
    cosx, -sinx, 0.0, 0.0, sinx, cosx, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0,
    0.0, 1.0
  ];
}

/** Returns a 4x4 affine matrix representing a 3D anisotropic scale. */
export function Scale3(x: number, y: number, z: number): rgi.Matrix44 {
  return [
    x, 0.0, 0.0, 0.0, 0.0, y, 0.0, 0.0, 0.0, 0.0, z, 0.0, 0.0, 0.0, 0.0, 1.0
  ];
}

/** Returns a 3x3 identity matrix, reprenenting an identity 2D affine transform. */
export const Identity2: rgi.Matrix33 =
  [1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0];

/** Returns a 4x4 identity matrix, reprenenting an identity 3D affine transform. */
export const Identity3: rgi.Matrix44 = [
  1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0
];

/** A matrix that transforms a 2D point (x, y) to the 3D point (x, y, 0). */
export let EmbedOnZPlane: rgi.Matrix43 =
  [1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0];

/** Multiplies a 3D vector by an affine transform to produce a new 3D vector. */
export function VMul3(a: rgi.Matrix44, b: rgi.Vec3): rgi.Vec3 {
  return [
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2] + a[3] * 1,
    a[4] * b[0] + a[5] * b[1] + a[6] * b[2] + a[7] * 1,
    a[8] * b[0] + a[9] * b[1] + a[10] * b[2] + a[11] * 1
  ];
}

/** Multiplies a 2D vector by an affine transform to produce a new 2D vector. */
export function VMul2(a: rgi.Matrix33, b: rgi.Vec2): rgi.Vec2 {
  return [
    a[0] * b[0] + a[1] * b[1] + a[2] * 1, a[3] * b[0] + a[4] * b[1] + a[5] * 1
  ];
}

/** Multiplies two 4x4 matrices together to produce a new 4x4 matrix. */
export function MMul4(a: rgi.Matrix44, b: rgi.Matrix44): rgi.Matrix44 {
  return [
    a[0] * b[0] + a[1] * b[4] + a[2] * b[8] + a[3] * b[12],
    a[0] * b[1] + a[1] * b[5] + a[2] * b[9] + a[3] * b[13],
    a[0] * b[2] + a[1] * b[6] + a[2] * b[10] + a[3] * b[14],
    a[0] * b[3] + a[1] * b[7] + a[2] * b[11] + a[3] * b[15],

    a[4] * b[0] + a[5] * b[4] + a[6] * b[8] + a[7] * b[12],
    a[4] * b[1] + a[5] * b[5] + a[6] * b[9] + a[7] * b[13],
    a[4] * b[2] + a[5] * b[6] + a[6] * b[10] + a[7] * b[14],
    a[4] * b[3] + a[5] * b[7] + a[6] * b[11] + a[7] * b[15],

    a[8] * b[0] + a[9] * b[4] + a[10] * b[8] + a[11] * b[12],
    a[8] * b[1] + a[9] * b[5] + a[10] * b[9] + a[11] * b[13],
    a[8] * b[2] + a[9] * b[6] + a[10] * b[10] + a[11] * b[14],
    a[8] * b[3] + a[9] * b[7] + a[10] * b[11] + a[11] * b[15],

    a[12] * b[0] + a[13] * b[4] + a[14] * b[8] + a[15] * b[12],
    a[12] * b[1] + a[13] * b[5] + a[14] * b[9] + a[15] * b[13],
    a[12] * b[2] + a[13] * b[6] + a[14] * b[10] + a[15] * b[14],
    a[12] * b[3] + a[13] * b[7] + a[14] * b[11] + a[15] * b[15],
  ];
}

/** Returns the cross product of two 3D vectors. */
export function Cross(a: rgi.Vec3, b: rgi.Vec3): rgi.Vec3 {
  return [
    a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
    a[0] * b[1] - a[1] * b[0]
  ];
}

/** Returns the dot product of two 2D vectors. */
export function Dot2(a: rgi.Vec2, b: rgi.Vec2): number {
  return a[0] * b[0] + a[1] * b[1];
}

/** Returns the dot product of two 3D vectors. */
export function Dot3(a: rgi.Vec3, b: rgi.Vec3): number {
  return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
}

/** Returns the negation of a 2D vector. */
export function Negate2(x: rgi.Vec2): rgi.Vec2 {
  return [-x[0], -x[1]];
}

/** Returns the negation of a 3D vector. */
export function Negate3(x: rgi.Vec3): rgi.Vec3 {
  return [-x[0], -x[1], -x[2]];
}

/** Multiplies two 3x3 matrices together to produce a new 3x3 matrix. */
export function MMul3(a: rgi.Matrix33, b: rgi.Matrix33): rgi.Matrix33 {
  return [
    a[0] * b[0] + a[1] * b[3] + a[2] * b[6],
    a[0] * b[1] + a[1] * b[4] + a[2] * b[7],
    a[0] * b[2] + a[1] * b[5] + a[2] * b[8],

    a[3] * b[0] + a[4] * b[3] + a[5] * b[6],
    a[3] * b[1] + a[4] * b[4] + a[5] * b[7],
    a[3] * b[2] + a[4] * b[5] + a[5] * b[8],

    a[6] * b[0] + a[7] * b[3] + a[8] * b[6],
    a[6] * b[1] + a[7] * b[4] + a[8] * b[7],
    a[6] * b[2] + a[7] * b[5] + a[8] * b[8],
  ];
}

/** Multiplies a 4x4 matrix with a 4x3 matrix to produce a 4x3 matrix. */
export function MMul443(a: rgi.Matrix44, b: rgi.Matrix43): rgi.Matrix43 {
  return [
    a[0] * b[0] + a[1] * b[3] + a[2] * b[6] + a[3] * b[9],
    a[0] * b[1] + a[1] * b[4] + a[2] * b[7] + a[3] * b[10],
    a[0] * b[2] + a[1] * b[5] + a[2] * b[8] + a[3] * b[11],

    a[4] * b[0] + a[5] * b[3] + a[6] * b[6] + a[7] * b[9],
    a[4] * b[1] + a[5] * b[4] + a[6] * b[7] + a[7] * b[10],
    a[4] * b[2] + a[5] * b[5] + a[6] * b[8] + a[7] * b[11],

    a[8] * b[0] + a[9] * b[3] + a[10] * b[6] + a[11] * b[9],
    a[8] * b[1] + a[9] * b[4] + a[10] * b[7] + a[11] * b[10],
    a[8] * b[2] + a[9] * b[5] + a[10] * b[8] + a[11] * b[11],

    a[12] * b[0] + a[13] * b[3] + a[14] * b[6] + a[15] * b[9],
    a[12] * b[1] + a[13] * b[4] + a[14] * b[7] + a[15] * b[10],
    a[12] * b[2] + a[13] * b[5] + a[14] * b[8] + a[15] * b[11],
  ];
}

/** Multiplies a 4x3 matrix with a 3x3 matrix to produce a 4x3 matrix. */
export function MMul433(a: rgi.Matrix43, b: rgi.Matrix33): rgi.Matrix43 {
  return [
    a[0] * b[0] + a[1] * b[3] + a[2] * b[6],
    a[0] * b[1] + a[1] * b[4] + a[2] * b[7],
    a[0] * b[2] + a[1] * b[5] + a[2] * b[8],

    a[3] * b[0] + a[4] * b[3] + a[5] * b[6],
    a[3] * b[1] + a[4] * b[4] + a[5] * b[7],
    a[3] * b[2] + a[4] * b[5] + a[5] * b[8],

    a[6] * b[0] + a[7] * b[3] + a[8] * b[6],
    a[6] * b[1] + a[7] * b[4] + a[8] * b[7],
    a[6] * b[2] + a[7] * b[5] + a[8] * b[8],

    a[9] * b[0] + a[10] * b[3] + a[11] * b[6],
    a[9] * b[1] + a[10] * b[4] + a[11] * b[7],
    a[9] * b[2] + a[10] * b[5] + a[11] * b[8],
  ];
}

/** Applies a translation to a 3D region to produce a translated 3D region. */
export function TranslatedRegion3(
  x: { source: rgi.Region3; translation: rgi.Vec3; }): rgi.Region3 {
  return rgi.Transform3(
    { source: x.source, transform: Translate3(x.translation) });
}

/** Embeds the 2D region onto the xy plane and then extrudes it by the given height along the z axis. */
export function ExtrudeFromZPlane(
  x: {
    /** The 2D region to be embedded into the xy plane and extruded. */
    source: rgi.Region2;
    /** The height that the input region should be extruded by from the xy plane. */
    height: number;
  }): rgi.Region3 {
  return rgi.Extrude({
    source: x.source,
    transforms: [
      EmbedOnZPlane, MMul443(Translate3([0, 0, x.height]), EmbedOnZPlane)
    ],
    closed: false
  });
}

/** Embeds the 2D region onto the xy plane and then extrudes it by the given height along the z axis, twisting it as it is extruded. */
export function TwistExtrudeFromZPlane(x: {
  /** The 2D region to be embedded into the xy plane and extruded. */
  source: rgi.Region2,
  /** The height that the input region should be extruded by from the xy plane. */
  height: number,
  /** The total amount of twist that should be applied uniformly during the extrusion. */
  twist_amount_in_degrees: number,
  /** The number of slices to interpolate the twist at. The higher this number, the smoother the twist. */
  num_slices: number
}): rgi.Region3 {
  return rgi.Extrude({
    source: x.source,
    transforms: [...Array(x.num_slices).keys()].map(slice => {
      const progress = slice / (x.num_slices - 1);
      return MMul443(
        Translate3([0, 0, progress * x.height]),
        MMul433(
          EmbedOnZPlane,
          Rotate2(progress * x.twist_amount_in_degrees)));
    }),
    closed: false
  });
}

/** Creates a torus in the xy plane. */
export function Torus(x: {
  /** The torus' inner radius in the xy plane. */
  inner_radius: number,
  /** The torus' outer radius in the xy plane. */
  outer_radius: number,
  /** The number of points on the polygonal approximation of a circle in a cross section of the torus. The higher the number, the smoother the torus will appear. */
  num_cross_section_points: number,
  /** The number of radial cross sections on in torus.  The higher the number the smoother the torus will appear. */
  num_slices: number
}): rgi.Region3 {
  let crossSection = rgi.CircleAsPolygon({
    circle: {
      radius: (x.outer_radius - x.inner_radius) / 2,
      center: [(x.outer_radius + x.inner_radius) / 2, 0]
    },
    num_points: x.num_cross_section_points
  });

  return rgi.Extrude({
    source: crossSection,
    transforms: [...Array(x.num_slices).keys()].map(slice => {
      const progress = slice / x.num_slices;
      return MMul443(
        Rotate3Z(progress * 360), MMul443(Rotate3X(90), EmbedOnZPlane));
    }),
    closed: true
  });
}

/** Creates a cylinder along the z axis. */
export function Cylinder(x: {
  /** Radius of the cylinder cross section in the xy plane. */
  radius: number;
  /** Height of the cylinder along the z axis. */
  height: number;
  /** Number of points in the polygonal approximation of a circle cross section. */
  num_points: number;
}): rgi.Region3 {
  return ExtrudeFromZPlane({
    source: rgi.CircleAsPolygon({
      circle: { radius: x.radius, center: [0, 0] },
      num_points: x.num_points
    }),
    height: x.height
  });
}

/** Returns a geodesic sphere, i.e. a subdivided icosahedron. */
export function GeodesicSphere(x: {
  /** Sphere parameters that will be approximated by the geodesic sphere. */
  sphere: rgi.SphereParams;
  /** Number of subdivision iterations. */
  iterations: number;
}): rgi.SubdivideSphere {
  return rgi.SubdivideSphere({
    source: rgi.Icosahedron({ sphere: x.sphere }),
    iterations: x.iterations
  });
}