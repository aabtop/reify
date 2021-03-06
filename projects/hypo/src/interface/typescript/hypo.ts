import * as rgi from 'reify_generated_interface'
export * from 'reify_generated_interface'

const DEGREES_TO_RADIANS = (2 * Math.PI) / 360;

export function Translate2(translation: rgi.Vec2): rgi.Matrix33 {
  return [1.0, 0.0, translation[0], 0.0, 1.0, translation[1], 0.0, 0.0, 1.0];
}

export function Rotate2(angle_in_degrees: number): rgi.Matrix33 {
  let sinx = Math.sin(angle_in_degrees * DEGREES_TO_RADIANS);
  let cosx = Math.cos(angle_in_degrees * DEGREES_TO_RADIANS);
  return [cosx, -sinx, 0.0, sinx, cosx, 0.0, 0.0, 0.0, 1.0];
}

export function Scale2(x: number, y: number): rgi.Matrix33 {
  return [x, 0.0, 0.0, 0.0, y, 0.0, 0.0, 0.0, 1.0];
}

export function Translate3(translation: rgi.Vec3): rgi.Matrix44 {
  return [
    1.0, 0.0, 0.0, translation[0], 0.0, 1.0, 0.0, translation[1], 0.0, 0.0, 1.0,
    translation[2], 0.0, 0.0, 0.0, 1.0
  ];
}

// Rotation around the X axis.
export function Rotate3X(angle_in_degrees: number): rgi.Matrix44 {
  let sinx = Math.sin(angle_in_degrees * DEGREES_TO_RADIANS);
  let cosx = Math.cos(angle_in_degrees * DEGREES_TO_RADIANS);
  return [
    1.0, 0.0, 0.0, 0.0, 0.0, cosx, -sinx, 0.0, 0.0, sinx, cosx, 0.0, 0.0, 0.0,
    0.0, 1.0
  ];
}

// Rotation around the Y axis.
export function Rotate3Y(angle_in_degrees: number): rgi.Matrix44 {
  let sinx = Math.sin(angle_in_degrees * DEGREES_TO_RADIANS);
  let cosx = Math.cos(angle_in_degrees * DEGREES_TO_RADIANS);
  return [
    cosx, 0.0, sinx, 0.0, 0.0, 1.0, 0.0, 0.0, -sinx, 0.0, cosx, 0.0, 0.0, 0.0,
    0.0, 1.0
  ];
}

// Rotation around the Z axis.
export function Rotate3Z(angle_in_degrees: number): rgi.Matrix44 {
  let sinx = Math.sin(angle_in_degrees * DEGREES_TO_RADIANS);
  let cosx = Math.cos(angle_in_degrees * DEGREES_TO_RADIANS);
  return [
    cosx, -sinx, 0.0, 0.0, sinx, cosx, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0,
    0.0, 1.0
  ];
}

export function Scale3(x: number, y: number, z: number): rgi.Matrix44 {
  return [
    x, 0.0, 0.0, 0.0, 0.0, y, 0.0, 0.0, 0.0, 0.0, z, 0.0, 0.0, 0.0, 0.0, 1.0
  ];
}

export const Identity2: rgi.Matrix33 =
    [1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0];
export const Identity3: rgi.Matrix44 = [
  1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0
];

// A matrix that transforms a 2D point (x, y) to the 3D point (x, y, 0).
export let EmbedOnZPlane: rgi.Matrix43 =
    [1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0];

export function VMul3(a: rgi.Matrix44, b: rgi.Vec3): rgi.Vec3 {
  return [
    a[0] * b[0] + a[1] * b[1] + a[2] * b[2] + a[3] * 1,
    a[4] * b[0] + a[5] * b[1] + a[6] * b[2] + a[7] * 1,
    a[8] * b[0] + a[9] * b[1] + a[10] * b[2] + a[11] * 1
  ];
}

export function VMul2(a: rgi.Matrix33, b: rgi.Vec2): rgi.Vec2 {
  return [
    a[0] * b[0] + a[1] * b[1] + a[2] * 1, a[3] * b[0] + a[4] * b[1] + a[5] * 1
  ];
}

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

export function Cross(a: rgi.Vec3, b: rgi.Vec3): rgi.Vec3 {
  return [
    a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2],
    a[0] * b[1] - a[1] * b[0]
  ];
}

export function Dot2(a: rgi.Vec2, b: rgi.Vec2): number {
  return a[0] * b[0] + a[1] * b[1];
}

export function Dot3(a: rgi.Vec3, b: rgi.Vec3): number {
  return a[0] * b[0] + a[1] * b[1] + a[2] * b[2];
}

export function Negate2(x: rgi.Vec2): rgi.Vec2 {
  return [-x[0], -x[1]];
}

export function Negate3(x: rgi.Vec3): rgi.Vec3 {
  return [-x[0], -x[1], -x[2]];
}

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

export function TranslatedRegion3(
    params: {source: rgi.Region3; translation: rgi.Vec3;}): rgi.Region3 {
  return rgi.Transform3(
      {source: params.source, transform: Translate3(params.translation)});
}

export function ExtrudeFromZPlane(
    params: {source: rgi.Region2; height: number;}): rgi.Region3 {
  return rgi.Extrude({
    source: params.source,
    transforms: [
      EmbedOnZPlane, MMul443(Translate3([0, 0, params.height]), EmbedOnZPlane)
    ],
    closed: false
  });
}

export function TwistExtrudeFromZPlane(params: {
  source: rgi.Region2,
  height: number,
  twist_amount_in_degrees: number,
  num_slices: number
}): rgi.Region3 {
  // assert(num_slices >= 2);

  return rgi.Extrude({
    source: params.source,
    transforms: [...Array(params.num_slices).keys()].map(slice => {
      const progress = slice / (params.num_slices - 1);
      return MMul443(
          Translate3([0, 0, progress * params.height]),
          MMul433(
              EmbedOnZPlane,
              Rotate2(progress * params.twist_amount_in_degrees)));
    }),
    closed: false
  });
}

export function RotateExtrudeAroundZAxis(params: {
  source: rgi.Region2,
  num_slices: number,
}): rgi.Region3 {
  return rgi.Extrude({
    source: params.source,
    transforms: [...Array(params.num_slices).keys()].map(slice => {
      const progress = slice / params.num_slices;
      return MMul443(
          Rotate3Z(progress * 360), MMul443(Rotate3X(90), EmbedOnZPlane));
    }),
    closed: true
  });
}

export function Torus(params: {
  inner_radius: number,
  outer_radius: number,
  num_cross_section_points: number,
  num_slices: number
}): rgi.Region3 {
  let crossSection = rgi.CircleAsPolygon({
    circle: {
      radius: (params.outer_radius - params.inner_radius) / 2,
      center: [(params.outer_radius + params.inner_radius) / 2, 0]
    },
    num_points: params.num_cross_section_points
  });

  return RotateExtrudeAroundZAxis(
      {source: crossSection, num_slices: params.num_slices});
}

export function Cylinder(params: {
  radius: number; height: number; num_points: number;
}): rgi.Region3 {
  return ExtrudeFromZPlane({
    source: rgi.CircleAsPolygon({
      circle: {radius: params.radius, center: [0, 0]},
      num_points: params.num_points
    }),
    height: params.height
  });
}

export function GeodesicSphere(params: {
  sphere: rgi.SphereParams; iterations: number;
}): rgi.SubdivideSphere {
  return rgi.SubdivideSphere({
    source: rgi.Icosahedron({sphere: params.sphere}),
    iterations: params.iterations
  });
}