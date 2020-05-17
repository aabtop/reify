import * as rgi from 'reify_generated_interface'
export * from 'reify_generated_interface'

export function Translate3(translation: rgi.Vec3): rgi.Matrix44 {
  return [
    1.0, 0.0, 0.0, translation[0], 0.0, 1.0, 0.0, translation[1], 0.0, 0.0, 1.0,
    translation[2], 0.0, 0.0, 0.0, 1.0
  ];
}

export let Identity3: rgi.Matrix33 =
    [1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0];
export let Identity4: rgi.Matrix44 =
    [1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0,
     1.0];

// A matrix that transforms a 2D point (x, y) to the 3D point (x, y, 0).
export let EmbedOnZPlane: rgi.Matrix43 = [
  1.0, 0.0, 0.0,
  0.0, 1.0, 0.0,
  0.0, 0.0, 0.0,
  0.0, 0.0, 1.0
];

export function MMul4(a: rgi.Matrix44, b: rgi.Matrix44) : rgi.Matrix44 {
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

export function MMul3(a: rgi.Matrix33, b: rgi.Matrix33) : rgi.Matrix33 {
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

export function MMul443(a: rgi.Matrix44, b: rgi.Matrix43) : rgi.Matrix43 {
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

export function MMul433(a: rgi.Matrix43, b: rgi.Matrix33) : rgi.Matrix43 {
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
    region: rgi.Region3, translation: rgi.Vec3): rgi.Region3 {
  return rgi.Transform3AsRegion3(
      rgi.Transform3({source: region, transform: Translate3(translation)}));
}

export function ExtrudeFromZPlane(
    source: rgi.Region2, height: number): rgi.Region3 {
      return rgi.ExtrudeAsRegion3(rgi.Extrude({
        source: source,
        transforms: [EmbedOnZPlane,
                     MMul443(Translate3([0, 0, height]), EmbedOnZPlane)]
      }));
}

export function Cylinder(
    radius: number, height: number, num_points: number): rgi.Region3 {
  return ExtrudeFromZPlane(rgi.CircleAsPolygonAsRegion2(rgi.CircleAsPolygon(
        {circle: {radius: radius, center: [0, 0]}, num_points: num_points})),
        height);
}
