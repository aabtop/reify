import * as rgi from 'reify_generated_interface'
export * from 'reify_generated_interface'

export function Translate3D(translation: rgi.Vec3): rgi.Matrix44 {
  return [
    1.0, 0.0, 0.0, translation[0], 0.0, 1.0, 0.0, translation[1], 0.0, 0.0, 1.0,
    translation[2], 0.0, 0.0, 0.0, 1.0
  ];
}

export function TranslatedRegion3(
    region: rgi.Region3, translation: rgi.Vec3): rgi.Region3 {
  return rgi.TransformRegion3AsRegion3(rgi.TransformRegion3(
      {source: region, transform: Translate3D(translation)}));
}

export function Cylinder(
    radius: number, thickness: number, num_points: number): rgi.Region3 {
  return rgi.ExtrudeRegion2AsRegion3(rgi.ExtrudeRegion2({
    source: rgi.CircleAsPolygonAsRegion2(rgi.CircleAsPolygon(
        {circle: {radius: radius, center: [0, 0]}, num_points: num_points})),
    path: [[0, 0, -thickness * 0.5], [0, 0, thickness * 0.5]]
  }));
}
