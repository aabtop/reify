import * as rgi from 'reify_generated_interface'
export * from 'reify_generated_interface'

export function Translate3D(translation: rgi.Vec3): rgi.Matrix44 {
  return [
    1.0, 0.0, 0.0, translation[0], 0.0, 1.0, 0.0, translation[1], 0.0, 0.0, 1.0,
    translation[2], 0.0, 0.0, 0.0, 1.0
  ];
}

export function TranslatedMesh3(
    mesh: rgi.Mesh3, translation: rgi.Vec3): rgi.Mesh3 {
  return rgi.TransformMesh3AsMesh3(
      rgi.TransformMesh3({source: mesh, transform: Translate3D(translation)}));
}

export function Cylinder(
    radius: number, thickness: number, num_points: number): rgi.Mesh3 {
  return rgi.ExtrudeMesh2AsMesh3(rgi.ExtrudeMesh2({
    source: rgi.CircleAsPolygonAsMesh2(rgi.CircleAsPolygon(
        {circle: {radius: radius, center: [0, 0]}, num_points: num_points})),
    path: [[0, 0, -thickness * 0.5], [0, 0, thickness * 0.5]]
  }));
}
