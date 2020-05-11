import * as rci from 'reify_core_interface'
export * from 'reify_core_interface'

export function Translate3D(translation: rci.Vec3): rci.Matrix44 {
  return [
    1.0, 0.0, 0.0, translation[0], 0.0, 1.0, 0.0, translation[1], 0.0, 0.0, 1.0,
    translation[2], 0.0, 0.0, 0.0, 1.0
  ];
}

export function TranslatedMesh3(
    mesh: rci.Mesh3, translation: rci.Vec3): rci.Mesh3 {
  return rci.TransformMesh3AsMesh3(
      rci.TransformMesh3({source: mesh, transform: Translate3D(translation)}));
}

export function Cylinder(radius: number, thickness: number): rci.Mesh3 {
  return rci.ExtrudeMesh2AsMesh3(rci.ExtrudeMesh2({
    source: rci.CircleAsMesh2(rci.Circle({radius: radius, center: [0, 0]})),
    path: [[0, 0, -thickness * 0.5], [0, 0, thickness * 0.5]]
  }));
}
