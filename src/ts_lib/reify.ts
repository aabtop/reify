function Translate3D(translation: reify.Vec3): reify.Matrix44 {
  return [
    1.0, 0.0, 0.0, translation[0],
    0.0, 1.0, 0.0, translation[1],
    0.0, 0.0, 1.0, translation[2],
    0.0, 0.0, 0.0, 1.0];
}

function TranslatedMesh3(mesh: reify.Mesh3, translation: reify.Vec3): reify.Mesh3 {
  return reify.TransformMesh3AsMesh({ source: mesh, transform: Translate3D(translation) });
}

function Cylinder(radius: number, thickness: number): reify.Mesh3 {
  return reify.ExtrudeMesh2AsMesh({
    source: reify.CircleAsMesh({ radius: radius, center: [0, 0] }),
    path: [[0, 0, -thickness * 0.5], [0, 0, thickness * 0.5]]
  });
}
