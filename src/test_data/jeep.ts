import * as reify from 'reify';

export function Jeep() {
  let jeep_wheel = reify.Cylinder(0.35, 0.15);

  return reify.Mesh3UnionAsMesh3({
    meshes: [
      reify.TranslatedMesh3(jeep_wheel, [1.0, 0.0, -0.5]),
      reify.TranslatedMesh3(jeep_wheel, [1.0, 0.0, -0.5]),
      reify.TranslatedMesh3(jeep_wheel, [1.0, 0.0, 0.5]),
      reify.TranslatedMesh3(jeep_wheel, [-1.0, 0.0, 0.5]),
      reify.TranslatedMesh3(jeep_wheel, [-1.0, 0.0, -0.5]),
    ]
  });
}
