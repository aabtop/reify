import * as reify from 'reify';

function Jeep(): reify.Mesh2 {
  let jeep_wheel = reify.Cylinder(0.35, 0.15);

  return reify.MeshUnion([
    reify.TranslatedMesh3(jeep_wheel, [1.0, 0.0, -0.5]),
    reify.TranslatedMesh3(jeep_wheel, [1.0, 0.0, -0.5]),
    reify.TranslatedMesh3(jeep_wheel, [1.0, 0.0, 0.5]),
    reify.TranslatedMesh3(jeep_wheel, [-1.0, 0.0, 0.5]),
    reify.TranslatedMesh3(jeep_wheel, [-1.0, 0.0, -0.5]),
  ]);
}
