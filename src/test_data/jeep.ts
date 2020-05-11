import * as reify from 'reify';

let jeep_wheel = reify.Cylinder(0.35, 0.15);

let my_jeep = reify.Mesh3UnionAsMesh3(reify.NewMesh3Union({
  meshes: [
    reify.TranslatedMesh3(jeep_wheel, [1.0, 0.0, -0.5]),
    reify.TranslatedMesh3(jeep_wheel, [1.0, 0.0, -0.5]),
    reify.TranslatedMesh3(jeep_wheel, [1.0, 0.0, 0.5]),
    reify.TranslatedMesh3(jeep_wheel, [-1.0, 0.0, 0.5]),
    reify.TranslatedMesh3(jeep_wheel, [-1.0, 0.0, -0.5]),
  ]
}));

export function Jeep() {
  return my_jeep;
}
