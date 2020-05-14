import * as hypo from 'hypo';

let jeep_wheel = hypo.Cylinder(0.35, 0.15);

let my_jeep = hypo.Mesh3UnionAsMesh3(hypo.Mesh3Union({
  meshes: [
    hypo.TranslatedMesh3(jeep_wheel, [1.0, 0.0, -0.5]),
    hypo.TranslatedMesh3(jeep_wheel, [1.0, 0.0, -0.5]),
    hypo.TranslatedMesh3(jeep_wheel, [1.0, 0.0, 0.5]),
    hypo.TranslatedMesh3(jeep_wheel, [-1.0, 0.0, 0.5]),
    hypo.TranslatedMesh3(jeep_wheel, [-1.0, 0.0, -0.5]),
  ]
}));

export function Jeep() {
  return my_jeep;
}
