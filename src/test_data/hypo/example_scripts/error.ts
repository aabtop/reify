import * as hypo from 'hypo';

function Jeep(): hypo.Mesh2 {
  let jeep_wheel = hypo.Cylinder(0.35, 0.15);

  return hypo.Mesh3UnionAsMesh3({
    meshes: [
      hypo.TranslatedMesh3(jeep_wheel, [1.0, 0.0, -0.5]),
      hypo.TranslatedMesh3(jeep_wheel, [1.0, 0.0, -0.5]),
      hypo.TranslatedMesh3(jeep_wheel, [1.0, 0.0, 0.5]),
      hypo.TranslatedMesh3(jeep_wheel, [-1.0, 0.0, 0.5]),
      hypo.TranslatedMesh3(jeep_wheel, [-1.0, 0.0, -0.5]),
    ]
  });
}
