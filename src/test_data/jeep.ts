function Jeep() {
  let jeep_wheel = Cylinder(0.35, 0.15);

  return reify.MeshUnion([
    TranslatedMesh3(jeep_wheel, [1.0, 0.0, -0.5]),
    TranslatedMesh3(jeep_wheel, [1.0, 0.0, -0.5]),
    TranslatedMesh3(jeep_wheel, [1.0, 0.0, 0.5]),
    TranslatedMesh3(jeep_wheel, [-1.0, 0.0, 0.5]),
    TranslatedMesh3(jeep_wheel, [-1.0, 0.0, -0.5]),
  ]);
}
