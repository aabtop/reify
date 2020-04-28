"use strict";
var reify;
(function (reify) {
  function CircleAsMesh(p0) {
    return { __kind: "CircleAsMesh", p0: p0 };
  }
  reify.CircleAsMesh = CircleAsMesh;
  function RectangleAsMesh(p0) {
    return { __kind: "RectangleAsMesh", p0: p0 };
  }
  reify.RectangleAsMesh = RectangleAsMesh;
  function ExtrudeMesh2AsMesh(p0) {
    return { __kind: "ExtrudeMesh2AsMesh", p0: p0 };
  }
  reify.ExtrudeMesh2AsMesh = ExtrudeMesh2AsMesh;
  function TransformMesh3AsMesh(p0) {
    return { __kind: "TransformMesh3AsMesh", p0: p0 };
  }
  reify.TransformMesh3AsMesh = TransformMesh3AsMesh;
  function MeshUnion(p0) {
    return { __kind: "MeshUnion", p0: p0 };
  }
  reify.MeshUnion = MeshUnion;
})(reify || (reify = {})); // namespace reify
function Translate3D(translation) {
  return [
    1.0, 0.0, 0.0, translation[0],
    0.0, 1.0, 0.0, translation[1],
    0.0, 0.0, 1.0, translation[2],
    0.0, 0.0, 0.0, 1.0
  ];
}
function TranslatedMesh3(mesh, translation) {
  return reify.TransformMesh3AsMesh({ source: mesh, transform: Translate3D(translation) });
}
function Cylinder(radius, thickness) {
  return reify.ExtrudeMesh2AsMesh({
    source: reify.CircleAsMesh({ radius: radius, center: [0, 0] }),
    path: [[0, 0, -thickness * 0.5], [0, 0, thickness * 0.5]]
  });
}
function Jeep() {
  let jeep_wheel = Cylinder(0.35, 0.15);
  return reify.MeshUnion([
    TranslatedMesh3(jeep_wheel, [1.0, 0.0, -0.5]),
    TranslatedMesh3(jeep_wheel, [1.0, 0.0, 0.5]),
    TranslatedMesh3(jeep_wheel, [-1.0, 0.0, 0.5]),
    TranslatedMesh3(jeep_wheel, [-1.0, 0.0, -0.5]),
  ]);
}
