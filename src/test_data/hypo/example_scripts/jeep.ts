import * as h from 'hypo';

export function Test() {
  return h.RectangleAsMesh2(
      h.Rectangle({left: -50, top: 75, right: 100, bottom: -150}));
  /*
    return h.CircleAsPolygonAsMesh2(h.CircleAsPolygon(
        {circle: {radius: 10, center: [0, 0]}, num_points: 20}));
  */
}

let jeep_wheel = h.Cylinder(0.35, 0.15, 20);

let my_jeep = h.Mesh3UnionAsMesh3(h.Mesh3Union({
  meshes: [
    h.TranslatedMesh3(jeep_wheel, [1.0, 0.0, -0.5]),
    h.TranslatedMesh3(jeep_wheel, [1.0, 0.0, -0.5]),
    h.TranslatedMesh3(jeep_wheel, [1.0, 0.0, 0.5]),
    h.TranslatedMesh3(jeep_wheel, [-1.0, 0.0, 0.5]),
    h.TranslatedMesh3(jeep_wheel, [-1.0, 0.0, -0.5]),
  ]
}));

export function Jeep() {
  return my_jeep;
}
