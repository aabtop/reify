import * as h from 'hypo';

export function Test() {
  return h.RectangleAsRegion2(
      h.Rectangle({left: -50, top: 75, right: 100, bottom: -150}));
  /*
    return h.CircleAsPolygonAsRegion2(h.CircleAsPolygon(
        {circle: {radius: 10, center: [0, 0]}, num_points: 20}));
  */
}

let jeep_wheel = h.Cylinder(0.35, 0.15, 20);

let my_jeep = h.Region3UnionAsRegion3(h.Region3Union({
  regions: [
    h.TranslatedRegion3(jeep_wheel, [1.0, 0.0, -0.5]),
    h.TranslatedRegion3(jeep_wheel, [1.0, 0.0, -0.5]),
    h.TranslatedRegion3(jeep_wheel, [1.0, 0.0, 0.5]),
    h.TranslatedRegion3(jeep_wheel, [-1.0, 0.0, 0.5]),
    h.TranslatedRegion3(jeep_wheel, [-1.0, 0.0, -0.5]),
  ]
}));

export function Jeep() {
  return my_jeep;
}
