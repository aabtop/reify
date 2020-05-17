import * as h from 'hypo';

let jeep_wheel = h.Cylinder(0.35, 0.15, 20);

let my_jeep = h.Union3AsRegion3(h.Union3({
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
