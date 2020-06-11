import * as h from 'hypo';

let jeep_wheel = h.Cylinder({radius: 0.35, height: 0.15, num_points: 20});

let my_jeep = h.Union3({
  regions: [
    h.TranslatedRegion3({source: jeep_wheel, translation: [1.0, 0.0, -0.5]}),
    h.TranslatedRegion3({source: jeep_wheel, translation: [1.0, 0.0, -0.5]}),
    h.TranslatedRegion3({source: jeep_wheel, translation: [1.0, 0.0, 0.5]}),
    h.TranslatedRegion3({source: jeep_wheel, translation: [-1.0, 0.0, 0.5]}),
    h.TranslatedRegion3({source: jeep_wheel, translation: [-1.0, 0.0, -0.5]}),
  ]
});

export function Jeep() {
  return my_jeep;
}
