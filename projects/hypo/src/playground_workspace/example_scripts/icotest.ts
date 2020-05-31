import * as h from 'hypo';

export function Main() {
  return h.Union3({
    regions: [
      h.Icosahedron({sphere: {radius: 10, center: [0, 0, 0]}}),
      h.Icosahedron({sphere: {radius: 1, center: [-11, 0, 0]}}),
      h.Icosahedron({sphere: {radius: 20, center: [0, 30, 0]}}),
    ]
  });
}
