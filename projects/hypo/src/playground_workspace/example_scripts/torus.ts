import * as h from 'hypo';

export function Torus() {
  return h.Torus({
    inner_radius: 2,
    outer_radius: 3,
    num_cross_section_points: 10,
    num_slices: 20
  });
}
