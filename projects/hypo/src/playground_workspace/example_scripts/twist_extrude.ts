import * as h from 'hypo';

export function Main() {
  let crossSection = h.Box2({corners: [[-1, -1], [1, 1]]});

  return h.TwistExtrudeFromZPlane({
    source: crossSection,
    height: 8,
    twist_amount_in_degrees: 360,
    num_slices: 64
  });
}
