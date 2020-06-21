import * as h from 'hypo';

export function Main() {
  const RADIUS = 1;
  const HEIGHT = 8;
  const TWIST = 360;
  const LAYERS = 64;

  let crossSection =
      h.Rectangle({points: [[-RADIUS, RADIUS], [RADIUS, -RADIUS]]});

  return h.TwistExtrudeFromZPlane({
    source: crossSection,
    height: HEIGHT,
    twist_amount_in_degrees: TWIST,
    num_slices: LAYERS
  });
}
