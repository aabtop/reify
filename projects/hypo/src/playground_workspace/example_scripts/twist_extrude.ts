import * as h from 'hypo';

function TwistExtrudeFromZPlane(params: {
  source: h.Region2,
  height: number,
  twist_amount_in_degrees: number,
  num_layers: number
}): h.Region3 {
  // assert(num_layers >= 2);

  return h.Extrude({
    source: params.source,
    transforms: Array.from({length: params.num_layers}).map((_, layer) => {
      let progress = layer / (params.num_layers - 1);
      return h.MMul443(
          h.Translate3([0, 0, progress * params.height]),
          h.MMul433(
              h.EmbedOnZPlane,
              h.Rotate2(progress * params.twist_amount_in_degrees)));
    })
  });
}

export function Main() {
  const RADIUS = 1;
  const HEIGHT = 8;
  const TWIST = 360;
  const LAYERS = 64;

  let crossSection =
      h.Rectangle({left: -RADIUS, top: RADIUS, right: RADIUS, bottom: -RADIUS});

  return TwistExtrudeFromZPlane({
    source: crossSection,
    height: HEIGHT,
    twist_amount_in_degrees: TWIST,
    num_layers: LAYERS
  });
}
