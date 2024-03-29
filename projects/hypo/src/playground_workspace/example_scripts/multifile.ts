import * as h from 'hypo';

import * as TungstenModule from 'tungsten';
import * as TorusModule from './torus';
import * as SpiralBeamsModule from 'spiral_beams';

export function Main() {
  return h.Union3({
    regions: [
      TorusModule.Torus(),
      h.Transform3({ source: TorusModule.Torus(), transform: h.Rotate3X(90) }),
      h.Transform3({ source: TungstenModule.Main(), transform: h.Rotate3Y(90) }),
      h.Transform3({ source: SpiralBeamsModule.Main(), transform: h.Translate3([0, 6, 0]) }),
    ]
  });
}