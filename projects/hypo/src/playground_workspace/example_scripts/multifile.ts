import * as h from 'hypo';

import * as TorusModule from './torus';

export function Main() {
  return h.Union3({
    regions: [
      TorusModule.Main(),
      h.Transform3({source: TorusModule.Main(), transform: h.Rotate3X(90)}),
      h.Transform3({source: TorusModule.Main(), transform: h.Rotate3Y(90)}),
    ]
  });
}