import * as h from 'hypo';

// I have a chunk of Tungsten that has a simple but somewhat interesting shape
// to it.  It makes for a good modeling test, at least.  This code essentially
// describes the piece.
let height = 7;
let start_clip_height = 6.5;

let base_square = h.Rectangle({left: -1.0, top: 1.0, right: 1.0, bottom: -1.0});

let column = h.ExtrudeFromZPlane({source: base_square, height: height});

let big_square = h.Rectangle({left: -2.0, top: 2.0, right: 2.0, bottom: -2.0});

let subtraction_region = h.Extrude({
  source: big_square,
  transforms: [
    h.MMul443(
        h.Translate3([0, 0, start_clip_height]),
        h.MMul443(h.Rotate3Y(20), h.EmbedOnZPlane)),
    h.MMul443(h.Translate3([0, 0, height + 0.5]), h.EmbedOnZPlane)
  ]
});


export function Tungsten() {
  const sphere = h.GeodesicSphere(
      {sphere: {radius: 0.15, center: [0, 0, 0]}, iterations: 2});

  return h.MinkowskiSum3(
      {regions: [sphere, h.Difference3({a: column, b: subtraction_region})]});
}
