import * as h from 'hypo';

export function Main() {
  return h.Transform2({
    source: h.MinkowskiSum2({
      regions: [
        h.CircleAsPolygon(
          { circle: { radius: 0.1, center: [0, 0] }, num_points: 50 }),
        h.Box2({ corners: [[-1, 1], [1, -1]] })
      ]
    }),
    transform: h.Rotate2(45)
  });
}
