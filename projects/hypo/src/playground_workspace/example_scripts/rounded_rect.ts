import * as h from 'hypo';

export function Main() {
  return h.Transform2({
    source: h.MinkowskiSum2({
      regions: [
        h.CircleAsPolygon(
            {circle: {radius: 0.1, center: [0, 0]}, num_points: 50}),
        h.Box2({corners: [[-1, 1], [1, -1]]})
      ]
    }),
    transform: h.Rotate2(45)
  });
}

export function Eye(center: h.Vec2) {
  let eye_area = h.Intersection2({
    regions: [
      h.CircleAsPolygon({
        circle: {radius: 10, center: [center[0], center[1] - 7.5]},
        num_points: 100
      }),
      h.CircleAsPolygon({
        circle: {radius: 10, center: [center[0], center[1] + 7.5]},
        num_points: 100
      })
    ]
  });

  let pupil = h.CircleAsPolygon(
      {circle: {radius: 2, center: [center[0], center[1]]}, num_points: 50});

  return h.Difference2({a: eye_area, b: pupil});
}

export function Eyes() {
  return h.Union2({regions: [Eye([10, 10]), Eye([30, 10])]});
}
