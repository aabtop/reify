import * as h from 'hypo';

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
  let rect = h.Rectangle({left: 0, top: 20, right: 40, bottom: 0});

  let eyes = h.Union2({regions: [Eye([10, 10]), Eye([30, 10])]});
  return h.Difference2({a: rect, b: eyes});
}
