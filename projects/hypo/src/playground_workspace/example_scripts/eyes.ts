import * as h from 'hypo';

export function Eye(center: h.Vec2) {
  let eye_area = h.Intersection2({
    regions: [
      h.CircleAsPolygon({
        circle: { radius: 10, center: [center[0], center[1] - 7.5] },
        num_points: 100
      }),
      h.CircleAsPolygon({
        circle: { radius: 10, center: [center[0], center[1] + 7.5] },
        num_points: 100
      })
    ]
  });

  let pupil = h.CircleAsPolygon(
    { circle: { radius: 2, center: [center[0], center[1]] }, num_points: 50 });

  return h.Difference2({ a: eye_area, b: pupil });
}

export function Eyes() {
  return h.Union2({ regions: [Eye([10, 0]), Eye([30, 0])] });
}

export function EyesBoundary() {
  return h.BoundaryOfRegion2({ region: Eyes() });
}

export function WidenedBoundary() {
  return h.WidenBoundary2({ boundary: EyesBoundary(), width: 0.1 });
}

export function ColumnOfColoredEyes() {
  const eyes = Eyes();

  const middleRegion = h.Transform2({ source: eyes, transform: h.Translate2([0, 0]) });

  return h.SvgElements({
    elements: [
      h.SvgPathElementFromRegion2({
        region: h.Transform2({ source: eyes, transform: h.Translate2([0, -15]) }),
        fill: h.SvgSolidColor({ color: [1.0, 0.3, 0.3, 1.0] }),
      }),
      h.SvgPathElementFromRegion2({
        region: middleRegion,
        fill: h.SvgSolidColor({ color: [0.4, 1.0, 0.4, 1.0] }),
      }),
      h.SvgPathElementFromBoundary2({
        boundary: h.BoundaryOfRegion2({ region: middleRegion }),
        stroke: h.SvgSolidColor({ color: [1.0, 0, 0, 1.0] }),
        width: h.SvgPercentage({ value: 1 }),
      }),
      h.SvgPathElementFromRegion2({
        region: h.Transform2({ source: eyes, transform: h.Translate2([0, 15]) }),
        fill: h.SvgSolidColor({ color: [0.3, 0.3, 1.0, 1.0] }),
      }),
    ]
  });
}