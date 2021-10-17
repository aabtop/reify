import * as h from 'hypo';

export function Base() {
  const OUTER_RADIUS = 6;
  const INNER_RADIUS = 5;
  const NUM_POINTS = 20;
  const STEP_HEIGHT = 1;

  const base_layer = (radius: number, height: number) => {
    return h.ExtrudeFromZPlane({
      source: h.CircleAsPolygon({
        circle: h.Circle({
          radius: radius,
          center: [0, 0],
        }),
        num_points: NUM_POINTS,
      }),
      height: height,
    });
  };

  return h.Union3(
    {
      regions: [
        base_layer(OUTER_RADIUS, STEP_HEIGHT),
        base_layer(INNER_RADIUS, STEP_HEIGHT * 2),
      ]
    }
  );
}

export function CoreCrossSection() {
  const RADIUS = 4;
  const NUM_POINTS = 20;
  const NUM_DIVETS = 20;

  const NUM_POINTS_IN_DIVET = 10;
  const DIVET_RADIUS = 0.68;
  const DIVET_OFFSET = 0.23;

  const SMOOTHING_CIRCLE_RADIUS = 0.15;
  const SMOOTHING_CIRCLE_NUM_POINTS = 5;

  const base = h.CircleAsPolygon({
    circle: h.Circle({
      radius: RADIUS,
      center: [0, 0],
    }),
    num_points: NUM_POINTS,
  });

  const divets = [...Array(NUM_DIVETS).keys()].map((i) => {
    const divetAngleInRadians = (i / NUM_DIVETS) * Math.PI * 2;
    return h.Transform2({
      source: h.CircleAsPolygon({
        circle: {
          radius: DIVET_RADIUS,
          center: [0, 0],
        },
        num_points: NUM_POINTS_IN_DIVET,
      }),
      transform: h.MMul3(h.Rotate2(divetAngleInRadians * 180.0 / Math.PI), h.Translate2([RADIUS + DIVET_OFFSET, 0]))
    });
  });

  const sharp = h.Difference2({ a: base, b: h.Union2({ regions: divets }) });

  return sharp;
}

function Core(height: number) {
  return h.ExtrudeFromZPlane({
    source: CoreCrossSection(),
    height: height,
  });
}

export function ExampleCore() {
  const EXAMPLE_CORE_HEIGHT = 20;
  return Core(EXAMPLE_CORE_HEIGHT);
}

export function Pillar(height: number) {
  return h.Union3({
    regions: [
      Base(),
      Core(height),
      h.Transform3({
        source: Base(),
        transform: h.MMul4(h.Translate3([0, 0, height]), h.Rotate3X(180))
      }),
    ]
  });
}

export function ExamplePillar() {
  const EXAMPLE_HEIGHT = 50;

  return Pillar(EXAMPLE_HEIGHT);
}
