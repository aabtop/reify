import * as h from 'hypo';

import * as p from './pillar';

function Platform(inner_length: number, inner_width: number, height: number, num_steps: number) {
  const STEP_LENGTH_DELTA = 5;
  const STEP_WIDTH_DELTA = 5;

  const base_layer = (length: number, width: number, height: number) => {
    return h.ExtrudeFromZPlane({
      source: h.Box2({
        corners: [[-length / 2, -width / 2], [length / 2, width / 2]]
      }),
      height: height,
    });
  };

  const step_height = height / num_steps;
  return h.Union3(
    {
      regions:
        [...Array(num_steps).keys()].map((i) => {
          return base_layer(
            inner_length + STEP_LENGTH_DELTA * i,
            inner_width + STEP_WIDTH_DELTA * i,
            (num_steps - i) * step_height
          )
        })
    });
}

export function ExamplePlatform() {
  const EXAMPLE_PLATFORM_INNER_LENGTH = 60;
  const EXAMPLE_PLATFORM_INNER_WIDTH = 40;
  const EXAMPLE_PLATFORM_HEIGHT = 15;
  const EXAMPLE_PLATFORM_NUM_STEPS = 4;

  return Platform(
    EXAMPLE_PLATFORM_INNER_LENGTH,
    EXAMPLE_PLATFORM_INNER_WIDTH,
    EXAMPLE_PLATFORM_HEIGHT,
    EXAMPLE_PLATFORM_NUM_STEPS);
}

export function Temple() {
  const PILLAR_HEIGHT = 40;
  const PLATFORM_INNER_WIDTH = 60;
  const PLATFORM_INNER_LENGTH = 100;
  const PLATFORM_HEIGHT = 8;
  const PLATFORM_NUM_STEPS = 4;
  const NUM_PILLARS_ALONG_LENGTH = 6;
  const NUM_PILLARS_ALONG_WIDTH = 2;
  const PILLARS_ALONG_WIDTH_PADDING = 8;
  const PILLARS_ALONG_LENGTH_PADDING = 8;
  const CEILING_HEIGHT = 4;
  const CEILING_NUM_STEPS = 2;
  const TEMPLE_HEIGHT = PILLAR_HEIGHT + PLATFORM_HEIGHT + CEILING_HEIGHT;

  const transformed_pillar = (position: [number, number]) => {
    return h.Transform3({
      transform: h.Translate3([position[0], position[1], PLATFORM_HEIGHT]),
      source: p.Pillar(PILLAR_HEIGHT)
    });
  };

  const pillars = [...Array(NUM_PILLARS_ALONG_LENGTH).keys()].map((xInd) => {
    return [...Array(NUM_PILLARS_ALONG_WIDTH).keys()].map((yInd) => {
      const x = (PLATFORM_INNER_LENGTH - 2 * PILLARS_ALONG_LENGTH_PADDING) * (xInd / (NUM_PILLARS_ALONG_LENGTH - 1) - 0.5);
      const y = (PLATFORM_INNER_WIDTH - 2 * PILLARS_ALONG_WIDTH_PADDING) * (yInd / (NUM_PILLARS_ALONG_WIDTH - 1) - 0.5);
      return transformed_pillar([x, y]);
    });
  }).flat();

  const ceilingPlatform = h.Transform3({
    source: Platform(PLATFORM_INNER_LENGTH, PLATFORM_INNER_WIDTH, CEILING_HEIGHT, CEILING_NUM_STEPS),
    transform: h.MMul4(h.Translate3([0, 0, TEMPLE_HEIGHT]), h.Rotate3X(180)),
  });

  const temple = h.Union3({
    regions: [
      Platform(PLATFORM_INNER_LENGTH, PLATFORM_INNER_WIDTH, PLATFORM_HEIGHT, PLATFORM_NUM_STEPS),
      ...pillars,
      ceilingPlatform,
    ]
  });

  return temple;
}
