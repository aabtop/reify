import * as h from 'hypo';

export function CrossSection() {
  const CROSS_SECTION_CIRCLE_RADIUS = 0.4;
  const CROSS_SECTION_CIRCLE_POINTS = 12;
  const CROSS_SECTION_RADIUS = 0.5;
  const NUM_CROSS_SECTION_PIECES = 3;

  // Define the basic shape for each fiber of the ring structure.
  let crossSectionPiece = h.CircleAsPolygon({
    circle: { radius: CROSS_SECTION_CIRCLE_RADIUS, center: [0, 0] },
    num_points: CROSS_SECTION_CIRCLE_POINTS
  });

  return h.Union2({
    regions: Array.from({ length: NUM_CROSS_SECTION_PIECES }).map((_, i) => {
      return h.Transform2({
        source: crossSectionPiece,
        // Move each cross section piece out from the center and then rotate the
        // piece around the center of the cross section depending on how many
        // cross section pieces there are.
        transform: h.MMul3(
          h.Rotate2(i / NUM_CROSS_SECTION_PIECES * 360),
          h.Translate2([CROSS_SECTION_RADIUS, 0]))
      });
    })
  });

}

export function Main() {
  const NUM_SLICES = 60;
  const RADIUS = 5;
  const NUM_TWISTS = 3;

  return h.Extrude({
    source: CrossSection(),
    transforms: Array.from({ length: NUM_SLICES }).map((_, slice) => {
      const progress = slice / (NUM_SLICES - 1);
      return h.MMul443(
        // Rotate each slice around the center of the ring.
        h.Rotate3Z(progress * 360),
        h.MMul443(
          // Flip the ring cross section from living in the Z plane to
          // instead live on the Y plane.
          h.Rotate3X(90),
          h.MMul433(
            h.EmbedOnZPlane,
            // Position the cross section away from the center of the
            // ring.
            h.MMul3(
              h.Translate2([RADIUS, 0]),
              h.Rotate2(progress * NUM_TWISTS * 360)))));
    }),
    closed: true
  });
}
