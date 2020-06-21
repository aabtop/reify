import * as h from 'hypo';

function Beam(radius: number, length: number): h.Region3 {
  let crossSection =
      h.Rectangle({points: [[-radius, radius], [radius, -radius]]});
  return h.TranslatedRegion3({
    source: h.ExtrudeFromZPlane({source: crossSection, height: length}),
    translation: [0, 0, -length / 2]
  });
}

function SpiralStack(stackSize: number): h.Region3[] {
  if (stackSize == 0) return [];

  const beamRadius = 5.0;
  const beamLength = 80.0;
  const twistIncrementInDegrees = 36;

  return [
    h.Transform3({
      source: Beam(beamRadius, beamLength),
      transform: h.MMul4(
          h.Translate3([2 * beamRadius * stackSize, 0, 0]),
          h.Rotate3X(twistIncrementInDegrees * stackSize))
    }),
    ...SpiralStack(stackSize - 1)
  ];
}

export function Main() {
  return h.Union3({regions: SpiralStack(10)});
}
