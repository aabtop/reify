import * as h from 'hypo';

const beamRadius = 0.25;
const beamLength = 4.0;
const twistIncrementInDegrees = 36;

function Beam(radius: number, length: number): h.Region3 {
  return h.Box3({
    corners: [[-radius, -radius, -length / 2], [radius, radius, length / 2]]
  });
}

function StackHeight(stackSize: number): number {
  return beamRadius * 2.0 * stackSize;
}

function SpiralStack(stackSize: number): h.Region3[] {
  if (stackSize == 0) return [];

  return [
    h.Transform3({
      source: Beam(beamRadius, beamLength),
      transform: h.MMul4(
        h.Translate3([0, 2 * beamRadius * stackSize, 0]),
        h.Rotate3Y(twistIncrementInDegrees * stackSize))
    }),
    ...SpiralStack(stackSize - 1)
  ];
}

export function SingleBeam() {
  return Beam(2, 4);
}

export function Main() {
  const stackSize = 10;

  return h.Transform3({
    source: h.Union3({ regions: SpiralStack(stackSize) }),
    transform: h.Translate3([0, -StackHeight(stackSize) / 2.0, 0])
  });
}
