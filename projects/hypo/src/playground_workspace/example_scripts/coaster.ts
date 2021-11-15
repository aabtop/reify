import * as h from 'hypo';

function PolygonOutline(n: number, width: number, transform: h.Matrix33): h.Region2 {
  return h.WidenBoundary2({
    boundary: h.BoundaryOfRegion2({
      region: h.Transform2({
        source: h.CircleAsPolygon({
          circle: h.Circle({ center: [0, 0], radius: 1 }),
          num_points: n,
        }), transform: transform
      })
    }),
    width: width,
  })
}

interface SymetricNGonSetParameters {
  n: number,
  thickness: number,
  offset: number,
  ngon_count: number,
};


function MakeSymmetricNGonSet(n: number, ngon_count: number, thickness: number, offset: number, rotation_offset: number): h.Region2 {
  function NGonList(ngons_left: number): h.Region2[] {
    if (ngons_left == 0) {
      return []
    }

    let rotation_amount = rotation_offset + (360 / ngon_count) * (ngon_count - ngons_left);

    return [
      PolygonOutline(n, thickness, h.MMul3(h.Rotate2(rotation_amount), h.Translate2([offset, 0]))),
      ...NGonList(ngons_left - 1),
    ]
  }

  return h.Union2({ regions: NGonList(ngon_count) });
}

export function CoasterBoundary() {
  return h.BoundaryOfRegion2({ region: Coaster() });
}

export function Coaster() {
  return h.Union2({
    regions: [
      MakeSymmetricNGonSet(5, 4, 0.1, 1, 0),
      MakeSymmetricNGonSet(5, 4, 0.08, 1, 45),
    ],
  });
}

export function Extruded() {
  return h.ExtrudeFromZPlane({ source: Coaster(), height: 0.15 })
}