import * as h from 'hypo';

export function Main() {
  const width = 3;
  const length = 3;
  const height = 3;
  const wall_thickness = 0.5;

  let house: h.Region3 = h.Difference3({
    a:
      h.Difference3({
        a: h.Box3({
          corners: [
            [0, 0, 0],
            [width, length, height]
          ]
        }),
        b: h.Box3({
          corners: [
            [
              wall_thickness, wall_thickness,
              wall_thickness
            ],
            [
              width - wall_thickness,
              length - wall_thickness,
              height - wall_thickness
            ]
          ]
        })
      }),
    b: h.Box3({
      corners: [
        [
          width * 0.6, length * 0.6,
          height * 0.3
        ],
        [
          width + 1, length + 1,
          height + 1
        ]
      ]
    })
  });


  return house;
}