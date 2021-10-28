import * as h from 'hypo';

// Authors: Andrew and Stavros (https://stavros.io)

interface Window {
  wall_index: number;
  x_position: number;
}
interface Door {
  wall_index: number;
  x_position: number;
}
interface Wall {
  origin: h.Vec3;
  orientation: h.Vec3;
}

interface HouseParams {
  width: number;
  length: number;
  height: number;
  wall_thickness: number;
  window_width: number;
  door_width: number;
  door_height: number;
  door_distance_from_ground: number;
  window_distance_from_ground: number;
  window_frame_thickness: number;
  window_frame_width: number;
  roof_height: number;
}

class House {
  params: HouseParams;

  walls: Wall[];
  windows: Window[];
  doors: Door[];

  constructor(params: HouseParams) {
    this.params = params;

    this.walls = [
      { origin: [0, 0, 0], orientation: [0, -1, 0] },
      { origin: [this.params.width, 0, 0], orientation: [1, 0, 0] },
      {
        origin: [this.params.width, this.params.length, 0],
        orientation: [0, 1, 0]
      },
      { origin: [0, this.params.length, 0], orientation: [-1, 0, 0] },
    ];

    this.windows = [];
    this.doors = []
  }

  wallToHouseCoordinates(wall_index: number, coords: h.Vec3): h.Vec3 {
    const wall_origin = this.walls[wall_index].origin;
    const outward: h.Vec3 = this.walls[wall_index].orientation;
    const up: h.Vec3 = [0, 0, 1];
    const right: h.Vec3 = h.Negate3(h.Cross(outward, up));

    // clang-format off
    const transform: h.Matrix44 = [
      right[0], up[0], outward[0], wall_origin[0],
      right[1], up[1], outward[1], wall_origin[1],
      right[2], up[2], outward[2], wall_origin[2],
      0, 0, 0, 1,
    ];
    // clang-format on

    return h.VMul3(transform, coords);
  }
  wallToHouseCorners(wall_index: number, corners: [h.Vec3, h.Vec3]):
    [h.Vec3, h.Vec3] {
    return [
      this.wallToHouseCoordinates(wall_index, corners[0]),
      this.wallToHouseCoordinates(wall_index, corners[1])
    ];
  }

  addWindow(wall_index: number, x_position: number) {
    this.windows.push({ wall_index: wall_index, x_position: x_position })
  }

  addDoor(wall_index: number, x_position: number) {
    this.doors.push({ wall_index: wall_index, x_position: x_position })
  }

  buildFrameWithHole(
    wall_index: number,
    hole_corners: [h.Vec2, h.Vec2],
    frame_thickness: number,
    frame_width: number,
    house: h.Region3,
  ): h.Region3 {
    let self = this;

    let hole_corners_3: [h.Vec3, h.Vec3] = [
      [hole_corners[0][0], hole_corners[0][1], frame_thickness],
      [
        hole_corners[1][0], hole_corners[1][1],
        -frame_thickness - this.params.wall_thickness
      ],
    ];
    let frame_corners: [h.Vec3, h.Vec3] = [
      [
        hole_corners_3[0][0] - frame_width, hole_corners_3[0][1] - frame_width,
        hole_corners_3[0][2]
      ],
      [
        hole_corners_3[1][0] + frame_width, hole_corners_3[1][1] + frame_width,
        hole_corners_3[1][2]
      ]
    ];
    let window_frame =
      h.Box3({ corners: this.wallToHouseCorners(wall_index, frame_corners) });
    return h.Difference3({
      a: h.Union3({ regions: [house, window_frame] }),
      b: h.Box3({ corners: this.wallToHouseCorners(wall_index, hole_corners_3) })
    });
  }

  buildDoor(door: Window, house: h.Region3): h.Region3 {
    const FRAME_THICKNESS = 0.04;
    const FRAME_WIDTH = 0.1;

    return this.buildFrameWithHole(
      door.wall_index,
      [
        [door.x_position, this.params.door_distance_from_ground],
        [
          door.x_position + this.params.door_width,
          this.params.door_distance_from_ground + this.params.door_height
        ],
      ],
      FRAME_THICKNESS,
      FRAME_WIDTH,
      house,
    );
  }

  buildWindow(window: Window, house: h.Region3): h.Region3 {
    const WINDOW_CROSS_WIDTH = 0.1;
    const WINDOW_CROSS_RADIUS = WINDOW_CROSS_WIDTH / 2;
    const x_mid = window.x_position + this.params.window_width / 2;
    const y_mid =
      this.params.window_distance_from_ground + this.params.window_width / 2;
    const z_mid = -this.params.wall_thickness / 2;
    const x_end = window.x_position + this.params.window_width;
    const y_end =
      this.params.window_distance_from_ground + this.params.window_width;

    let cross_vertical_beam = h.Box3({
      corners: this.wallToHouseCorners(
        window.wall_index,
        [
          [
            x_mid - WINDOW_CROSS_RADIUS,
            this.params.window_distance_from_ground,
            z_mid - WINDOW_CROSS_RADIUS
          ],
          [x_mid + WINDOW_CROSS_RADIUS, y_end, z_mid + WINDOW_CROSS_RADIUS]
        ])
    });
    let cross_horizontal_beam = h.Box3({
      corners: this.wallToHouseCorners(
        window.wall_index,
        [
          [
            window.x_position, y_mid - WINDOW_CROSS_RADIUS,
            z_mid - WINDOW_CROSS_RADIUS
          ],
          [x_end, y_mid + WINDOW_CROSS_RADIUS, z_mid + WINDOW_CROSS_RADIUS]
        ])
    });

    return h.Union3({
      regions: [
        this.buildFrameWithHole(
          window.wall_index,
          [
            [window.x_position, this.params.window_distance_from_ground],
            [x_end, y_end]
          ],
          this.params.window_frame_thickness, this.params.window_frame_width,
          house),
        cross_vertical_beam,
        cross_horizontal_beam,
      ]
    });
  }

  buildRoof(): h.Region3 {
    let roof_cross_section = h.Polygon({
      path: [
        [0, 0], [this.params.width / 2, this.params.roof_height],
        [this.params.width, 0]
      ]
    });

    return h.Extrude({
      closed: false,
      source: roof_cross_section,
      transforms: [
        h.MMul443(
          h.MMul4(h.Translate3([0, 0, this.params.height]), h.Rotate3X(90)),
          h.EmbedOnZPlane),
        h.MMul443(
          h.MMul4(
            h.Translate3([0, this.params.length, this.params.height]),
            h.Rotate3X(90)),
          h.EmbedOnZPlane)
      ]
    });
  }

  build(): h.Region3 {
    let house: h.Region3 = h.Difference3({
      a: h.Union3({
        regions: [
          h.Difference3({
            a: h.Box3({
              corners: [
                [0, 0, 0],
                [this.params.width, this.params.length, this.params.height]
              ]
            }),
            b: h.Box3({
              corners: [
                [
                  this.params.wall_thickness, this.params.wall_thickness,
                  this.params.wall_thickness
                ],
                [
                  this.params.width - this.params.wall_thickness,
                  this.params.length - this.params.wall_thickness,
                  this.params.height - this.params.wall_thickness
                ]
              ]
            })
          }),
          this.buildRoof(),
        ]
      }),
      b: h.Box3({
        corners: [
          [
            this.params.width * 0.8, this.params.length * 0.8,
            this.params.height * 0.3
          ],
          [
            this.params.width + 1, this.params.length + 1,
            this.params.height + this.params.roof_height + 1
          ]
        ]
      })
    });

    for (let window of this.windows) {
      house = this.buildWindow(window, house);
    }

    for (let door of this.doors) {
      house = this.buildDoor(door, house);
    }

    return house;
  }
}

export function SingleHouse() {
  let house = new House({
    width: 7,
    length: 9,
    height: 3,
    wall_thickness: 0.2,
    window_width: 1,
    door_width: 1.3,
    door_height: 2,
    door_distance_from_ground: 0.2,
    window_distance_from_ground: 1,
    window_frame_thickness: 0.04,
    window_frame_width: 0.1,
    roof_height: 2,
  });

  house.addWindow(0, 3);
  house.addWindow(1, 1);
  house.addWindow(1, 3.5);
  house.addDoor(1, 6);
  house.addWindow(2, 3);
  house.addWindow(3, 3);
  house.addWindow(3, 1);
  house.addWindow(3, 6);

  return h.Transform3({ source: house.build(), transform: h.Rotate3X(0) });
}

export function RowOfHouses() {
  const house = SingleHouse();
  const NUM_HOUSES = 6;
  const SPACING_BETWEEN_HOUSES = 10.0;
  const COLORS: h.Vec3[] = [
    [1, 0.3, 0.3],
    [0.4, 1, 0.4],
    [0.2, 0.2, 1],
    [1, 0.4, 1],
    [1, 1, 0.4],
    [0.3, 1, 1],
  ];

  const house_x_positions_and_colors: [number, h.Vec3][] = [...Array(NUM_HOUSES).keys()].map((i) => [(i - (NUM_HOUSES / 2)) * SPACING_BETWEEN_HOUSES, COLORS[i]]);
  return h.TriangleSoupSet3({
    triangle_soups: house_x_positions_and_colors.map(
      (([x_translation, color]) =>
        h.TriangleSoupWithColor3({
          triangle_soup:
            h.AffineTransformTriangleSoup3({
              triangle_soup: h.TriangleSoupFromRegion3({ region: house }),
              transform: h.Translate3([x_translation, 0, 0]),
            }),
          color: color
        })))
  });
}