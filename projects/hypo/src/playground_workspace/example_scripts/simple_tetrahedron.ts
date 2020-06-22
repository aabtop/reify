import * as h from 'hypo';

export function Main() {
  return h.TriangleList3({
    vertices: [[0, 0, 0], [1, 0, 0], [0, 1, 0], [0, 0, 1]],
    triangles: [[0, 1, 2], [0, 3, 1], [0, 2, 3], [2, 1, 3]]
  });
}
