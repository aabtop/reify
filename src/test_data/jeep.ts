namespace reify {
  export type Vec2 = [number, number];
  
  export type Vec3 = [number, number, number];
  
  export type Matrix44 = [number, number, number, number, number, number, number, number, number, number, number, number, number, number, number, number];
  
  export type Circle = { radius: number; center: Vec2;  };
  
  export type Rectangle = { width: number; height: number; top_left: Vec2;  };
  
  export function CircleAsMesh(p0: Circle): Mesh2{
    return { __kind: "CircleAsMesh", p0: p0};
  }
  export function RectangleAsMesh(p0: Rectangle): Mesh2{
    return { __kind: "RectangleAsMesh", p0: p0};
  }
  export type Mesh2 = {__kind: "CircleAsMesh"; p0: Circle; } | {__kind: "RectangleAsMesh"; p0: Rectangle; };
  
  export type ExtrudeMesh2 = { source: Mesh2; path: [Vec3, Vec3];  };
  
  
  export type TransformMesh3 = { source: Mesh3; transform: Matrix44;  };
  
  export function ExtrudeMesh2AsMesh(p0: ExtrudeMesh2): Mesh3{
    return { __kind: "ExtrudeMesh2AsMesh", p0: p0};
  }
  export function TransformMesh3AsMesh(p0: TransformMesh3): Mesh3{
    return { __kind: "TransformMesh3AsMesh", p0: p0};
  }
  export function MeshUnion(p0: Mesh3[]): Mesh3{
    return { __kind: "MeshUnion", p0: p0};
  }
  export type Mesh3 = {__kind: "ExtrudeMesh2AsMesh"; p0: ExtrudeMesh2; } | {__kind: "TransformMesh3AsMesh"; p0: TransformMesh3; } | {__kind: "MeshUnion"; p0: Mesh3[]; };
  
}  // namespace reify
  
function Translate3D(translation: reify.Vec3): reify.Matrix44 {
  return [
    1.0, 0.0, 0.0, translation[0],
    0.0, 1.0, 0.0, translation[1],
    0.0, 0.0, 1.0, translation[2],
    0.0, 0.0, 0.0, 1.0];
}

function TranslatedMesh3(mesh: reify.Mesh3, translation: reify.Vec3): reify.Mesh3 {
  return reify.TransformMesh3AsMesh({ source: mesh, transform: Translate3D(translation) });
}

function Cylinder(radius: number, thickness: number): reify.Mesh3 {
  return reify.ExtrudeMesh2AsMesh({
    source: reify.CircleAsMesh({ radius: radius, center: [0, 0] }),
    path: [[0, 0, -thickness * 0.5], [0, 0, thickness * 0.5]]
  });
}


function Jeep() {
  let jeep_wheel = Cylinder(0.35, 0.15);

  return reify.MeshUnion([
    TranslatedMesh3(jeep_wheel, [1.0, 0.0, -0.5]),
    TranslatedMesh3(jeep_wheel, [1.0, 0.0, -0.5]),
    TranslatedMesh3(jeep_wheel, [1.0, 0.0, 0.5]),
    TranslatedMesh3(jeep_wheel, [-1.0, 0.0, 0.5]),
    TranslatedMesh3(jeep_wheel, [-1.0, 0.0, -0.5]),
  ]);
}
