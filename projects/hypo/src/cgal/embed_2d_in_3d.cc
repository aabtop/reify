#include "cgal/embed_2d_in_3d.h"

#include <CGAL/Constrained_Delaunay_triangulation_2.h>
#include <CGAL/Triangulation_face_base_with_info_2.h>

#include "cgal/extrude.h"
#include "cgal/types_nef_polyhedron_3.h"
#include "cgal/types_polygons.h"
#include "cgal/types_surface_mesh.h"

namespace hypo {
namespace cgal {

namespace {
Nef_polyhedron_3 EmbedPolygonIn3DXYPlane(const Polygon_2& polygon) {
  std::vector<Point_3> point_3s;
  point_3s.reserve(polygon.size());
  for (auto iter = polygon.vertices_begin(); iter != polygon.vertices_end();
       ++iter) {
    point_3s.emplace_back(iter->x(), iter->y(), 0);
  }
  return Nef_polyhedron_3(point_3s.begin(), point_3s.end());
}

Nef_polyhedron_3 EmbedPolygonWithHolesIn3DXYPlane(
    const Polygon_with_holes_2& polygon_with_holes) {
  Nef_polyhedron_3 output(
      EmbedPolygonIn3DXYPlane(polygon_with_holes.outer_boundary()));
  for (auto iter = polygon_with_holes.holes_begin();
       iter != polygon_with_holes.holes_end(); ++iter) {
    output ^= EmbedPolygonIn3DXYPlane(*iter);
  }
  return output;
}
}  // namespace

namespace {
struct FaceInfo2 {
  int nesting_level;

  bool in_domain() const { return nesting_level % 2 == 1; }
};

using Triangulation_vertex_base_2 = CGAL::Triangulation_vertex_base_2<Kernel>;
using Triangulation_face_base_with_info_2 =
    CGAL::Triangulation_face_base_with_info_2<FaceInfo2, Kernel>;
using Constrained_triangulation_face_base_2 =
    CGAL::Constrained_triangulation_face_base_2<
        Kernel, Triangulation_face_base_with_info_2>;
using Triangulation_data_structure_2 =
    CGAL::Triangulation_data_structure_2<Triangulation_vertex_base_2,
                                         Constrained_triangulation_face_base_2>;
using Constrained_Delaunay_triangulation_2 =
    CGAL::Constrained_Delaunay_triangulation_2<
        Kernel, Triangulation_data_structure_2>;

void AddPolygonWithHolesToTriangulation(
    const Polygon_with_holes_2& polygon_with_holes,
    Constrained_Delaunay_triangulation_2* cdt) {
  cdt->insert_constraint(polygon_with_holes.outer_boundary().vertices_begin(),
                         polygon_with_holes.outer_boundary().vertices_end(),
                         true);
  for (auto iter = polygon_with_holes.holes_begin();
       iter != polygon_with_holes.holes_end(); ++iter) {
    const Polygon_2& hole = *iter;
    cdt->insert_constraint(hole.vertices_begin(), hole.vertices_end(), true);
  }
}

// From the CGAL manual example:
// https://doc.cgal.org/latest/Triangulation_2/Triangulation_2_2polygon_triangulation_8cpp-example.html
// Essentially, we start with a triangle known to be on the outside of the
// region identified by the polygon set, and from there flood fill its
// neighboors with the same nesting level value (e.g. 0, indicating that we're
// outside the region).  Whenever we encounter a constraint edge, we do not
// visit through it.  Instead, once we are done marking everything, we return to
// look at the list of constraint edges we encounter and then pass through them
// while incrementing the |nesting_level| counter, and then repeat the flood
// filling process.  This will result in all triangles being marked with a
// nesting level.

void FloodFillNestingLevel(
    Constrained_Delaunay_triangulation_2* cdt,
    Constrained_Delaunay_triangulation_2::Face_handle initial_face,
    int fill_value,
    std::vector<Constrained_Delaunay_triangulation_2::Edge>* borders) {
  if (initial_face->info().nesting_level != -1) {
    return;
  }

  std::vector<Constrained_Delaunay_triangulation_2::Face_handle> queue;
  queue.push_back(initial_face);
  while (!queue.empty()) {
    Constrained_Delaunay_triangulation_2::Face_handle current_face =
        queue.back();
    queue.pop_back();
    if (current_face->info().nesting_level == -1) {
      current_face->info().nesting_level = fill_value;
      for (int i = 0; i < 3; i++) {
        Constrained_Delaunay_triangulation_2::Edge e(current_face, i);
        Constrained_Delaunay_triangulation_2::Face_handle n =
            current_face->neighbor(i);
        if (n->info().nesting_level == -1) {
          if (cdt->is_constrained(e))
            borders->push_back(e);
          else
            queue.push_back(n);
        }
      }
    }
  }
}

void ComputeTriangleNestingLevels(Constrained_Delaunay_triangulation_2* cdt) {
  for (Constrained_Delaunay_triangulation_2::Face_handle f :
       cdt->all_face_handles()) {
    f->info().nesting_level = -1;
  }
  std::vector<Constrained_Delaunay_triangulation_2::Edge> borders;
  FloodFillNestingLevel(cdt, cdt->infinite_face(), 0, &borders);
  while (!borders.empty()) {
    Constrained_Delaunay_triangulation_2::Edge e = borders.back();
    borders.pop_back();
    Constrained_Delaunay_triangulation_2::Face_handle n =
        e.first->neighbor(e.second);
    if (n->info().nesting_level == -1) {
      FloodFillNestingLevel(cdt, n, e.first->info().nesting_level + 1,
                            &borders);
    }
  }
}

// Returns a triangulation of the polygon set with each triangle marked with
// a nesting level.  Triangles with an odd nesting level are considered in the
// set.
Constrained_Delaunay_triangulation_2 TriangulatePolygonSet(
    const Polygon_set_2& polygon_set) {
  Constrained_Delaunay_triangulation_2 cdt;
  std::vector<Polygon_with_holes_2> polygons_with_holes;
  polygons_with_holes.reserve(polygon_set.number_of_polygons_with_holes());
  polygon_set.polygons_with_holes(std::back_inserter(polygons_with_holes));
  for (const auto& polygon_with_holes : polygons_with_holes) {
    AddPolygonWithHolesToTriangulation(polygon_with_holes, &cdt);
  }

  ComputeTriangleNestingLevels(&cdt);
  return cdt;
}

using Surface_mesh_vertex_descriptor =
    typename boost::graph_traits<Surface_mesh>::vertex_descriptor;

Surface_mesh ConvertTriangulationToSurfaceMesh(
    const Constrained_Delaunay_triangulation_2& cdt) {
  Surface_mesh mesh;

  using VertexMap = std::unordered_map<
      typename Constrained_Delaunay_triangulation_2::Vertex_handle,
      Surface_mesh_vertex_descriptor>;
  VertexMap descriptors;
  for (auto iter = cdt.finite_faces_begin(); iter != cdt.finite_faces_end();
       ++iter) {
    Constrained_Delaunay_triangulation_2::Face_handle face = iter;

    if (!face->info().in_domain()) continue;

    std::array<Surface_mesh_vertex_descriptor, 3> face_vertices;
    for (int i = 0; i < 3; ++i) {
      auto [found, insert_ok] = descriptors.insert(
          std::pair(face->vertex(i), Surface_mesh_vertex_descriptor()));
      if (insert_ok) {
        const Point_2& pt = face->vertex(i)->point();
        found->second = add_vertex(Point_3(pt[0], pt[1], 0), mesh);
      }
      face_vertices[i] = found->second;
    }

    CGAL::Euler::add_face(face_vertices, mesh);
  }

  return mesh;
}

}  // namespace

Nef_polyhedron_3 EmbedPolygonSetAs3DSurfaceMesh(
    const Polygon_set_2& polygon_set,
    const std::vector<hypo::Matrix43>& transforms, bool closed) {
  Constrained_Delaunay_triangulation_2 cdt = TriangulatePolygonSet(polygon_set);

  Surface_mesh input_as_mesh = ConvertTriangulationToSurfaceMesh(cdt);

  Surface_mesh extruded_mesh;

  ExtrudeMeshWithTransformList(input_as_mesh, extruded_mesh, transforms,
                               closed);

  return Nef_polyhedron_3(extruded_mesh);
}

}  // namespace cgal
}  // namespace hypo
