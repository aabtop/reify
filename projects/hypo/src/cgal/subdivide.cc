#include "cgal/subdivide.h"

#include <CGAL/boost/graph/convert_nef_polyhedron_to_polygon_mesh.h>
#include <CGAL/subdivision_method_3.h>

#include <cmath>

#include "cgal/types_nef_polyhedron_3.h"
#include "cgal/types_surface_mesh.h"

namespace hypo {
namespace cgal {

Surface_mesh Subdivide(const Surface_mesh& src, hypo::SubdivideMethod method,
                       int iterations) {
  Surface_mesh work_mesh(src);

  switch (method) {
    case hypo::SubdivideMethod::Loop: {
      CGAL::Subdivision_method_3::Loop_subdivision(
          work_mesh, CGAL::parameters::number_of_iterations(iterations));
    } break;
    case hypo::SubdivideMethod::Sqrt3: {
      CGAL::Subdivision_method_3::Sqrt3_subdivision(
          work_mesh, CGAL::parameters::number_of_iterations(iterations));
    } break;
    default: {
      assert(false);
    } break;
  }

  return work_mesh;
}

class SphereProjection_mask_3 {
  using halfedge_descriptor =
      typename boost::graph_traits<Surface_mesh>::halfedge_descriptor;
  using vertex_descriptor =
      typename boost::graph_traits<Surface_mesh>::vertex_descriptor;
  using Vertex_pmap =
      typename boost::property_map<Surface_mesh, CGAL::vertex_point_t>::type;

 public:
  SphereProjection_mask_3(const hypo::Sphere& sphere, Surface_mesh* mesh)
      : sphere_(sphere),
        center_(sphere.center[0], sphere.center[1], sphere.center[2]),
        mesh_(mesh),
        vpm_(CGAL::get(CGAL::vertex_point, *mesh_)) {}

  void edge_node(halfedge_descriptor hd, Point_3& out_point) {
    const Point_3& p1 = boost::get(vpm_, CGAL::target(hd, *mesh_));
    const Point_3& p2 = boost::get(vpm_, CGAL::source(hd, *mesh_));
    Vector_3 from_center = Point_3((p1[0] + p2[0]) * 0.5, (p1[1] + p2[1]) * 0.5,
                                   (p1[2] + p2[2]) * 0.5) -
                           center_;
    auto from_center_normalized =
        from_center / CGAL::sqrt(CGAL::to_double(from_center.squared_length()));
    out_point = center_ + from_center_normalized * sphere_.radius;
  }
  void vertex_node(vertex_descriptor vd, Point_3& out_point) {
    out_point = boost::get(vpm_, vd);
  }
  void border_node(halfedge_descriptor hd, Point_3& out_point, Point_3&) {
    // SubdivideSphere should not be used on non-closed surfaces.
    assert(false);
    edge_node(hd, out_point);
  }

 private:
  hypo::Sphere sphere_;
  Point_3 center_;
  Surface_mesh* mesh_;
  Vertex_pmap vpm_;
};

Surface_mesh SubdivideSphere(const Surface_mesh& src,
                             const hypo::Sphere& sphere, int iterations) {
  Surface_mesh work_mesh(src);

  CGAL::Subdivision_method_3::PTQ(
      work_mesh, SphereProjection_mask_3(sphere, &work_mesh),
      CGAL::parameters::number_of_iterations(iterations));

  return work_mesh;
}

}  // namespace cgal
}  // namespace hypo
