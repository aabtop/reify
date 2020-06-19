#include "cgal/extrude.h"

#include <CGAL/Polygon_mesh_processing/extrude.h>
#include <CGAL/boost/graph/Euler_operations.h>
#include <CGAL/boost/graph/named_params_helper.h>

namespace hypo {
namespace cgal {

namespace {
Point_3 MultMatrix(const Matrix43& mat, const Point_3& p) {
  return Point_3(mat[0] * p.x() + mat[1] * p.y() + mat[2],
                 mat[3] * p.x() + mat[4] * p.y() + mat[5],
                 mat[6] * p.x() + mat[7] * p.y() + mat[8]);
}

using DefaultNamedParameters = decltype(CGAL::parameters::all_default());

using surface_mesh_vertex_descriptor =
    boost::graph_traits<Surface_mesh>::vertex_descriptor;
using surface_mesh_halfedge_descriptor =
    boost::graph_traits<Surface_mesh>::halfedge_descriptor;

template <typename InputMesh, typename OutputMesh>
using VertexMapList = std::vector<
    std::pair<typename boost::graph_traits<InputMesh>::vertex_descriptor,
              typename boost::graph_traits<OutputMesh>::vertex_descriptor>>;
template <typename InputMesh, typename OutputMesh>
using HalfedgeMapList = std::vector<
    std::pair<typename boost::graph_traits<InputMesh>::halfedge_descriptor,
              typename boost::graph_traits<OutputMesh>::halfedge_descriptor>>;

template <typename InputMesh, typename OutputMesh>
struct CopiedGraphMappings {
  VertexMapList<InputMesh, OutputMesh> vertices;
  HalfedgeMapList<InputMesh, OutputMesh> halfedges;
};

template <typename InputMesh, typename OutputMesh>
CopiedGraphMappings<InputMesh, OutputMesh> CopyFaceGraph(const InputMesh& input,
                                                         OutputMesh* output) {
  CopiedGraphMappings<InputMesh, OutputMesh> result;

  using CGAL::parameters::choose_parameter;
  using CGAL::parameters::get_parameter;

  auto output_vpm =
      choose_parameter(get_parameter(CGAL::parameters::all_default(),
                                     CGAL::internal_np::vertex_point),
                       CGAL::get_property_map(CGAL::vertex_point, *output));
  auto input_vpm =
      choose_parameter(get_parameter(CGAL::parameters::all_default(),
                                     CGAL::internal_np::vertex_point),
                       CGAL::get_const_property_map(CGAL::vertex_point, input));

  CGAL::copy_face_graph(input, *output, std::back_inserter(result.vertices),
                        std::back_inserter(result.halfedges),
                        CGAL::Emptyset_iterator(), input_vpm, output_vpm);
  return result;
}

void TransformVertices(
    const Surface_mesh& input, Surface_mesh* output, const Matrix43& transform,
    const VertexMapList<Surface_mesh, Surface_mesh>& vertex_mapping) {
  for (std::size_t i = 0; i < vertex_mapping.size(); ++i) {
    auto input_point = input.point(vertex_mapping[i].first);
    output->point(vertex_mapping[i].second) =
        MultMatrix(transform, input_point);
  }
}

template <typename Mesh, typename HalfedgeMapList>
std::vector<std::size_t> GetBorderHalfedgesIndices(
    const Mesh& mesh, const HalfedgeMapList& halfedge_mapping) {
  std::vector<std::size_t> result;
  for (size_t i = 0; i < halfedge_mapping.size(); ++i) {
    if (CGAL::is_border(halfedge_mapping[i].first, mesh)) {
      result.push_back(i);
    }
  }
  return result;
}

void CreateInnerLayer(
    const Surface_mesh& input,
    const std::vector<surface_mesh_halfedge_descriptor>& input_border_halfedges,
    const Matrix43& transform,
    std::vector<surface_mesh_halfedge_descriptor>* below_border_halfedges,
    Surface_mesh* output) {
  std::vector<surface_mesh_halfedge_descriptor> above_border_halfedges;
  above_border_halfedges.reserve(below_border_halfedges->size());

  std::unordered_map<surface_mesh_vertex_descriptor,
                     surface_mesh_vertex_descriptor>
      vertex_map;
  std::unordered_map<surface_mesh_vertex_descriptor,
                     surface_mesh_halfedge_descriptor>
      associated_halfedges;

  // Iterate around the border half edges, creating the above layer vertices
  // and edges as we go.  In the end we will end up with a closed, oriented
  // path of edges, with no faces, representing the edge of the above layer.
  // We will connect the below and above layers with faces after we finish
  // setting up the path for the above layer.
  for (size_t i = 0; i < input_border_halfedges.size(); ++i) {
    surface_mesh_halfedge_descriptor ih = input_border_halfedges[i];
    surface_mesh_vertex_descriptor input_target_vertex = target(ih, input);
    surface_mesh_vertex_descriptor input_source_vertex = source(ih, input);
    surface_mesh_vertex_descriptor output_target_vertex;
    surface_mesh_vertex_descriptor output_source_vertex;

    // For each halfedge, lookup their source and target vertices, or create
    // them by transforming the input vertex if they're not already created.
    auto target_vertex = vertex_map.find(input_target_vertex);
    if (target_vertex != vertex_map.end()) {
      output_target_vertex = target_vertex->second;
    } else {
      output_target_vertex = output->add_vertex(
          MultMatrix(transform, input.point(input_target_vertex)));
      vertex_map.insert(std::pair(input_target_vertex, output_target_vertex));
    }
    auto source_vertex = vertex_map.find(input_source_vertex);
    if (source_vertex != vertex_map.end()) {
      output_source_vertex = source_vertex->second;
    } else {
      output_source_vertex = output->add_vertex(
          MultMatrix(transform, input.point(input_source_vertex)));
      vertex_map.insert(std::pair(input_source_vertex, output_source_vertex));
    }

    // At this point we are now guaranteed to have both |previous_vertex|
    // and |next_vertex| initialized, so we can now create our edges and
    // faces between them.
    auto halfedge =
        output->add_edge(output_source_vertex, output_target_vertex);
    output->set_halfedge(output_target_vertex, halfedge);

    if (target_vertex != vertex_map.end()) {
      auto next_halfedge = associated_halfedges[output_target_vertex];
      output->set_next(halfedge, next_halfedge);
      output->set_next(opposite(next_halfedge, *output),
                       opposite(halfedge, *output));
    } else {
      associated_halfedges[output_target_vertex] = halfedge;
    }
    if (source_vertex != vertex_map.end()) {
      auto prev_halfedge = associated_halfedges[output_source_vertex];
      output->set_next(prev_halfedge, halfedge);
      output->set_next(opposite(halfedge, *output),
                       opposite(prev_halfedge, *output));
    } else {
      associated_halfedges[output_source_vertex] = halfedge;
    }

    above_border_halfedges.push_back(halfedge);
  }

  // Now connect the above path edge to the below path edge by creating the
  // faces in between.
  CGAL::Polygon_mesh_processing::extrude_impl::create_strip(
      above_border_halfedges, *below_border_halfedges, *output);

  // Get ready for the next layer by setting up our new bottom layer as the
  // "above" halfedges from this iteration's above layer.
  below_border_halfedges->clear();
  for (auto he : above_border_halfedges) {
    below_border_halfedges->push_back(opposite(he, *output));
  }
}
}  // namespace

void extrude_mesh_multiple(const Surface_mesh& input, Surface_mesh& output,
                           const std::vector<Matrix43>& transforms) {
  CGAL_assertion(!CGAL::is_closed(input));
  assert(transforms.size() > 1);

  // Create the bottom and top caps, transforming the vertices appropriately as
  // we do so.
  auto bottom_mapping = CopyFaceGraph(input, &output);
  TransformVertices(input, &output, transforms.front(),
                    bottom_mapping.vertices);
  // Orient the bottom layer properly so its polygons are facing outwards.
  CGAL::Polygon_mesh_processing::reverse_face_orientations(output);

  // Create the top cap, the last transform.
  auto top_mapping = CopyFaceGraph(input, &output);
  TransformVertices(input, &output, transforms.back(), top_mapping.vertices);

  CGAL_assertion(bottom_mapping.vertices.size() == top_mapping.vertices.size());

  // Find the border edges so that we can prepare to connect them to the next
  // layer.  Eventually we'll need the top layer's border edges as well so we
  // also compute that now as well.
  auto border_vertex_indices =
      GetBorderHalfedgesIndices(input, bottom_mapping.halfedges);

  std::vector<surface_mesh_halfedge_descriptor> input_border_halfedges;
  input_border_halfedges.reserve(border_vertex_indices.size());
  std::vector<surface_mesh_halfedge_descriptor> bottom_border_halfedges;
  bottom_border_halfedges.reserve(input_border_halfedges.size());
  std::vector<surface_mesh_halfedge_descriptor> top_border_halfedges;
  top_border_halfedges.reserve(input_border_halfedges.size());

  for (auto i : border_vertex_indices) {
    input_border_halfedges.push_back(bottom_mapping.halfedges[i].first);
    bottom_border_halfedges.push_back(bottom_mapping.halfedges[i].second);
    top_border_halfedges.push_back(top_mapping.halfedges[i].second);
  }

  // Prepare to iterate through the inner layers.
  std::vector<surface_mesh_halfedge_descriptor> below_border_halfedges =
      std::move(bottom_border_halfedges);

  // Go through each transform in the list, connecting the next one (which we
  // refer to as the "top" layer) with the previous one (the "bottom" layer).
  // We use "top" and "bottom" to distinguish between "next" and "previous",
  // which we use while iterating through the border vertices iteration.
  for (size_t transform_index = 1; transform_index < transforms.size() - 1;
       ++transform_index) {
    // Each iteration, this function will update |below_border_halfedges| to
    // be the new border edge after incorporating this next inner layer.
    CreateInnerLayer(input, input_border_halfedges, transforms[transform_index],
                     &below_border_halfedges, &output);
  }

  // Finally create the final triangle strip connecting the top layer to the
  // previous layer.
  CGAL::Polygon_mesh_processing::extrude_impl::create_strip(
      top_border_halfedges, below_border_halfedges, output);

  CGAL_assertion(output.is_valid());
  CGAL_assertion(CGAL::is_closed(output));
}

}  // namespace cgal
}  // namespace hypo
