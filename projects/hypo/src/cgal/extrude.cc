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

template <typename Mesh>
std::vector<typename boost::graph_traits<Mesh>::halfedge_descriptor>
GetBorderHalfedgesIndices(const Mesh& mesh) {
  std::vector<typename boost::graph_traits<Mesh>::halfedge_descriptor> result;
  for (auto edge : edges(mesh)) {
    auto he = halfedge(edge, mesh);
    auto he_opp = opposite(he, mesh);
    if (CGAL::is_border(he, mesh)) {
      result.push_back(he);
    }
    if (CGAL::is_border(he_opp, mesh)) {
      result.push_back(he_opp);
    }
  }
  return result;
}

std::vector<surface_mesh_halfedge_descriptor> CreateSliceEdges(
    const Surface_mesh& input,
    const std::vector<surface_mesh_halfedge_descriptor>& input_border_halfedges,
    const Matrix43& transform, Surface_mesh* output) {
  std::vector<surface_mesh_halfedge_descriptor> slice_bottom_halfedges;
  slice_bottom_halfedges.reserve(input_border_halfedges.size());

  std::unordered_map<surface_mesh_vertex_descriptor,
                     surface_mesh_vertex_descriptor>
      vertex_map;
  std::unordered_map<surface_mesh_vertex_descriptor,
                     surface_mesh_halfedge_descriptor>
      associated_halfedges;

  // Iterate around the border half edges, creating the slice vertices
  // and edges as we go.  In the end we will end up with a closed, oriented
  // path of edges, with no faces, representing the border edges of the slice.
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

    slice_bottom_halfedges.push_back(halfedge);
  }

  return slice_bottom_halfedges;
}

void CreateInnerSlice(
    const Surface_mesh& input,
    const std::vector<surface_mesh_halfedge_descriptor>& input_border_halfedges,
    const Matrix43& transform,
    std::vector<surface_mesh_halfedge_descriptor>* below_border_halfedges,
    Surface_mesh* output) {
  std::vector<surface_mesh_halfedge_descriptor> above_border_halfedges =
      CreateSliceEdges(input, input_border_halfedges, transform, output);

  // Now connect the above path edge to the below path edge by creating the
  // faces in between.
  CGAL::Polygon_mesh_processing::extrude_impl::create_strip(
      above_border_halfedges, *below_border_halfedges, *output);

  // Get ready for the next slice by setting up our new bottom slice as the
  // "above" halfedges from this iteration's above slice.
  below_border_halfedges->clear();
  for (auto he : above_border_halfedges) {
    below_border_halfedges->push_back(opposite(he, *output));
  }
}
}  // namespace

void ExtrudeMeshWithTransformList(const Surface_mesh& input,
                                  Surface_mesh& output,
                                  const std::vector<Matrix43>& transforms,
                                  bool closed) {
  CGAL_assertion(!CGAL::is_closed(input));
  assert(transforms.size() > 1);

  std::vector<surface_mesh_halfedge_descriptor> input_border_halfedges =
      GetBorderHalfedgesIndices(input);
  std::vector<surface_mesh_halfedge_descriptor> bottom_border_halfedges;
  std::vector<surface_mesh_halfedge_descriptor> top_border_halfedges;

  // Setup the top and bottom slices, either with capped surfaces or with either
  // side of a border slice if closed.
  if (!closed) {
    // Create the bottom and top caps, transforming the vertices appropriately
    // as we do so.
    auto bottom_mapping = CopyFaceGraph(input, &output);
    TransformVertices(input, &output, transforms.front(),
                      bottom_mapping.vertices);
    // Orient the bottom slice properly so its polygons are facing outwards.
    CGAL::Polygon_mesh_processing::reverse_face_orientations(output);

    // Create the top cap, the last transform.
    auto top_mapping = CopyFaceGraph(input, &output);
    TransformVertices(input, &output, transforms.back(), top_mapping.vertices);

    CGAL_assertion(bottom_mapping.vertices.size() ==
                   top_mapping.vertices.size());

    // Find the border edges so that we can prepare to connect adjacent slices.
    // Eventually we'll need the top slice's border edges as well so we also
    // compute that now while it's convenient.
    size_t num_border_halfedges = input_border_halfedges.size();
    bottom_border_halfedges.reserve(num_border_halfedges);
    top_border_halfedges.reserve(num_border_halfedges);

    // We recalculate |input_border_halfedges| to ensure the order matches
    // with the mappings.
    input_border_halfedges.clear();

    for (size_t i = 0; i < bottom_mapping.halfedges.size(); ++i) {
      assert(bottom_mapping.halfedges[i].first ==
             top_mapping.halfedges[i].first);
      if (CGAL::is_border(bottom_mapping.halfedges[i].first, input)) {
        input_border_halfedges.push_back(bottom_mapping.halfedges[i].first);
        bottom_border_halfedges.push_back(bottom_mapping.halfedges[i].second);
        top_border_halfedges.push_back(top_mapping.halfedges[i].second);
      }
    }

    // The number of border half edges shouldn't change when we recalculate it.
    assert(input_border_halfedges.size() == num_border_halfedges);
  } else {
    // We're setting up a closed loop extrude, so set the "bottom" and "top"
    // halfedge set to be either side of the starting slice.
    top_border_halfedges =
        CreateSliceEdges(input, input_border_halfedges, transforms[0], &output);
    bottom_border_halfedges.reserve(top_border_halfedges.size());
    for (auto he : top_border_halfedges) {
      bottom_border_halfedges.push_back(opposite(he, output));
    }
  }

  // Prepare to iterate through the inner slices.
  std::vector<surface_mesh_halfedge_descriptor> below_border_halfedges =
      std::move(bottom_border_halfedges);

  // Go through each transform in the list, connecting the next one (which we
  // refer to as the "top" slice) with the previous one (the "bottom" slice).
  // We use "top" and "bottom" to distinguish between "next" and "previous",
  // which we use while iterating through the border vertices iteration.
  size_t last_transform_index = transforms.size() - (closed ? 0 : 1);
  for (size_t transform_index = 1; transform_index < last_transform_index;
       ++transform_index) {
    // Each iteration, this function will update |below_border_halfedges| to
    // be the new border edge after incorporating this next inner slice.
    CreateInnerSlice(input, input_border_halfedges, transforms[transform_index],
                     &below_border_halfedges, &output);
  }

  // Finally create the final triangle strip connecting the top slice to the
  // previous slice.
  CGAL::Polygon_mesh_processing::extrude_impl::create_strip(
      top_border_halfedges, below_border_halfedges, output);

  CGAL_assertion(output.is_valid());
  CGAL_assertion(CGAL::is_closed(output));
}  // namespace cgal

}  // namespace cgal
}  // namespace hypo
