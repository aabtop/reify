#ifndef _HYPO_CGAL_TYPES_TRIANGLE_SOUP_H_
#define _HYPO_CGAL_TYPES_TRIANGLE_SOUP_H_

#include <array>
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <memory>
#include <unordered_set>

#include "reify/purecpp/hypo.h"

namespace hypo {
namespace cgal {

struct TriangleSoup {
  using Vector3 = Vec3;
  struct Vertex {
    Vector3 position;
    Vector3 normal;
  };
  std::shared_ptr<const std::vector<Vertex>> vertices;
  using Triangle = std::array<uint32_t, 3>;
  std::shared_ptr<const std::vector<Triangle>> triangles;

  sRGB color;
  glm::mat4 transform;
};

using TriangleSoupSet = std::unordered_set<std::shared_ptr<const TriangleSoup>>;

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_CGAL_TYPES_TRIANGLE_SOUP_H_
