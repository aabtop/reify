#ifndef _IDE_VULKAN_TRIANGLE_SOUP_H
#define _IDE_VULKAN_TRIANGLE_SOUP_H

#include <array>
#include <vector>

// In indexed triangle list of 3D vertices.
struct TriangleSoup {
  using Vector3 = std::array<float, 3>;
  struct Vertex {
    Vector3 position;
    Vector3 normal;
  };
  using Index = uint32_t;
  using Triangle = std::array<Index, 3>;

  std::vector<Vertex> vertices;
  std::vector<Triangle> triangles;
};

#endif  // _IDE_VULKAN_TRIANGLE_SOUP_H
