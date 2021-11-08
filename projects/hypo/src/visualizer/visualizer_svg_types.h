#ifndef _HYPO_VISUALIZER_SVG_VISUALIZER_TYPES_H_
#define _HYPO_VISUALIZER_SVG_VISUALIZER_TYPES_H_

#include <memory>
#include <vector>

#include "reify/purecpp/hypo.h"
#include "src/visualizer/vulkan/simple_simplex_renderer2.h"

namespace hypo {
namespace visualizer {

// This largely mirrors the Reify IDT type definitions, but with the
// CGAL base references constructed.

using TriangleSoup2 = SimpleSimplexRenderer2::SimplexSoup<2, 2>;
using LineSegmentSoup2 = SimpleSimplexRenderer2::SimplexSoup<2, 1>;

struct VisualizerSvgPathElementFromRegion2 {
  std::shared_ptr<const TriangleSoup2> region_triangles;

  hypo::SvgFillStyle fill;
};

struct VisualizerSvgPathElementFromBoundary2 {
  std::shared_ptr<const LineSegmentSoup2> boundary_line_segments;

  hypo::SvgStrokeStyle stroke;
  hypo::SvgWidth width;
};

using VisualizerSvgPathElement =
    std::variant<VisualizerSvgPathElementFromRegion2,
                 VisualizerSvgPathElementFromBoundary2>;

using VisualizerSvgElement = std::variant<VisualizerSvgPathElement>;

using VisualizerSvgElements =
    std::vector<std::shared_ptr<const VisualizerSvgElement>>;

}  // namespace visualizer
}  // namespace hypo

#endif  // _HYPO_VISUALIZER_SVG_VISUALIZER_TYPES_H_
