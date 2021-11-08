#ifndef _HYPO_SVG_SVG_TYPES_H_
#define _HYPO_SVG_SVG_TYPES_H_

#include <memory>
#include <vector>

#include "cgal/types_polygons.h"
#include "reify/purecpp/hypo.h"

namespace hypo {
namespace svg {

// This largely mirrors the Reify IDT type definitions, but with the
// CGAL base references constructed.

struct PathElementFromRegion2 {
  std::shared_ptr<const cgal::Polygon_set_2> region;

  hypo::SvgFillStyle fill;
};

struct PathElementFromBoundary2 {
  std::shared_ptr<const cgal::Polygon_set_2> boundary;

  hypo::SvgStrokeStyle stroke;
  hypo::SvgWidth width;
};

using PathElement =
    std::variant<PathElementFromRegion2, PathElementFromBoundary2>;

using Element = std::variant<PathElement>;

using Elements = std::vector<std::shared_ptr<const Element>>;

}  // namespace svg
}  // namespace hypo

#endif  // _HYPO_SVG_SVG_TYPES_H_
