#include "svg/export_to_svg.h"

#include <fstream>
#include <sstream>

#include "cgal/types_core.h"
#include "cgal/types_polygons.h"
#include "svg/svg_types.h"

namespace hypo {
namespace svg {

namespace {

void OutputPoint(std::ostream* out, const cgal::Point_2& point) {
  *out << point.x() << "," << point.y();
}

void OutputPolygon(std::ostream* out, const cgal::Polygon_2& polygon,
                   bool reversed = false) {
  if (polygon.size() < 3) {
    return;
  }

  auto begin = polygon.vertices_begin();
  auto end = polygon.vertices_end();
  std::optional<std::vector<cgal::Point_2>> reversed_vertices;
  if (reversed) {
    reversed_vertices.emplace();
    reversed_vertices->reserve(polygon.size());
    std::reverse_copy(begin, end, std::back_inserter(*reversed_vertices));
    begin = reversed_vertices->begin();
    end = reversed_vertices->end();
  }

  *out << "M ";
  OutputPoint(out, *polygon.vertices_begin());
  *out << " L ";

  for (auto iter = polygon.vertices_begin() + 1; iter != polygon.vertices_end();
       ++iter) {
    const cgal::Point_2& point = *iter;
    OutputPoint(out, point);
    *out << " ";
  }

  *out << "Z";
}

void OutputPolygonWithHoles(
    std::ostream* out, const cgal::Polygon_with_holes_2& polygon_with_holes,
    const std::string& style_tags = std::string()) {
  *out << "  <path d=\"";
  OutputPolygon(out, polygon_with_holes.outer_boundary());

  for (auto iter = polygon_with_holes.holes_begin();
       iter != polygon_with_holes.holes_end(); ++iter) {
    *out << std::endl;
    *out << "           ";
    OutputPolygon(out, *iter, true);
  }

  *out << "\" " << style_tags << " />" << std::endl;
}

std::string sRGBAToCssColorString(const hypo::sRGBA& color) {
  std::ostringstream oss;
  oss << "rgba(" << color[0] * 255 << ", " << color[1] * 255 << ", "
      << color[2] * 255 << ", " << color[3] * 255 << ")";
  return oss.str();
}

std::string SvgWidthToCss(const hypo::SvgWidth& x) {
  std::ostringstream oss;

  if (auto percentage = std::get_if<hypo::SvgPercentage>(&x)) {
    oss << percentage->value << "%";
  } else if (auto absolute = std::get_if<hypo::SvgAbsolute>(&x)) {
    oss << absolute->value;
    switch (absolute->units) {
      case hypo::SvgScalarUnitType::px: {
        oss << "px";
      } break;
      default:
        assert(false);
    }
  }

  return oss.str();
}

}  // namespace

bool ExportToSVG(const Elements& svg_elements,
                 const std::filesystem::path& output_filepath) {
  std::ofstream out(output_filepath);

  cgal::Bbox_2 bounding_box;
  std::ostringstream elements_out;

  auto render_polygon_set =
      [&elements_out, &bounding_box](
          const cgal::Polygon_set_2& polygon_set,
          const std::function<void(std::ostream*,
                                   const cgal::Polygon_with_holes_2&)>&
              render_polygon_with_holes) {
        std::vector<cgal::Polygon_with_holes_2> polygons_with_holes;
        polygons_with_holes.reserve(
            polygon_set.number_of_polygons_with_holes());
        polygon_set.polygons_with_holes(
            std::back_inserter(polygons_with_holes));

        for (const auto& polygon_with_holes : polygons_with_holes) {
          bounding_box += polygon_with_holes.bbox();
          render_polygon_with_holes(&elements_out, polygon_with_holes);
        }
      };

  for (const auto& svg_element : svg_elements) {
    if (auto svg_path_element = std::get_if<PathElement>(&(*svg_element))) {
      if (auto path_element_from_region2 =
              std::get_if<PathElementFromRegion2>(svg_path_element)) {
        render_polygon_set(
            *path_element_from_region2->region,
            [color =
                 std::get<hypo::SvgSolidColor>(path_element_from_region2->fill)
                     .color](
                std::ostream* out,
                const cgal::Polygon_with_holes_2& polygon_with_holes) {
              OutputPolygonWithHoles(
                  out, polygon_with_holes,
                  "fill=\"" + sRGBAToCssColorString(color) + "\"");
            });
      } else if (auto path_element_from_boundary2 =
                     std::get_if<PathElementFromBoundary2>(svg_path_element)) {
        render_polygon_set(
            *path_element_from_boundary2->boundary,
            [color = std::get<hypo::SvgSolidColor>(
                         path_element_from_boundary2->stroke)
                         .color,
             width = path_element_from_boundary2->width](
                std::ostream* out,
                const cgal::Polygon_with_holes_2& polygon_with_holes) {
              OutputPolygonWithHoles(out, polygon_with_holes,
                                     "fill=\"transparent\" stroke=\"" +
                                         sRGBAToCssColorString(color) +
                                         "\" stroke-width=\"" +
                                         SvgWidthToCss(width) + "\"");
            });
      } else {
        assert(false);  // Unknown SVG path element.
      }
    } else {
      assert(false);  // Unknown SVG element.
    }
  }

  out << "<svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\""
      << std::endl;
  out << "             viewBox=\"" << bounding_box.xmin() << " "
      << bounding_box.ymin() << " " << bounding_box.xmax() - bounding_box.xmin()
      << " " << bounding_box.ymax() - bounding_box.ymin() << "\">" << std::endl;

  out << "   <!-- Setup coordinate space such that positive y is up. -->"
      << std::endl;
  out << "  <g transform=\"translate(0," << bounding_box.ymax() << ")\">";
  out << "<g transform=\"scale(1, -1)\">";
  out << "<g transform=\"translate(0," << -bounding_box.ymin() << ")\">"
      << std::endl;
  out << std::endl;

  out << elements_out.str() << std::endl;

  out << std::endl;
  out << "  </g></g></g>" << std::endl;

  out << "</svg>" << std::endl;
  return true;
}

}  // namespace svg
}  // namespace hypo
