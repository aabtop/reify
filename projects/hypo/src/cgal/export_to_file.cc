#include "export_to_file.h"

#include "cgal/construct_region2.h"
#include "cgal/construct_region3.h"
#include "export_to_stl.h"
#include "export_to_svg.h"

namespace hypo {
namespace cgal {

bool ExportToFile(const hypo::Region2& region2,
                  const std::string& output_filepath) {
  auto cgal_region2 = hypo::cgal::ConstructRegion2(region2);
  if (!cgal_region2) {
    std::cerr << "Error when converting from hypo description to CGAL object."
              << std::endl;
    return false;
  }

  return hypo::cgal::ExportToSVG(*cgal_region2, output_filepath + ".svg");
}

bool ExportToFile(const hypo::Region3& region3,
                  const std::string& output_filepath) {
  auto cgal_region3 = hypo::cgal::ConstructRegion3(region3);
  if (!cgal_region3) {
    std::cerr << "Error when converting from hypo description to CGAL object."
              << std::endl;
    return false;
  }

  return hypo::cgal::ExportToSTL(*cgal_region3, output_filepath + ".stl");
}

}  // namespace cgal
}  // namespace hypo
