#ifndef _HYPO_EXPORT_TO_FILE_H_
#define _HYPO_EXPORT_TO_FILE_H_

#include <chrono>
#include <optional>
#include <string>

#include "cgal/construct_region2.h"
#include "cgal/construct_region3.h"
#include "export_to_stl.h"
#include "export_to_svg.h"
#include "reify/purecpp/hypo.h"

namespace hypo {
namespace cgal {

struct BuildAndExportResults {
  std::chrono::microseconds build_time;
  std::chrono::microseconds export_time;
  std::string output_filepath;
};

template <typename T>
auto BuildRegion(const T& region) {
  if constexpr (std::is_same<T, hypo::Region2>::value) {
    return hypo::cgal::ConstructRegion2(region);
  } else if constexpr (std::is_same<T, hypo::Region3>::value) {
    return hypo::cgal::ConstructRegion3(region);
  } else {
    assert(false);
  }
}

template <typename T>
std::optional<BuildAndExportResults> BuildAndExportToFile(
    const T& region, const std::string& output_base_filepath) {
  BuildAndExportResults results;

  std::chrono::high_resolution_clock::time_point start_build_time =
      std::chrono::high_resolution_clock::now();
  auto built_region = BuildRegion(region);
  results.build_time = std::chrono::duration_cast<std::chrono::microseconds>(
      std::chrono::high_resolution_clock::now() - start_build_time);

  std::chrono::high_resolution_clock::time_point start_export_time =
      std::chrono::high_resolution_clock::now();

  bool export_success = false;
  if constexpr (std::is_same<T, hypo::Region2>::value) {
    results.output_filepath = output_base_filepath + ".svg";
    export_success = hypo::cgal::ExportToSVG(std::move(built_region),
                                             results.output_filepath);
  } else if constexpr (std::is_same<T, hypo::Region3>::value) {
    results.output_filepath = output_base_filepath + ".stl";
    export_success = hypo::cgal::ExportToSTL(std::move(built_region),
                                             results.output_filepath);
  } else {
    assert(false);
  }

  results.export_time = std::chrono::duration_cast<std::chrono::microseconds>(
      std::chrono::high_resolution_clock::now() - start_export_time);

  if (export_success) {
    return results;
  } else {
    return std::nullopt;
  }
}

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_EXPORT_TO_FILE_H_