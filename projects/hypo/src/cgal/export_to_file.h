#ifndef _HYPO_EXPORT_TO_FILE_H_
#define _HYPO_EXPORT_TO_FILE_H_

#include <fmt/format.h>

#include <chrono>
#include <filesystem>
#include <optional>
#include <string>

#include "cgal/construct_region2.h"
#include "cgal/construct_region3.h"
#include "cgal/errors.h"
#include "export_to_stl.h"
#include "export_to_svg.h"
#include "reify/purecpp/hypo.h"
#include "reify/utils/error.h"

namespace hypo {
namespace cgal {

struct BuildAndExportResults {
  std::chrono::microseconds build_time;
  std::chrono::microseconds export_time;
  std::filesystem::path output_filepath;
};

template <typename T>
auto BuildObject(const T& object) {
  if constexpr (std::is_same<T, hypo::Region2>::value) {
    return hypo::cgal::CallCgalAndCatchExceptions(&hypo::cgal::ConstructRegion2,
                                                  object);
  } else if constexpr (std::is_same<T, hypo::Boundary2>::value) {
    return hypo::cgal::CallCgalAndCatchExceptions(
        &hypo::cgal::ConstructBoundary2, object);
  } else if constexpr (std::is_same<T, hypo::Region3>::value) {
    return hypo::cgal::CallCgalAndCatchExceptions(&hypo::cgal::ConstructRegion3,
                                                  object);
  } else {
    assert(false);
  }
}

template <typename T>
reify::utils::ErrorOr<BuildAndExportResults> BuildAndExportToFile(
    const T& object, const std::string& output_base_filepath) {
  BuildAndExportResults results;

  std::chrono::high_resolution_clock::time_point start_build_time =
      std::chrono::high_resolution_clock::now();
  REIFY_UTILS_ASSIGN_OR_RETURN(built_region, BuildObject(object));
  results.build_time = std::chrono::duration_cast<std::chrono::microseconds>(
      std::chrono::high_resolution_clock::now() - start_build_time);

  std::chrono::high_resolution_clock::time_point start_export_time =
      std::chrono::high_resolution_clock::now();

  bool export_success = false;
  if constexpr (std::is_same<T, hypo::Region2>::value) {
    results.output_filepath = output_base_filepath + ".svg";
    export_success = hypo::cgal::ExportRegionToSVG(std::move(built_region),
                                                   results.output_filepath);
  } else if constexpr (std::is_same<T, hypo::Boundary2>::value) {
    results.output_filepath = output_base_filepath + ".svg";
    export_success = hypo::cgal::ExportBoundaryToSVG(std::move(built_region),
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
    return reify::utils::Error{fmt::format("Error writing to file {}.",
                                           results.output_filepath.string())};
  }
}

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_EXPORT_TO_FILE_H_