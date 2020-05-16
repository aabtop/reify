#ifndef _HYPO_EXPORT_TO_FILE_H_
#define _HYPO_EXPORT_TO_FILE_H_

#include <string>

#include "hypo.h"

namespace hypo {
namespace cgal {

bool ExportToFile(const hypo::Region2& region2,
                  const std::string& output_filepath);

bool ExportToFile(const hypo::Region3& region3,
                  const std::string& output_filepath);

}  // namespace cgal
}  // namespace hypo

#endif  // _HYPO_EXPORT_TO_FILE_H_