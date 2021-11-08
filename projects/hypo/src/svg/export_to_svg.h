#ifndef _HYPO_SVG_EXPORT_TO_SVG_H_
#define _HYPO_SVG_EXPORT_TO_SVG_H_

#include <filesystem>

#include "svg/svg_types.h"

namespace hypo {
namespace svg {

bool ExportToSVG(const Elements& svg_elements,
                 const std::filesystem::path& output_filepath);

}  // namespace svg
}  // namespace hypo

#endif  // _HYPO_SVG_EXPORT_TO_SVG_H_
