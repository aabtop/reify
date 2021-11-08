#ifndef _HYPO_VISUALIZER_CONSTRUCT_SVG_VISUALIZERS_H_
#define _HYPO_VISUALIZER_CONSTRUCT_SVG_VISUALIZERS_H_

#include <memory>

#include "reify/pure_cpp/thread_pool_cache_runner.h"
#include "reify/purecpp/hypo.h"
#include "visualizer/visualizer_svg_types.h"

namespace hypo {
namespace visualizer {

using FutureVisualizerSvgElements =
    reify::pure_cpp::ThreadPoolCacheRunner::Future<
        std::shared_ptr<const VisualizerSvgElements>>;

FutureVisualizerSvgElements ConstructVisualizerSvgElements(
    reify::pure_cpp::ThreadPoolCacheRunner* runner, const hypo::SvgElements& x);

}  // namespace visualizer
}  // namespace hypo

#endif  // _HYPO_VISUALIZER_CONSTRUCT_SVG_VISUALIZERS_H_
