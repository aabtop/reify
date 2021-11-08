#ifndef _HYPO_SVG_CONSTRUCT_SVG_H_
#define _HYPO_SVG_CONSTRUCT_SVG_H_

#include "reify/pure_cpp/thread_pool_cache_runner.h"
#include "reify/purecpp/hypo.h"
#include "svg/svg_types.h"

namespace hypo {
namespace svg {

using FutureElements = reify::pure_cpp::ThreadPoolCacheRunner::Future<
    std::shared_ptr<const Elements>>;

FutureElements ConstructSvgElements(
    reify::pure_cpp::ThreadPoolCacheRunner* runner, const hypo::SvgElements& x);

}  // namespace svg
}  // namespace hypo

#endif  // _HYPO_SVG_CONSTRUCT_SVG_H_
