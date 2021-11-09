#include "svg/construct_svg.h"

#include "cgal/construct_region2.h"

namespace hypo {
namespace svg {

namespace {
using FutureElement = reify::pure_cpp::ThreadPoolCacheRunner::Future<
    std::shared_ptr<const Element>>;

PathElement ConstructSvgPathElement(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::SvgPathElementFromRegion2& x) {
  return PathElementFromRegion2{cgal::ConstructRegion2(runner, x.region).Get(),
                                x.fill};
}

PathElement ConstructSvgPathElement(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::SvgPathElementFromBoundary2& x) {
  return PathElementFromBoundary2{
      cgal::ConstructBoundary2(runner, x.boundary).Get(), x.stroke, x.width};
}

Element ConstructSvgElement(reify::pure_cpp::ThreadPoolCacheRunner* runner,
                            const hypo::SvgPathElement& x) {
  return std::visit(
      [runner](const auto& y) { return ConstructSvgPathElement(runner, y); },
      static_cast<const hypo::SvgPathElement::AsVariant&>(x));
}

FutureElement ConstructSvgElementFuture(
    reify::pure_cpp::ThreadPoolCacheRunner* runner, const hypo::SvgElement& x) {
  return std::visit(
      [runner](const auto& y) {
        auto future =
            runner->MakeFutureWithoutCaching<Element>(&ConstructSvgElement, y);
        future.Get();
        return std::move(future);
      },
      static_cast<const hypo::SvgElement::AsVariant&>(x));
}

Elements ConstructSvgElementsInternal(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::SvgElements& x) {
  std::vector<FutureElement> children;
  children.reserve(x.elements.size());
  for (const hypo::SvgElement& element : x.elements) {
    children.push_back(ConstructSvgElementFuture(runner, element));
  }

  Elements result;
  result.reserve(children.size());
  for (auto& child : children) {
    result.push_back(child.Get());
  }

  return result;
}

}  // namespace

FutureElements ConstructSvgElements(
    reify::pure_cpp::ThreadPoolCacheRunner* runner,
    const hypo::SvgElements& x) {
  auto future = runner->MakeFutureWithoutCaching<Elements>(
      &ConstructSvgElementsInternal, x);
  future.Get();
  return future;
}

}  // namespace svg
}  // namespace hypo
