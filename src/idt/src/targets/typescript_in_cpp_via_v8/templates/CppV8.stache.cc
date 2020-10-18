// {{!
// clang-format off
// }}
#include "{{v8HeaderFile}}"

#include <cassert>
#include <v8.h>

#include "{{immutableRefCountedHeaderFile}}"
#include "context_environment.h"

namespace {{namespace}} {

{{#convertToImmRefCntFunctions}}
{{{.}}}
{{/convertToImmRefCntFunctions}}

}  // namespace {{namespace}}
