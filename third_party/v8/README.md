# V8 in Reify

This directory declares some Bazel rules to enable V8 to be fetched and
built within Bazel.  That said, it still uses GN to do the building, not
Bazel, so it is a bit awkward.  We use the fetch_*.sh/.bat and build_*.sh/.bat
files to define the process of how to fetch and build V8.
