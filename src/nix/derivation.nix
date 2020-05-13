# While reifyProject describes how to setup a CMake project that presents
# a custom interface, this derivation will actually build that CMake project.
{stdenv, lib, cmake, v8, reifyProject, clang-tools}:
 
stdenv.mkDerivation {
  name = lib.strings.removeSuffix "-project" reifyProject.name;

  nativeBuildInputs = [cmake reifyProject clang-tools];
  propagatedBuildInputs = [ v8 ];

  src = reifyProject;

  enableParallelBuilding = true;
}
