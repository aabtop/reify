{clangStdenv, cmake, memdesc, v8, reify-interface, clang-tools}:
 
clangStdenv.mkDerivation {
  name = "reify";
  
  nativeBuildInputs = [cmake memdesc reify-interface clang-tools];
  buildInputs = [ v8 ];

  src = ../.;

  enableParallelBuilding = true;
}
