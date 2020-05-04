{stdenv, cmake, memdesc, v8, reify-interface, clang-tools, closurecompiler}:
 
stdenv.mkDerivation {
  name = "reify";
  
  nativeBuildInputs = [cmake memdesc reify-interface clang-tools closurecompiler];
  buildInputs = [ v8 ];

  src = ../.;

  enableParallelBuilding = true;

  cmakeFlags = [
    ("-DREIFY_INTERFACE_GENERATOR_DIRECTORY:PATH=" +
      (reify-interface.outPath + "/bin"))
  ];
}
