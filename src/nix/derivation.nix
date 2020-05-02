{stdenv, cmake, memdesc, v8, reify-interface, clang-tools}:
 
stdenv.mkDerivation {
  name = "reify";
  
  nativeBuildInputs = [cmake memdesc reify-interface clang-tools];
  buildInputs = [ v8 ];

  src = ../.;

  enableParallelBuilding = true;

  cmakeFlags = [
    ("-DREIFY_INTERFACE_GENERATOR_DIRECTORY:PATH=" +
      (reify-interface.outPath + "/bin"))
  ];
}
