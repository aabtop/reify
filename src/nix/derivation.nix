{stdenv, cmake, memdesc, v8}:
 
stdenv.mkDerivation {
  name = "reify";
  
  nativeBuildInputs = [ cmake memdesc ];
  buildInputs = [ v8 ];

  src = ../.;

  enableParallelBuilding = true;
}
