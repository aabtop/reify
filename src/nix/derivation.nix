{stdenv, cmake, memdesc, v8, reify-interface}:
 
stdenv.mkDerivation {
  name = "reify";
  
  nativeBuildInputs = [ cmake memdesc reify-interface ];
  buildInputs = [ v8 ];

  src = ../.;

  enableParallelBuilding = true;
}
