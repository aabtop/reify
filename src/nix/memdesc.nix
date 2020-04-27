{stdenv, fetchFromGitHub, cmake, bison, flex}:
 
stdenv.mkDerivation {
  name = "memdesc";
  src = fetchFromGitHub {
    owner = "aabtop";
    repo = "memdesc";
    rev = "67b7a48b83c5d4e10bf78b1c9327877a0f9cca62";
    sha256 = "1s4qpngq8mcflhmg4hl489wiacb34k88yy3zlrail38wgnijrfjq";
  } + "/src";

  nativeBuildInputs = [ cmake bison flex ];
  buildInputs = [];
  checkInputs = [];

  enableParallelBuilding = true;

  makeTarget = "memdesc";
  installPhase = "mkdir -p $out/bin; cp -r package/* $out/bin";
}
