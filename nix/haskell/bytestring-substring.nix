{ mkDerivation, base, bytestring, fetchgit, pipes, primitive
, stdenv
}:
mkDerivation {
  pname = "bytestring-substring";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/chessai/bytestring-substring.git";
    sha256 = "0x73a9jq8ih9v9bngvxy5k43ki5pc6pmq29kz86fxy08nbirf2zx";
    rev = "826e5a393d4f7106cb10006b649c3c1e7c38994f";
  };
  libraryHaskellDepends = [ base bytestring pipes primitive ];
  homepage = "https://github.com/chessai/bytestring-substring";
  license = stdenv.lib.licenses.bsd3;
}
