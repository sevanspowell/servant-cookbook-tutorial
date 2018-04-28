{ mkDerivation, aeson, base, wai, warp, servant-server, stdenv }:

mkDerivation {
  pname = "servant-cookbook";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base wai warp servant-server
  ];
  executableHaskellDepends = [ base ];
  description = "Servant cookbook tutorial";
  license = stdenv.lib.licenses.bsd3;
}