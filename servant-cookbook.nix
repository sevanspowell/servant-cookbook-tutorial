{ mkDerivation, aeson, base, bytestring, http-client
, markdown-unlit, postgresql-simple, resource-pool, servant
, servant-client, servant-server, sqlite-simple, stdenv, stm, text
, transformers
}:
mkDerivation {
  pname = "servant-cookbook";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring http-client markdown-unlit postgresql-simple
    resource-pool servant servant-client servant-server sqlite-simple
    stm text transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}
