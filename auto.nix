{ mkDerivation, aeson, base, bytestring, directory, mtl, process
, rainbow, safe, stdenv, system-filepath, yaml
}:
mkDerivation {
  pname = "fir";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring directory mtl process rainbow safe
    system-filepath yaml
  ];
  homepage = "https://github.com/regellosigkeitsaxiom/fir#readme";
  license = stdenv.lib.licenses.bsd3;
}
