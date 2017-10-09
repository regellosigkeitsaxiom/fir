{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "fir-database-0.1.0.0";
  src = ./.;
  dontBuild = true;
  installPhase = ''
    echo Iceberg
    mkdir -p $out
    cp -r . $out/
  '';
}
