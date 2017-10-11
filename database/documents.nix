{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "fir-documents-0.1.0.0";
  src = ./.;
  dontBuild = true;
  installPhase = ''
    mkdir -p $out
    cp -r documents $out/
  '';
}
