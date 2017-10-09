{ pkgs ? import <nixpkgs> {} }:

let
  database = pkgs.callPackage ./database/default.nix {};
  inherit (pkgs) haskell;
  example = pkgs.haskellPackages.callPackage ./auto.nix {};

  addRuntimeDependency = drv: x: addRuntimeDependencies drv x;
  addRuntimeDependencies = drv: xs: haskell.lib.overrideCabal drv (drv: {
    buildDepends = (drv.buildDepends or []) ++ [ pkgs.makeWrapper ];
    postInstall = ''
      #cp -r database $out/
      ln -s ${database} $out/database
      ${drv.postInstall or ""}
      for exe in "$out/bin/"* ; do
        wrapProgram "$exe" --prefix PATH ":" \
          ${pkgs.lib.makeBinPath xs}
      done
    '';
  });
  deps = with pkgs; [ gcc-arm-embedded ];

in addRuntimeDependency example deps
