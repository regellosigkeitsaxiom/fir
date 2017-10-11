{ pkgs ? import <nixpkgs> {} }:

let
  minimal = pkgs.callPackage ./database/minimal.nix {};
  extended = pkgs.callPackage ./database/extended.nix {};
  examples = pkgs.callPackage ./database/examples.nix {};
  mcus = pkgs.callPackage ./database/mcus.nix {};
  documents = pkgs.callPackage ./database/documents.nix {};
  inherit (pkgs) haskell;
  example = pkgs.haskellPackages.callPackage ./auto.nix {};

  addRuntimeDependency = drv: x: addRuntimeDependencies drv x;
  addRuntimeDependencies = drv: xs: haskell.lib.overrideCabal drv (drv: {
    buildDepends = (drv.buildDepends or []) ++ [ pkgs.makeWrapper ];
    postInstall = ''
      mkdir -p $out/database/
      ln -s ${minimal}/CMSIS $out/database/CMSIS
      ln -s ${minimal}/startups $out/database/startups
      ln -s ${minimal}/linkers $out/database/linkers
      ln -s ${extended}/HAL $out/database/HAL
      ln -s ${examples}/examples $out/database/examples
      ln -s ${mcus}/mcu $out/database/mcu
      ln -s ${documents}/documents $out/database/documents
      ${drv.postInstall or ""}
      for exe in "$out/bin/"* ; do
        wrapProgram "$exe" --prefix PATH ":" \
          ${pkgs.lib.makeBinPath xs}
      done
    '';
  });
  deps = with pkgs; [ gcc-arm-embedded ];

in addRuntimeDependency example deps
