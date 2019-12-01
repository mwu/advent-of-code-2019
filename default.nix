{ nixpkgs ? import (builtins.fetchTarball {
    name = "nixos-unstable-2019-12-01";
    url = https://github.com/nixos/nixpkgs/archive/0ee0489d42e6f0df5991113caee6feae97bca057.tar.gz;
    sha256 = "1ldlg2nm8fcxszc29rngw2893z8ci4bpa3m0i6kfwjadfrcrfa42";
  }) {}
, compiler ? "ghc881" }:

let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) lib;

  haskellPackages = pkgs.haskell.packages.${compiler};

  adventOfCode2019 = haskellPackages.callCabal2nix "advent-of-code-2019" (
    lib.sourceByRegex ./. [
      "^src.*$"
      "package.yaml"
    ]
  ) {};
in {
  inherit adventOfCode2019;
  inherit haskellPackages;
}
