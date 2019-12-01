with import ./default.nix {};

haskellPackages.shellFor {
  packages = p: [
    adventOfCode2019
  ];

  nativeBuildInputs = with haskellPackages; [
    cabal-install
    ghcid
    hpack
  ];
}
