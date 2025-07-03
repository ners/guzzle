{
  nixConfig = {
    extra-substituters = "https://cache.ners.ch/haskell";
    extra-trusted-public-keys = "haskell:WskuxROW5pPy83rt3ZXnff09gvnu80yovdeKDw5Gi3o=";
  };

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = inputs:
    with builtins;
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      sourceFilter = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter
          (file: any file.hasExt [ "cabal" "hs" "md" ])
          root;
      };
      pname = "guzzle";
      overlay = lib.composeManyExtensions [
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (hfinal: hprev: with prev.haskell.lib.compose; {
                ${pname} = hfinal.callCabal2nix pname (sourceFilter ./.) {
                  optparse-applicative = lib.pipe { } [
                    (hprev.callHackageDirect
                      {
                        pkg = "optparse-applicative";
                        ver = "0.19.0.0";
                        sha256 = "sha256-dhqvRILfdbpYPMxC+WpAyO0KUfq2nLopGk1NdSN2SDM=";
                      })
                    (appendPatch ./arg-backtracking.patch)
                    dontCheck
                  ];
                };
              })
            ];
          };
        })
      ];
    in
    {
      overlays.default = overlay;
    }
    //
    foreach inputs.nixpkgs.legacyPackages (system: pkgs':
      let pkgs = pkgs'.extend overlay; in {
        formatter.${system} = pkgs.nixpkgs-fmt;
        legacyPackages.${system} = pkgs;
        packages.${system}.default = pkgs.haskellPackages.${pname};
        devShells.${system}.default = pkgs.haskellPackages.shellFor {
          packages = ps: [ ps.${pname} ];
          nativeBuildInputs = with pkgs.haskellPackages; [
            cabal-gild
            cabal-install
            fourmolu
            haskell-language-server
          ];
        };
      }
    );
}
