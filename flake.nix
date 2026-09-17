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
      runtimeDependenciesFor = pkgs: with pkgs; [
        grim
        libnotify
        slurp
        wf-recorder
        wl-clipboard
      ];
      overlay = lib.composeManyExtensions [
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (hfinal: hprev: with prev.haskell.lib.compose; {
                ${pname} = (hfinal.callCabal2nix pname (sourceFilter ./.) {
                  optparse-applicative = lib.pipe { } [
                    (_: hprev.optparse-applicative_0_19_0_0)
                    (appendPatch ./arg-backtracking.patch)
                    dontCheck
                  ];
                }).overrideAttrs (attrs: {
                  nativeBuildInputs = attrs.nativeBuildInputs or [ ] ++ [
                    prev.makeWrapper
                    prev.installShellFiles
                  ];
                  postInstall = ''
                    ${attrs.postInstall or ""}
                    wrapProgram $out/bin/${pname} --prefix PATH : "${lib.makeBinPath (runtimeDependenciesFor prev)}"
                    installShellCompletion --cmd ${pname} \
                      --bash <($out/bin/${pname} --bash-completion-script $out/bin/${pname}) \
                      --fish <($out/bin/${pname} --fish-completion-script $out/bin/${pname}) \
                      --zsh <($out/bin/${pname} --zsh-completion-script $out/bin/${pname})
                  '';
                });
              })
            ];
          };
          ${pname} = final.haskellPackages.${pname};
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
        packages.${system}.default = pkgs.${pname};
        devShells.${system}.default = pkgs.haskellPackages.shellFor {
          packages = ps: [ ps.${pname} ];
          nativeBuildInputs = with pkgs.haskellPackages; [
            cabal-gild
            cabal-install
            fourmolu
            haskell-language-server
          ]
          ++ runtimeDependenciesFor pkgs;
        };
      }
    );
}
