inputs: { config, build, lib, pkgs, ... }:

let
  sourceFilter = root: with lib.fileset; toSource {
    inherit root;
    fileset = fileFilter (file: lib.any file.hasExt [ "cabal" "hs" "md" ]) root;
  };
  pname = "guzzle";
  defaultGhc = inputs.nixpkgs.legacyPackages.${config.system}.haskellPackages.ghc;
  defaultGhcVersion = "ghc" + lib.replaceStrings [ "." ] [ "" ] defaultGhc.version;
in
{
  compiler = defaultGhcVersion;
  ghcVersions = [ defaultGhcVersion ];
  systems = builtins.attrNames inputs.nixpkgs.legacyPackages;
  cabal = {
    author = "ners";
    build-type = "Simple";
    license = "Apache-2.0";
    license-file = "LICENCE.md";
    version = "0.1.0.0";
    meta = {
      maintainer = "ners@gmx.ch";
      homepage = "https://github.com/ners/${pname}";
      synopsis = "guzzle your contents in style ✨";
    };
    language = "GHC2021";
    default-extensions = [
      "ApplicativeDo"
      "BlockArguments"
      "DataKinds"
      "DefaultSignatures"
      "DeriveAnyClass"
      "DeriveGeneric"
      "DerivingVia"
      "ExplicitNamespaces"
      "LambdaCase"
      "NoImplicitPrelude"
      "OverloadedLabels"
      "OverloadedLists"
      "OverloadedStrings"
      "PackageImports"
      "RecordWildCards"
      "StrictData"
      "TypeFamilies"
      "ViewPatterns"
    ];
    ghc-options = [
      "-Weverything"
      "-Wno-unsafe"
      "-Wno-all-missed-specialisations"
      "-Wno-missing-export-lists"
      "-Wno-missing-import-lists"
      "-Wno-missing-kind-signatures"
      "-Wno-missing-role-annotations"
      "-Wno-missing-safe-haskell-mode"
    ];
  };
  packages.${pname} = {
    src = sourceFilter ./.;
    executable = {
      enable = true;
      source-dirs = "app";
      dependencies = [
        "aeson"
        "ansi-terminal"
        "bytestring"
        "extra"
        "optparse-applicative"
        "text"
        "typed-process"
      ];
    };
  };
  outputs.packages =
    let
      unwrapped = build.packages.dev.${pname}.package.overrideAttrs {
        pname = "${pname}-unwrapped";
      };
      wrapped = 
        pkgs.runCommand "${pname}-${unwrapped.version}"
          {
            nativeBuildInputs = with pkgs; [ makeWrapper ];
            inherit pname;
            inherit (unwrapped) version meta;
          } ''
          makeWrapper ${lib.getExe unwrapped} $out/bin/${pname} --set PATH "${lib.makeBinPath runtimeDeps}";
        '';
      runtimeDeps = with pkgs; [
        grim
        slurp
        wf-recorder
      ];
    in {
      default = wrapped;
      ${pname} = wrapped;
      "${pname}-unwrapped" = unwrapped;
    };
  envs.dev = {
    env.DIRENV_IN_ENVRC = "";
    setup-pre = /*bash*/ ''
      NIX_MONITOR=disable nix run .#gen-cabal
      NIX_MONITOR=disable nix run .#tags
    '';
    buildInputs = with config.pkgs; [
    ];
  };
}
