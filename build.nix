let
  config = (import ./release.nix).config;
  pkgs = import <nixpkgs> { inherit config; };
in

pkgs.haskellPackages.xrandr
