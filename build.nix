let
  config = import ./release.nix;
  pkgs = import <nixpkgs> { inherit config; };
in

pkgs.haskellPackages.xrandr
