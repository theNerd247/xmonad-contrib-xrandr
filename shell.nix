let
  config = import ./release.nix; 

  pkgs = import <nixpkgs> { inherit config; };

  ghc = pkgs.haskellPackages.ghcWithHoogle (hpkgs: with hpkgs;
    [ cabal-install
    ] ++ xrandr.buildInputs
  );
in

pkgs.mkShell 
{ buildInputs = [ ghc ];
}
