let
  config = (import ./release.nix).config; 

  pkgs = import <nixpkgs> { inherit config; };

  ghc = pkgs.haskellPackages.ghcWithHoogle (hpkgs: with hpkgs;
    [ cabal-install
    ] ++ xrandr.buildInputs
  );
in

pkgs.mkShell 
{ buildInputs = [ ghc ];
}
