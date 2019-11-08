let
  pkgs = import <nixpkgs> { inherit config; };

  ghc = pkgs.haskellPackages.ghcWithHoogle (hpkgs: with hpkgs;
    [ cabal-install
    ] ++ xrandr.buildInputs
  );
in

mkShell 
{ buildInputs = [ ghc ];
}
