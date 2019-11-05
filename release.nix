let
  config = 
  {
    packageOverrides = pkgs: 
    {
      haskellPackages = pkgs.haskellPackages.override 
      {
        overrides = new: old:
        {
          xrandr = new.callPackage ./default.nix {};
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

  ghc = pkgs.haskellPackages.ghcWithHoogle (hpkgs: with hpkgs;
    [ cabal-install
    ] ++ xrandr.buildInputs
  );
in

with pkgs;

if lib.inNixShell then
  mkShell 
  { buildInputs = [ ghc ];
  }
else
{ xrandr = haskellPackages.xrandr;
}
