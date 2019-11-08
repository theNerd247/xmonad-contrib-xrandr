{ config = 
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
}
