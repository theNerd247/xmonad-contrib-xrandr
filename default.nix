{ mkDerivation, attoparsec, base, recursion-schemes, shelly, stdenv
, text
}:
mkDerivation {
  pname = "xrandr";
  version = "0.1.0.1";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base recursion-schemes shelly text
  ];
  doHaddock = false;
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
