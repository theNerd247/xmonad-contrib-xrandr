{ mkDerivation, attoparsec, base, recursion-schemes, shelly, stdenv
, text
}:
mkDerivation {
  pname = "xrandr";
  version = "0.3.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base recursion-schemes shelly text
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
