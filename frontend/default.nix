with (import <nixpkgs> {});
let
  pname = "acstalks-frontend";

  env = bundlerEnv {
    name = "${pname}";
    inherit ruby;
    gemfile = ./Gemfile;
    lockfile = ./Gemfile.lock;
    gemset = ./gemset.nix;
  };
in stdenv.mkDerivation {
  name = "${pname}";
  buildInputs = [ env ruby ];
}
