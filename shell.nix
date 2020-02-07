{ pkgs ? import <nixpkgs> {} }:

let
  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "ca1159603ecdb1098adbc3255a60b43763058634";
    sha256 = "0cjagghlamcbxflapnsg8sa3s771mj7jnrn4vay0jz64hxzphzfb";
  }) {
    inherit pkgs;
  };

in pkgs.mkShell {
  buildInputs = [ easy-ps.purs easy-ps.spago pkgs.nodejs pkgs.nodePackages.parcel-bundler easy-ps.purty ];
}
