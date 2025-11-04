{
  description = "A flake that loads some packages";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ghcVersion = "928";
        ghc = pkgs.haskell.compiler."ghc${ghcVersion}";
        hls = pkgs.haskell-language-server.override {
            supportedGhcVersions = [ ghcVersion ];
        };
      in
      rec {
        devShells.default = pkgs.mkShell {
          packages = [
            hls
          ] ++ (with pkgs; [
            stack
          ]);
        };
      }
    );
}
