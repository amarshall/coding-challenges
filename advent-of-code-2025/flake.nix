{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , devshell
    , flake-utils
    }:
    flake-utils.lib.eachDefaultSystem (system: (
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            devshell.overlays.default
          ];
        };
      in
      {
        devShells.default = pkgs.devshell.mkShell {
          motd = "";
          devshell.packages = [
            (pkgs.haskellPackages.ghcWithPackages (hpkgs: [
              hpkgs.executable-path
              hpkgs.extra
              # hpkgs.hgeometry
              hpkgs.matrix
              hpkgs.parallel
              hpkgs.regex-with-pcre
              hpkgs.split
            ]))
            pkgs.haskell-language-server
            (pkgs.writeScriptBin "runhaskell2" ''
              runhaskell -i"$(dirname "$1")" "$@"
            '')
            pkgs.hyperfine
            pkgs.nixpkgs-fmt
            pkgs.ruby_3_4
          ];
        };
      }
    ));
}
