{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    ghc-wasm-meta.url = "gitlab:ghc/ghc-wasm-meta?host=gitlab.haskell.org";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let pkgs = inputs.nixpkgs.legacyPackages.${system};
    in
    {
      devShells.default = pkgs.mkShell {
        packages = [
          inputs.ghc-wasm-meta.packages.${system}.all_gmp
          pkgs.nodejs_21
          pkgs.npm-check-updates
          pkgs.dart-sass
          pkgs.esbuild
          pkgs.binaryen
          pkgs.wabt
          pkgs.zlib
        ];
      };
    });
}
