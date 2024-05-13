{
  inputs = {
    ghc-wasm-meta.url = "gitlab:ghc/ghc-wasm-meta?host=gitlab.haskell.org";
  };
  outputs = inputs: inputs.ghc-wasm-meta.inputs.flake-utils.lib.eachDefaultSystem (system:
    let pkgs = inputs.ghc-wasm-meta.inputs.nixpkgs.legacyPackages.${system};
    in
    {
      devShells.default = pkgs.mkShell {
        packages = [
          inputs.ghc-wasm-meta.packages.${system}.all_9_10
          pkgs.dart-sass
        ];
      };
    });
}
