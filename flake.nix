{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let pkgs = inputs.nixpkgs.legacyPackages.${system};
    in
    {
      devShells.default = pkgs.mkShell {
        packages = [
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
