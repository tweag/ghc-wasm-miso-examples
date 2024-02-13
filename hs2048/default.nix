{ pkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a0aeb23";
    sha256 = "04dgg0f2839c1kvlhc45hcksmjzr8a22q1bgfnrx71935ilxl33d";
  }){}
}:
let
  result = pkgs.haskell.packages.ghcjs.callCabal2nix "miso" (pkgs.fetchFromGitHub {
    rev = "adea51505f30853caae76f38faa5e9f192ae8827";
    sha256 = "0x8dik2cx5j11svb091pzy2ycvhbb88534fng2v9bic5yx1a8c72";
    owner = "haskell-miso";
    repo = "miso";
  }) {};
  hs2048 = pkgs.haskell.packages.ghcjs.callPackage ./hs-2048.nix {
    miso = result;
  };
  inherit (pkgs) sass closurecompiler;
in
  pkgs.runCommand "hs2048" { inherit hs2048; } ''
    mkdir -p $out
    ${sass}/bin/sass ${hs2048.src}/static/main.scss $out/main.css
    ${closurecompiler}/bin/closure-compiler ${hs2048}/bin/app.jsexe/all.js > $out/all.js
    cp ${hs2048.src}/static/index.html $out/index.html
  ''
