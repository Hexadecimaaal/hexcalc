{
  description = "";

  inputs = {
    nixpkgs.url      = "github:nixos/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    flake-utils.url  = "github:numtide/flake-utils";
    flake-utils.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
      with pkgs;
      {
        devShell = mkShell {
          buildInputs = [
            openssl
            pkgconfig
            (rust-bin.nightly."2021-11-01".default.override {
              extensions = [ "rust-src" ];
              targets = ["thumbv6m-none-eabi"];
            })
            (openocd.overrideAttrs (old: {
              src = fetchFromGitHub {
                owner = "raspberrypi";
                repo = "openocd";
                rev =  "4f2ae619714c9565a7e2daa28f3b3d1a714305e9";
                hash = "sha256-4d/awbyDhDzqk8xnOu/Rn43M2uRkRnwq/u9MHmNnbXI=";
                fetchSubmodules = true;
              };
              nativeBuildInputs = old.nativeBuildInputs ++ [
                which
                libtool
                automake
                autoconf
              ];
              SKIP_SUBMODULE = "yaaaass";
              preConfigure = "./bootstrap";
            }))
            libusb1
            flip-link
          ];
        };
      }
    );
}
