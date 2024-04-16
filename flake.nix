{
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";

  outputs = {
    self,
    nixpkgs,
    ...
  }: let
    inherit (nixpkgs) lib;
    forSystems = f:
      lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
      ] (system: f nixpkgs.legacyPackages.${system});
  in {
    packages = forSystems (pkgs: rec {
      cl-hyprland-ipc = pkgs.callPackage ./nix {
        lispImpl = pkgs.sbcl;
      };
      default = cl-hyprland-ipc;
    });

    devShells = forSystems (
      pkgs: {
        default = pkgs.mkShell {
          name = "cl-hyprland-ipc-shell";
          nativeBuildInputs = [
            (pkgs.sbcl.withPackages (lib.const self.packages.${pkgs.system}.cl-hyprland-ipc.lispLibs))
          ];
        };
      }
    );

    formatter = forSystems (pkgs: pkgs.alejandra);
  };
}
