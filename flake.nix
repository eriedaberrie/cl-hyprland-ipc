{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default-linux";
  };

  outputs = {
    self,
    nixpkgs,
    systems,
    ...
  }: let
    inherit (nixpkgs) lib;
    forSystems = f:
      lib.genAttrs (import systems) (system: f nixpkgs.legacyPackages.${system});
  in {
    overlays = {
      default = _: prev: {
        cl-hyprland-ipc = prev.callPackage ./nix {
          lispImpl = prev.sbcl;
        };
      };
    };

    packages = forSystems (pkgs: rec {
      inherit (self.overlays.default pkgs pkgs) cl-hyprland-ipc;
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
