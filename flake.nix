{
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";

  outputs = {
    self,
    nixpkgs,
    ...
  }: let
    inherit (nixpkgs) lib;
    forSystems = lib.genAttrs [
      "x86_64-linux"
    ];
    pkgsFor = system:
      import nixpkgs {inherit system;};
  in {
    packages = forSystems (system: let
      pkgs = pkgsFor system;
    in rec {
      cl-hyprland-ipc = pkgs.callPackage ./nix {
        lispImpl = pkgs.sbcl;
      };
      default = cl-hyprland-ipc;
    });

    devShells = forSystems (
      system: let
        pkgs = pkgsFor system;
      in {
        default = pkgs.mkShell {
          name = "cl-hyprland-ipc-shell";
          nativeBuildInputs = [
            (pkgs.sbcl.withPackages (_: self.packages.${system}.cl-hyprland-ipc.lispLibs))
          ];
        };
      }
    );

    formatter = forSystems (system: (pkgsFor system).alejandra);
  };
}
