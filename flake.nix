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
      import nixpkgs {
        inherit system;
        overlays = [self.overlays.default];
      };
  in {
    overlays.default = _: prev: {
      sbcl = prev.sbcl.withOverrides (_: prev': {
        cl-hyprland-ipc = prev.sbcl.buildASDFSystem {
          pname = "cl-hyprland-ipc";
          version = "0.1.0";
          src = lib.cleanSource ./.;
          lispLibs = with prev'; [babel jzon];
        };
      });
    };

    packages = forSystems (system: rec {
      inherit (pkgsFor system) sbcl;
      inherit (sbcl.pkgs) cl-hyprland-ipc;
      default = cl-hyprland-ipc;
    });

    devShells = forSystems (
      system: let
        pkgs = pkgsFor system;
      in {
        default = pkgs.mkShell {
          name = "cl-hyprland-ipc-shell";
          nativeBuildInputs = [(pkgs.sbcl.withPackages (ps: ps.cl-hyprland-ipc.lispLibs))];
        };
      }
    );

    formatter = forSystems (system: (pkgsFor system).alejandra);
  };
}
