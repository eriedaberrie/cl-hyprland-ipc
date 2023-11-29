{
  lib,
  lispImpl,
}:
lispImpl.buildASDFSystem {
  pname = "cl-hyprland-ipc";
  version = "0.1.0";
  src = lib.cleanSource ../.;
  lispLibs = with lispImpl.pkgs; [alexandria babel jzon split-sequence];
}
