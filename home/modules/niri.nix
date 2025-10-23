# home/modules/niri.nix
{ config, lib, ... }:

let
  # Use path literals (pure). These are relative to this file's location.
  cfgPath  = ../../generated/niri/config.kdl;
  keysPath = ../../generated/niri/key_bindings.txt;

  niriDir  = "${config.xdg.configHome}/niri";
in
{
  # 1) Always install config.kdl from the repo
  home.file."${niriDir}/config.kdl".text = builtins.readFile cfgPath;

  # 2) Optionally install key_bindings.txt if present in the repo
  #    (pure check during evaluation)
  #    If the file isn't there, nothing is created.
  lib.mkIf (builtins.pathExists keysPath) {
    home.file."${niriDir}/key_bindings.txt".text = builtins.readFile keysPath;
  }
}
