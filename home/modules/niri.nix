# home/modules/niri.nix
{ config, lib, ... }:

let
  cfgPath  = ../../generated/niri/config.kdl;
  keysPath = ../../generated/niri/key_bindings.txt;
  niriDir  = "${config.xdg.configHome}/niri";
in
{
  # Ensure our definition overrides any other module's .source/.text
  home.file."${niriDir}/config.kdl" = lib.mkForce {
    text = builtins.readFile cfgPath;
  };

  # Add key_bindings.txt only if it exists (pure)
  home.file = lib.optionalAttrs (builtins.pathExists keysPath) {
    "${niriDir}/key_bindings.txt".text = builtins.readFile keysPath;
  };
}
