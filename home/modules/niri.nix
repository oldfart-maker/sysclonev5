# home/modules/niri.nix
{ config, lib, ... }:

let
  cfgPath  = ../generated/niri/config.kdl;
  keysPath = ../generated/niri/key_bindings.txt;
  niriDir  = "${config.xdg.configHome}/niri";
in
{
  home.file =
    (lib.optionalAttrs (builtins.pathExists keysPath) {
      "${niriDir}/key_bindings.txt".text = builtins.readFile keysPath;
    })
    // {
      "${niriDir}/config.kdl" = lib.mkForce {
        text = builtins.readFile cfgPath;
      };
    };

  xdg.configFile."niri/config.kdl" = {
    force = true;
    text  = builtins.readFile cfgPath;
};
}
