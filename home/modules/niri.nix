# home/modules/niri.nix
{ config, lib, ... }:

let
  cfgPath  = ../../generated/niri/config.kdl;
  keysPath = ../../generated/niri/key_bindings.txt;
  niriDir  = "${config.xdg.configHome}/niri";
in
{
  # Build a single home.file attrset:
  home.file =
    # Conditionally add key_bindings.txt if it exists (pure, eval-time)
    (lib.optionalAttrs (builtins.pathExists keysPath) {
      "${niriDir}/key_bindings.txt".text = builtins.readFile keysPath;
    })
    # Always add config.kdl, and force our definition to win over any other module.
    // {
      "${niriDir}/config.kdl" = lib.mkForce {
        text = builtins.readFile cfgPath;
      };
    };
}
