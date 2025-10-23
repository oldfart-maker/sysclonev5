# home/modules/niri.nix
{ config, lib, ... }:

let
  cfgPath  = ../../generated/niri/config.kdl;
  keysPath = ../../generated/niri/key_bindings.txt;
  niriDir  = "${config.xdg.configHome}/niri";
in
{
  # Always install config.kdl from the repo
  home.file =

    # Conditionally add key_bindings.txt if it exists (pure, eval-time)
    (lib.optionalAttrs (builtins.pathExists keysPath) {
      "${niriDir}/key_bindings.txt".text = builtins.readFile keysPath;
    })

    # Always-present entries go here
    // {
      "${niriDir}/config.kdl".text = builtins.readFile cfgPath;
    };
}
