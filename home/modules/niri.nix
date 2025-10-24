# home/modules/niri.nix
{ config, lib, ... }:

let
  cfgPath  = ../generated/niri/config.kdl;
  keysPath = ../generated/niri/key_bindings.txt;
in
{
  # diagnostic: prove this module is imported (remove later)
  home.sessionVariables.NIRI_MODULE_FINGERPRINT = "niri.nix imported";

  # make sure XDG is on (belt & suspenders)
  xdg.enable = true;

  # fail loudly if the main file isn't present in the repo
  assertions = [{
    assertion = builtins.pathExists cfgPath;
    message = "niri.nix: ../generated/niri/config.kdl not found next to this module.";
  }];

  xdg.configFile =
    {
      "niri/config.kdl" = {
        force  = true;
        source = cfgPath;   # <-- use source, not text
      };
    }
    // lib.optionalAttrs (builtins.pathExists keysPath) {
      "niri/key_bindings.txt" = {
        force  = true;
        source = keysPath;  # <-- use source, not text
      };
    };
}
