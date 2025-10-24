{ config, lib, ... }:

let
  cfgPath  = ../generated/niri/config.kdl;
  keysPath = ../generated/niri/key_bindings.txt;
in
{
  # keep the fingerprint for now
  home.sessionVariables.NIRI_MODULE_FINGERPRINT = "niri.nix imported";

  # fail loudly if the main file is missing
  assertions = [{
    assertion = builtins.pathExists cfgPath;
    message = "niri.nix: ../generated/niri/config.kdl not found next to this module.";
  }];

  # Use home.file so outputs appear under result/home-files
  home.file = {
    ".config/niri/config.kdl" = {
      force  = true;
      source = cfgPath;
    };
  }
  // lib.optionalAttrs (builtins.pathExists keysPath) {
    ".config/niri/key_bindings.txt" = {
      force  = true;
      source = keysPath;
    };
  };
}
