# home/modules/niri.nix
{ config, lib, ... }:

let
  # These are path literals, resolved relative to THIS file.
  # Adjust if you move your repo layout again.
  cfgPath  = ../generated/niri/config.kdl;
  keysPath = ../generated/niri/key_bindings.txt;
in
{
  # Write into ~/.config/niri/*
  # We define the whole attrset for xdg.configFile in one place,
  # and then extend it with an optional entry for key_bindings.txt.
  xdg.configFile =
    # Always install config.kdl from your repo
    {
      "niri/config.kdl" = {
        force = true;                         # clobber stale files safely
        text  = builtins.readFile cfgPath;    # pure: content baked into the store
      };
    }
    # Optionally install key_bindings.txt if present in the repo
    lib.optionalAttrs (builtins.pathExists keysPath) {
      "niri/key_bindings.txt" = {
        force = true;
        text  = builtins.readFile keysPath;
      };
    };
}
