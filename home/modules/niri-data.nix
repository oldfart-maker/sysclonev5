# home/modules/niri-data.nix
{ config, lib, ... }:

let
  # Paths in your repo
  niriRoot   = ../dotfiles/niri;
  scriptsDir = niriRoot + "/scripts";
  rofiDir    = niriRoot + "/rofi";
  makoDir    = niriRoot + "/mako";

  linkDir = src: target: {
    ${target} = {
      source    = src;
      recursive = true;
      force     = true;
    };
  };
in
{
  # Nice errors if you forget to commit folders you expect to exist
  assertions = [
    { assertion = builtins.pathExists scriptsDir;
      message   = "Expected dotfiles/niri/scripts in the repo."; }
  ];

  # Link only the folders that actually exist in the repo
  home.file =
    lib.mkMerge [
      (lib.optionalAttrs (builtins.pathExists scriptsDir)
        (linkDir scriptsDir ".config/niri/scripts"))

      (lib.optionalAttrs (builtins.pathExists rofiDir)
        (linkDir rofiDir ".config/niri/rofi"))

      (lib.optionalAttrs (builtins.pathExists makoDir)
        (linkDir makoDir ".config/niri/mako"))
    ];

  xdg.configFile."mako/config" =
    lib.mkIf (builtins.pathExists (makoDir + "/config")) {
      source = makoDir + "/config";
    };
}
