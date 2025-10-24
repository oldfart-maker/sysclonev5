# modules/rofi-colors-compat.nix
{ config, pkgs, lib, ... }:

let
  ch = config.lib.stylix.colors.withHashtag;
  sharedDir = "${config.home.homeDirectory}/.config/niri/rofi/shared";
in
{
  home.file."${sharedDir}/colors.rasi".text = ''
    * {
      background:     ${ch.base00};
      background-alt: ${ch.base01};
      foreground:     ${ch.base05};
      selected:       ${ch.base0D};
      active:         ${ch.base0E};
      urgent:         ${ch.base08};
    }
  '';
}
