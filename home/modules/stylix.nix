# modules/stylix.nix
{ config, pkgs, lib, ... }:
{
  stylix = {
    enable   = true;

    # Pick ONE of these sources (image OR base16 scheme)
    # image    = "${config.home.homeDirectory}/Pictures/wallpapers/waves.jpg";
    # polarity = "dark";
    
    base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-mocha.yaml";

    targets = {
      waybar = {
        enable = false;
        addCss = false;  # we'll provide our own style/colors
      };
      rofi = {
        enable = true;
      };
    };
  };
}
