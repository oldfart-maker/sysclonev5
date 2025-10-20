# home/modules/system-theme.nix
{ config, pkgs, lib, ... }:
let
  useImage   = false;  # flip to false to use base16
  wallpaper  = "/home/username/Pictures/walls/gruv-rock.jpg";
  base16File = ./themes/catppuccin-mocha.yaml;
in
{
  stylix.enable = true;
  stylix.image = lib.mkIf useImage wallpaper;
  stylix.base16Scheme = lib.mkIf (!useImage) base16File;
  stylix.polarity = "dark";

  stylix.fonts = {
    monospace = "JetBrains Mono";
    sansSerif = "DejaVu Sans";
    serif     = "DejaVu Serif";
  };
  stylix.cursor = {
    package = pkgs.qogir-icon-theme;
    name    = "Qogirr";
    size    = 12;
  };

  # Example: let Stylix manage Alacritty fully
  stylix.targets.alacritty.enable = true;

  # Keep Stylix hands-off where you have custom modules
  stylix.targets.waybar = {
    enable = false;
    addCss = false;
  };
  stylix.targets.rofi.enable = false;
  stylix.targets.foot.enable = false;

  # Ensure themes/fonts exist
  home.packages = with pkgs; [
    jetbrains-mono
    qogir-icon-theme
    # add your GTK/icon theme pkgs if you set gtk.theme/iconTheme
  ];
}
