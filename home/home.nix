{ config, pkgs, lib, ... }:
{
  home.username = "username";
  home.homeDirectory = "/home/username";
  home.stateVersion = "24.05";

  # keep git installed via HM from now on
  home.packages = with pkgs; [ git ];

  imports = [
    ./modules/system-theme.nix
    ./modules/fonts.nix
    ./modules/fonts-extra.nix    
    ./modules/niri.nix
    ./modules/niri-data.nix
    ./modules/mako.nix
    ./modules/foot.nix
    ./modules/tools.nix
   # ./modules/emacs.nix
   # ./modules/sway.nix
   # ./modules/waybar-niri.nix
   # ./modules/rofi-colors-compat.nix
   # ./modules/wallpaper.nix
   # ./modules/fish.nix
   #  ./modules/fastfetch.nix
  ];

  programs.home-manager.enable = true;  
}
