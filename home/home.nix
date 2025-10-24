{ config, pkgs, lib, ... }:
{
  home.username = "username";
  home.homeDirectory = "/home/username";
  home.stateVersion = "24.05";

  # keep git installed via HM from now on
  home.packages = with pkgs; [ git ];

  imports = [
   # ./modules/emacs.nix
   # ./modules/fonts.nix
   # ./modules/fonts-extra.nix
   # ./modules/sway.nix
    ./modules/niri.nix
   # ./modules/system-theme.nix
   # ./modules/waybar-niri.nix
   # ./modules/hm-deploy.nix
   # ./modules/rofi-colors-compat.nix
   # ./modules/foot.nix
   # ./modules/wallpaper.nix
   # ./modules/fish.nix
   #  ./modules/fastfetch.nix
  ];

  programs.home-manager.enable = true;  
}
