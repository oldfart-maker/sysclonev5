{ config, pkgs, lib, ... }:
{
  home.username = "username";
  home.homeDirectory = "/home/username";
  home.stateVersion = "24.05";

  # keep git installed via HM from now on
  home.packages = with pkgs; [ git ];

  imports = [
    # Global
    ./modules/user-dirs.nix    
    ./modules/system-theme.nix
    ./modules/fonts.nix
    ./modules/fonts-extra.nix
    ./modules/foot.nix    
    ./modules/fish.nix
    ./modules/fastfetch.nix
    ./modules/mako.nix
    ./modules/tools.nix
    ./modules/wallpaper.nix
    # Niri Specific
    ./modules/niri.nix
    ./modules/niri-data.nix
    ./modules/waybar-niri.nix
    ./modules/rofi.nix
    # Other
    ./modules/emacs.nix
  ];

  programs.home-manager.enable = true;  
}
