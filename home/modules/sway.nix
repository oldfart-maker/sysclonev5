{ pkgs, ... }:
{
  wayland.windowManager.sway = {
    enable = true;
    package = pkgs.sway;
    
  # turn off waybar
  config = {
    bars = [ ];   # <— disables Sway’s built-in bar(s)
  };
  
    extraConfig = builtins.readFile ./dotfiles/sway/config;
  };

  home.packages = with pkgs; [ wl-clipboard grim slurp swaybg wofi kanshi mako waybar ];
  services.mako.enable = true;
  programs.waybar.enable = true;
}
