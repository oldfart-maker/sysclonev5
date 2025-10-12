{ pkgs, ... }:
{
  wayland.windowManager.sway = {
    enable = true;
    package = pkgs.sway;
    config = {
      modifier = "Mod4";
      terminal = "alacritty";
    };
    extraConfig = ''
      # extra sway config lines
    '';
  };

  home.packages = with pkgs; [ wl-clipboard grim slurp swaybg wofi kanshi mako waybar ];
  services.mako.enable = true;
  programs.waybar.enable = true;
}
