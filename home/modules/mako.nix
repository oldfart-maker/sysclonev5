# home/modules/mako.nix
{ config, pkgs, lib, ... }:
{
  services.mako = {
    enable = true;
    settings = {
      # Minimal, safe defaults; tweak as you like
      font = "JetBrains Mono 11";
      background-color = "#111111ee";
      text-color = "#ffffffff";
      border-radius = 8;
      default-timeout = 4000;
      # example extras:
      # width = 340;
      # height = 200;
      # anchor = "top-right";
      # padding = 8;
      # margin = "12,12,12,12";
    };
  };
}
