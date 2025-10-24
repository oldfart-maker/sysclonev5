# home/modules/foot.nix
{ config, pkgs, ... }:

let
  c = config.lib.stylix.colors;  # from system-theme.nix
in
{
  # Stylix foot target intentionally disabled by your system-theme.nix
  programs.foot = {
    enable = true;
    package = pkgs.foot;

    settings = {
      main = {
        term = "foot";
        font = "JetBrains Mono:size=10";
        pad = "8x8";
      };
      colors = {
        background = c.base00;  foreground = c.base05;
        regular0 = c.base00;    regular1 = c.base08;
        regular2 = c.base0B;    regular3 = c.base0A;
        regular4 = c.base0D;    regular5 = c.base0E;
        regular6 = c.base0C;    regular7 = c.base05;
        bright0  = c.base03;    bright1  = c.base08;
        bright2  = c.base0B;    bright3  = c.base0A;
        bright4  = c.base0D;    bright5  = c.base0E;
        bright6  = c.base0C;    bright7  = c.base07;
      };
    };
  };
}
