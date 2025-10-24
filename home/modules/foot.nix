# home/modules/foot.nix
{ config, pkgs, ... }:

let
  c = config.lib.stylix.colors;  # Stylix palette (no fallbacks)
in
{
  # We set colors explicitly below; keep Stylix from also writing Foot colors.
  stylix.targets.foot.enable = false;

  programs.foot = {
    enable = true;
    package = pkgs.foot;

    # HM will generate ~/.config/foot/foot.ini from this attrset.
    settings = {
      main = {
        term = "foot";
        font = "JetBrains Mono:size=11";
        # If you want the bold/italic variants explicitly:
        # font-bold = "JetBrains Mono:style=Bold:size=11";
        # font-italic = "JetBrains Mono:style=Italic:size=11";
        # font-bold-italic = "JetBrains Mono:style=Bold Italic:size=11";
        line-height = "1.1";
      };

      colors = {
        background = c.base00;
        foreground = c.base05;

        regular0 = c.base00;
        regular1 = c.base08;
        regular2 = c.base0B;
        regular3 = c.base0A;
        regular4 = c.base0D;
        regular5 = c.base0E;
        regular6 = c.base0C;
        regular7 = c.base05;

        bright0  = c.base03;
        bright1  = c.base08;
        bright2  = c.base0B;
        bright3  = c.base0A;
        bright4  = c.base0D;
        bright5  = c.base0E;
        bright6  = c.base0C;
        bright7  = c.base07;
      };
    };
  };
}
