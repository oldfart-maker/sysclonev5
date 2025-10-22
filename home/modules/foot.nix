{ config, pkgs, lib, ... }:

let
  c = config.lib.stylix.colors;
in
{
  xdg.enable = true;

  # writes ~/.config/niri/foot/foot.ini, overwriting any existing file
  xdg.configFile."niri/foot/foot.ini" = {
    text = ''
      [main]
      font=JetBrains Mono:size=11
      font-bold=JetBrains Mono:style=Bold:size=11
      font-italic=JetBrains Mono:style=Italic:size=11
      font-bold-italic=JetBrains Mono:style=Bold Italic:size=11
      line-height=1.1

      [colors]
      background=${c.base00}
      foreground=${c.base05}
      regular0=${c.base00}
      regular1=${c.base08}
      regular2=${c.base0B}
      regular3=${c.base0A}
      regular4=${c.base0D}
      regular5=${c.base0E}
      regular6=${c.base0C}
      regular7=${c.base05}
      bright0=${c.base03}
      bright1=${c.base08}
      bright2=${c.base0B}
      bright3=${c.base0A}
      bright4=${c.base0D}
      bright5=${c.base0E}
      bright6=${c.base0C}
      bright7=${c.base07}
    '';
    force = true;
  };
}
