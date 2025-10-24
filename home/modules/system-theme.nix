# modules/system-theme.nix
{ config, pkgs, lib, ... }:
{
  stylix = {
    enable = true;
  # image = "/home/username/Pictures/walls/gruv-rock.jpg";    
    base16Scheme = "${pkgs.base16-schemes}/share/themes/chalk.yaml";
    polarity = "dark";

    fonts = {
      monospace = { package = pkgs.jetbrains-mono; name = "JetBrains Mono"; };
      sansSerif = { package = pkgs.dejavu_fonts;  name = "DejaVu Sans";    };
      serif     = { package = pkgs.dejavu_fonts;  name = "DejaVu Serif";   };
      emoji     = { package = pkgs.noto-fonts-emoji; name = "Noto Color Emoji"; };
    };

    cursor = {
      package = pkgs.qogir-icon-theme;
      name    = "Qogirr";
      size    = 12;
    };

    targets = {
      waybar = { enable = false; addCss = false; };
      rofi.enable = false;
      foot.enable = false;
      fish.enable = false;
    };
  };
}
