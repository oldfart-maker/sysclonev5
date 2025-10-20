{ config, pkgs, lib, ... }:
{
  stylix.enable = true;

  # choose ONE source of colors:
  # stylix.image = "/home/username/Pictures/walls/gruv-rock.jpg";

  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-mocha.yaml";

  stylix.polarity = "dark";

  stylix.fonts = {
    monospace = {
      package = pkgs.jetbrains-mono;   # the package that provides the font
      name    = "JetBrains Mono";      # the font family name apps will use
    };
    sansSerif = {
      package = pkgs.dejavu_fonts;
      name    = "DejaVu Sans";
    };
    serif = {
      package = pkgs.dejavu_fonts;
      name    = "DejaVu Serif";
    };
    emoji = {
      package = pkgs.noto-fonts-emoji;
      name    = "Noto Color Emoji";
    };

    # (optional) size hints Stylix propagates to supported targets
    sizes = {
      applications = 10;
      terminal     = 11;
      desktop      = 10;
      popups       = 10;
    };
  };

  stylix.cursor = {
    package = pkgs.qogir-icon-theme;
    name    = "Qogirr";
    size    = 12;
  };

  # Keep Stylix off the targets you manage yourself
  stylix.targets = {
    waybar = { enable = false; addCss = false; };
    rofi.enable = false;
    foot.enable = false;
    # Example of opting IN when desired:
    # alacritty.enable = true;
  };

  # If you set GTK/icon theme names elsewhere, make sure the packages exist.
  home.packages = with pkgs; [
    jetbrains-mono
    dejavu_fonts
    noto-fonts-emoji
    qogir-icon-theme
    # your GTK + icon themes if you set gtk.theme/iconTheme
  ];
}
