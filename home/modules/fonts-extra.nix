{ config, pkgs, ... }:
{
  fonts.fontconfig.enable = true;

  # Smallest fix: just install the symbols + an emoji font.
  home.packages = [
    (pkgs.nerdfonts.override { fonts = [ "NerdFontsSymbolsOnly" ]; })
    pkgs.noto-fonts-emoji
    pkgs.font-awesome        # optional; some themes use FA
  ];
}
