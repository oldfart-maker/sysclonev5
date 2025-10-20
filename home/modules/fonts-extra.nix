{ config, pkgs, lib, ... }:
{
  # Add icon/emoji fonts (no old nerdfonts.override)
  home.packages = [
    pkgs.nerd-fonts.symbols-only   # provides all Nerd Font glyphs (PUA) for icons
    pkgs.noto-fonts-emoji          # color emoji fallback
    pkgs.font-awesome              # optional, some modules/themes use FA
  ];
}
