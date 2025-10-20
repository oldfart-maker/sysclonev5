{ config, pkgs, lib, ... }:
{
  fonts.fontconfig.enable = true;

  home.packages = [
    pkgs.jetbrains-mono
    pkgs.fira-code
    pkgs.cantarell-fonts

    # new way: separate nerd-fonts namespace
    pkgs.nerd-fonts.symbols-only   # provides all Waybar icon glyphs
    pkgs.noto-fonts-emoji
  ];
}
