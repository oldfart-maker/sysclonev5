{ config, pkgs, lib, ... }:
{
  # Enable per-user fontconfig
  fonts.fontconfig.enable = true;

  # Core text fonts you already had
  home.packages = [
    pkgs.jetbrains-mono
    pkgs.fira-code
    pkgs.cantarell-fonts
  ];

  # Help apps choose sensible defaults
  fonts.fontconfig.defaultFonts = {
    monospace = [ "JetBrains Mono" "Fira Code" ];
    sansSerif = [ "Cantarell" ];
    emoji     = [ "Noto Color Emoji" ];
  };
}
