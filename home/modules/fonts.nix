# home/modules/fonts.nix
{ config, pkgs, ... }:
{
  # Per-user fontconfig (needed for defaults & fallback resolution)
  fonts.fontconfig.enable = true;

  # Install the families you always want available
  home.packages = with pkgs; [
    jetbrains-mono
    fira-code
    cantarell-fonts
    noto-fonts-emoji
  ];

  # Set global defaults (independent of Stylix)
  fonts.fontconfig.defaultFonts = {
    monospace = [ "JetBrains Mono" "Fira Code" ];
    sansSerif = [ "Cantarell"     ];
    serif     = [ "DejaVu Serif"  ];
    emoji     = [ "Noto Color Emoji" ];
  };
}
