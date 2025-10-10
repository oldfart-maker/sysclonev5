{ config, pkgs, lib, ... }:
{
  fonts.fontconfig.enable = true;
  home.packages = [
    pkgs.jetbrains-mono
    pkgs.fira-code
    pkgs.cantarell-fonts
  ];
}
