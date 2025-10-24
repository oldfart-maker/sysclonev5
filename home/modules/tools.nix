# home/modules/tools.nix
{ config, ... }:
{
  # Ensure ~/.local/bin exists and script is linked, executable
  home.file.".local/bin/hm-update" = {
    source = ../scripts/hm-update.sh;  # path relative to this .nix file
    executable = true;
  };
}
