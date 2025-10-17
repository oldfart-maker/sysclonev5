{ config, pkgs, lib, ... }:
let
  niriDir = "${config.home.homeDirectory}/.config/niri";
in {
  # Install Niri only. No services.
  home.packages = [ pkgs.niri ];
  
  home.file.".config/niri/".directory = true;

  # '';
}
