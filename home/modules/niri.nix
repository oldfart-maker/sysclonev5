{ config, pkgs, lib, ... }:
let
  niriDir = "${config.home.homeDirectory}/.config/niri";
in {
  
  home.packages = [ pkgs.niri ];
  
  # '';
}
