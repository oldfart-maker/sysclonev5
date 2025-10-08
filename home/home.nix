{ config, pkgs, lib, ... }:

{
  # who/where
  home.username = "username";
  home.homeDirectory = "/home/username";
  home.stateVersion = "24.05";

  programs.home-manager.enable = true;

  # pull in your Emacs module
  imports = [
    ./modules/emacs.nix
  ];
}
