{ config, pkgs, lib, ... }:
{
  home.username = "username";
  home.homeDirectory = "/home/username";
  # Pin HM state version for stable option semantics
  home.stateVersion = "24.05";

  programs.home-manager.enable = true;

  # small sanity packages
  home.packages = with pkgs; [ ripgrep fd ];
}
