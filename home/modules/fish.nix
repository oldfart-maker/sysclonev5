# home/modules/fish.nix
{ config, pkgs, lib, ... }:

let
  userCfg = ../dotfiles/fish/config.fish;
  funcs   = ../dotfiles/fish/functions;
in
{
  programs.fish = {
    enable = true;
    # your config runs for interactive shells from conf.d
    # (rename the file if you want a different order)
  };

  # Put your config into conf.d so it’s sourced automatically
  xdg.configFile."fish/conf.d/00-user.fish" =
    lib.mkIf (builtins.pathExists userCfg) {
      source = userCfg;     # store-managed, shows up in result/home-files
      force  = true;
    };

  # Keep your functions dir if you use it
  home.file.".config/fish/functions" =
    lib.mkIf (builtins.pathExists funcs) {
      source    = funcs;
      recursive = true;
      force     = true;
    };
}
