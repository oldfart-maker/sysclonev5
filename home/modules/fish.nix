# home/modules/fish.nix
{ config, pkgs, lib, ... }:

let
  confdDir = ../dotfiles/fish/conf.d;
  funcsDir = ../dotfiles/fish/functions;
in
{
  programs.fish = {
    enable = true;

    plugins = [
      { name = "tide"; src = pkgs.fishPlugins.tide; }
    ];

    # Optional: add a couple of sane defaults; keep these minimal
    interactiveShellInit = ''
      # Example: ensure nix profile is in PATH if your login shell missed it
      test -f ~/.nix-profile/etc/profile.d/nix.sh; and source ~/.nix-profile/etc/profile.d/nix.sh ^/dev/null
    '';
  };

  xdg.configFile."fish/conf.d" = lib.mkIf (builtins.pathExists confdDir) {
    source    = confdDir;
    recursive = true;
    force     = true;
  };

  home.file.".config/fish/functions" = lib.mkIf (builtins.pathExists funcsDir) {
    source    = funcsDir;
    recursive = true;
    force     = true;
  };
}
