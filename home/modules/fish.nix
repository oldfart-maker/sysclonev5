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
      # note the `.src`
      { name = "tide"; src = pkgs.fishPlugins.tide.src; }
    ];
  
  interactiveShellInit = ''
    # Prefer fish-native Nix init if present
    if test -e ~/.nix-profile/etc/profile.d/nix.fish
      source ~/.nix-profile/etc/profile.d/nix.fish
    else if test -e /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish
      source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish
    else
      # Fallback for odd installs: ensure nix profile bin is in PATH
      if not contains -- $HOME/.nix-profile/bin $PATH
        set -gx PATH $HOME/.nix-profile/bin $PATH
      end
    end
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
