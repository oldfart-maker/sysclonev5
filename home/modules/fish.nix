{ config, pkgs, lib, ... }:

let
  repoRoot = "${config.home.homeDirectory}/projects/sysclonev5";
  df       = name: "${repoRoot}/home/dotfiles/${name}";

  hasFuncs = builtins.pathExists (df "fish/functions");
  hasConf  = builtins.pathExists (df "fish/config.fish");
in
{
  programs.fish.enable = true;
  home.packages = [ pkgs.fish ];

  # config.fish → symlink from your repo (only if it exists)
  home.file.".config/fish/config.fish" = lib.mkIf hasConf {
    source = config.lib.file.mkOutOfStoreSymlink (df "fish/config.fish");
  };

  # functions/ → symlink only if present
  home.file.".config/fish/functions" = lib.mkIf hasFuncs {
    source = config.lib.file.mkOutOfStoreSymlink (df "fish/functions");
  };

  # Helpful check so you notice if you forgot to export files
  home.activation.fishDotfilesCheck = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    miss=0
    if [ ! -f "${df "fish/config.fish"}" ]; then
      echo "[fish] NOTE: ${df "fish/config.fish"} missing (ok if intentional)"
    fi
    if [ ! -d "${df "fish/functions"}" ]; then
      echo "[fish] NOTE: ${df "fish/functions"} missing (ok if intentional)"
    fi
    exit $miss
  '';
}
