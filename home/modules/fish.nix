{ config, pkgs, lib, ... }:

let
  cfgFile   = ../dotfiles/fish/config.fish;
  funcsDir  = ../dotfiles/fish/functions;
in
{
  programs.fish.enable = true;
  home.packages = [ pkgs.fish ];

  # config.fish → from repo (if present)
  home.file.".config/fish/config.fish" =
    lib.mkIf (builtins.pathExists cfgFile) {
      source = cfgFile;     # ← store-managed, shows in result/home-files
      force  = true;        # clobber any stray file
    };

  # functions/ → from repo (if present)
  home.file.".config/fish/functions" =
    lib.mkIf (builtins.pathExists funcsDir) {
      source    = funcsDir; # ← directory; HM will link files recursively
      recursive = true;
      force     = true;
    };

  # Loud-but-friendly check (optional)
  home.activation.fishDotfilesCheck = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    if [ ! -f "${cfgFile}" ]; then
      echo "[fish] NOTE: ${cfgFile} not found (ok if intentional)."
    fi
    if [ ! -d "${funcsDir}" ]; then
      echo "[fish] NOTE: ${funcsDir} not found (ok if intentional)."
    fi
  '';

}
