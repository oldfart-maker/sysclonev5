{ config, pkgs, lib, ... }:

let
  cfgPath  = ../dotfiles/fastfetch/config.jsonc;
  logoPath = ../dotfiles/fastfetch/logo;
in
{
  home.packages = [ pkgs.fastfetch ];

  xdg.configFile."fastfetch/config.jsonc" = {
    source = cfgPath;       # ← store-managed
    force  = true;
  };

  xdg.configFile."fastfetch/logo" = lib.mkIf (builtins.pathExists logoPath) {
    source    = logoPath;   # directory or file; OK either way
    recursive = true;
    force     = true;
  };

  home.activation.fastfetchCheck = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    if [ ! -f "${cfgPath}" ]; then
      echo "[fastfetch] ERROR: ${cfgPath} missing."
      echo "[fastfetch] Copy your config into home/dotfiles/fastfetch/ then hm-update."
      exit 42
    fi
  '';
}
