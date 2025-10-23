{ config, pkgs, lib, ... }:

let
  repoRoot = "${config.home.homeDirectory}/projects/sysclonev5";
  cfgPath  = "${repoRoot}/home/dotfiles/fastfetch/config.jsonc";
  logoPath = "${repoRoot}/home/dotfiles/fastfetch/logo";
  hasLogo  = builtins.pathExists logoPath;
in {
  home.packages = [ pkgs.fastfetch ];

  # Symlink the config from your repo working tree
  xdg.configFile."fastfetch/config.jsonc".source =
    config.lib.file.mkOutOfStoreSymlink cfgPath;

  # (Optional) if you keep a custom logo alongside the config
  xdg.configFile."fastfetch/logo" = lib.mkIf hasLogo {
    source = config.lib.file.mkOutOfStoreSymlink logoPath;
  };

  # Helpful error if you forget to export the config
  home.activation.fastfetchCheck = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    if [ ! -f "${cfgPath}" ]; then
      echo "[fastfetch] ERROR: ${cfgPath} is missing."
      echo "[fastfetch] Copy it into home/dotfiles/fastfetch/, commit, push, then run hm-update."
      exit 42
    fi
  '';
}
