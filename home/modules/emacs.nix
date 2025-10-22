{ config, pkgs, lib, ... }:

let
  repoRoot    = "${config.home.homeDirectory}/projects/sysclonev5";
  genEmacsDir = "${repoRoot}/generated/emacs";
  genEarly    = "${genEmacsDir}/early-init.el";
  genInit     = "${genEmacsDir}/init.el";
  genModules  = "${genEmacsDir}/modules";

  emacsDir    = "${config.xdg.configHome}/emacs-prod";

  emacsPkg    = (pkgs.emacs30-pgtk or pkgs.emacs29-pgtk or pkgs.emacs-gtk or pkgs.emacs);
in
{
  home.packages = [ emacsPkg ];

  # Symlink the generated files/dir from your repo
  home.file."${emacsDir}/early-init.el".source =
    config.lib.file.mkOutOfStoreSymlink genEarly;

  home.file."${emacsDir}/init.el".source =
    config.lib.file.mkOutOfStoreSymlink genInit;

  home.file."${emacsDir}/modules".source =
    config.lib.file.mkOutOfStoreSymlink genModules;

  # Friendly guard so you don’t forget to generate on the host
  home.activation.emacsGeneratedCheck = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    set -Eeuo pipefail
    missing=0
    [ -f "${genInit}" ]  || { echo "[emacs] ERROR: ${genInit} missing";  missing=1; }
    [ -d "${genModules}" ] || { echo "[emacs] ERROR: ${genModules} missing"; missing=1; }
    [ -f "${genEarly}" ] || echo "[emacs] NOTE: ${genEarly} missing (ok if you don't use it)"
    if [ $missing -ne 0 ]; then
      echo "[emacs] Run on host: tools/tangle-sync.sh --emacs && git push"
      echo "[emacs] Then on this machine: hm-update"
      exit 41
    fi
  '';

  systemd.user.services."emacs-prod" = {
    Unit = {
      Description = "Emacs daemon (emacs-prod)";
      After = [ "graphical-session.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      Type = "simple";
      ExecStart = "${emacsPkg}/bin/emacs --fg-daemon=emacs-prod --init-directory=%h/.config/emacs-prod";
      Restart = "on-failure";
    };
    Install.WantedBy = [ "default.target" ];
  };
}
