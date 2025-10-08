{ config, pkgs, lib, ... }:
let
  emacsPkg = (pkgs.emacs30-pgtk or pkgs.emacs29-pgtk or pkgs.emacs-gtk);
  repoDir  = "${config.home.homeDirectory}/projects/emacs_babel_config";
in {
  # Emacs + git on PATH
  home.packages = [ emacsPkg pkgs.git ];

  # Your existing daemon (keep as-is if you already have it)
  systemd.user.services."emacs-prod" = {
    Unit = {
      Description = "Emacs daemon (emacs-prod)";
      After = [ "graphical-session.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      Type = "simple";
      ExecStart = "${emacsPkg}/bin/emacs --fg-daemon=emacs-prod";
      Restart = "on-failure";
    };
    Install.WantedBy = [ "default.target" ];
  };

  # Clone + tangle at HM switch time (runs *every* switch)
  home.activation.emacsBabelSync = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
    set -eu
    mkdir -p "$HOME/projects"

    if [ ! -d "${repoDir}/.git" ]; then
      ${pkgs.git}/bin/git clone https://github.com/oldfart-maker/emacs_babel_config.git "${repoDir}"
    else
      ${pkgs.git}/bin/git -C "${repoDir}" fetch --all
      ${pkgs.git}/bin/git -C "${repoDir}" pull --ff-only
    fi

    # Tangle in batch (no daemon required)
    ${emacsPkg}/bin/emacs --batch -l org \
      --eval '(progn (require (quote ob-tangle))
                     (org-babel-tangle-file
                       (expand-file-name "emacs_config.org" "'"${repoDir}"'")))'
  '';

  programs.git.enable = true;
}