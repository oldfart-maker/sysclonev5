{ config, pkgs, lib, ... }:
let
  emacsPkg   = (pkgs.emacs30-pgtk or pkgs.emacs29-pgtk or pkgs.emacs-gtk);
  repoDir    = "${config.home.homeDirectory}/projects/emacs_babel_config";
  xdgBase    = "${config.home.homeDirectory}/.config";
  emacsDir   = "${xdgBase}/emacs-prod";
  modulesDir = "${emacsDir}/modules";
in
{
  # tools available on PATH
  home.packages = [ emacsPkg pkgs.git pkgs.rsync ];

  # shared assets anchor
  xdg.configFile."emacs-common/.keep".text = "";

  # ensure target dirs exist
  home.activation.ensureEmacsDirs = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
    mkdir -p "${emacsDir}" "${modulesDir}" "${xdgBase}/emacs-common"
  '';

  # clone/pull + tangle + sync into ~/.config/emacs-prod
  home.activation.emacsBabelTangle = lib.hm.dag.entryAfter [ "ensureEmacsDirs" ] ''
    set -eu
    mkdir -p "$HOME/projects"

    if [ ! -d "${repoDir}/.git" ]; then
      ${pkgs.git}/bin/git clone https://github.com/oldfart-maker/emacs_babel_config.git "${repoDir}"
    else
      ${pkgs.git}/bin/git -C "${repoDir}" fetch --all
      ${pkgs.git}/bin/git -C "${repoDir}" pull --ff-only
    fi

    PATH="${pkgs.git}/bin:$PATH" \
    ${emacsPkg}/bin/emacs --batch -l org \
      --eval "(setq org-confirm-babel-evaluate nil)" \
      --eval "(setq org-babel-default-header-args (cons (cons :eval \"no\") (assq-delete-all :eval org-babel-default-header-args)))" \
      --eval "(add-to-list 'exec-path \"${pkgs.git}/bin\")" \
      --eval "(setenv \"PATH\" (concat \"${pkgs.git}/bin:\" (getenv \"PATH\")))" \
      --eval "(require 'ob-tangle)" \
      --eval "(org-babel-tangle-file (expand-file-name \"emacs_config.org\" \"${repoDir}\"))"

    if [ -f "${repoDir}/init.el" ]; then
      install -m0644 "${repoDir}/init.el" "${emacsDir}/init.el"
    fi
    if [ -f "${repoDir}/early-init.el" ]; then
      install -m0644 "${repoDir}/early-init.el" "${emacsDir}/early-init.el"
    fi
    if [ -d "${repoDir}/modules" ]; then
      ${pkgs.rsync}/bin/rsync -a --delete "${repoDir}/modules/" "${modulesDir}/"
    fi
  '';

  # daemon that uses ~/.config/emacs-prod as init directory
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
    Install = { WantedBy = [ "default.target" ]; };
  };
}