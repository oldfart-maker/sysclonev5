# niri.nix
{ config, pkgs, lib, ... }:

let
  emacsPkg   = (pkgs.emacs30-pgtk or pkgs.emacs29-pgtk or pkgs.emacs-gtk);
  repoDir    = "${config.home.homeDirectory}/projects/niri_babel_config";
  xdgBase    = "${config.home.homeDirectory}/.config";
  niriDir    = "${config.home.homeDirectory}/.config/niri";  
in
{
  # Install Niri + a few handy tools you referenced
  home.packages = with pkgs; [
    niri
  ];

  # Ensure ~/.config/niri exists before activation runs
  home.file.".config/niri/".recursive = true;

  # Run Org Babel (execute Elisp+Python) THEN tangle; skip executing KDL.
  home.activation.niriBabel = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    set -euo pipefail

    mkdir -p ${niriDir}

    # If you need extra tools in PATH during batch, add here:
    export PATH="${pkgs.git}/bin:$PATH"

    ${emacsPkg}/bin/emacs --batch -Q -l org \
      --eval "(setq org-confirm-babel-evaluate nil)" \
      --eval "(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (python . t)))" \
      --eval "(require 'ob-tangle)" \
      --eval "(let* ((file (expand-file-name \"${orgFile}\" \"${repoDir}\"))) \
                (with-current-buffer (find-file-noselect file) \
                  ;; Prevent attempts to *execute* KDL blocks; we only tangle them.
                  (setq-local org-babel-default-header-args:kdl '((:eval . \"no\"))) \
                  (org-babel-execute-buffer) \
                  (org-babel-tangle)))"
}
