# niri.nix
{ config, pkgs, lib, ... }:

let
  # Choose your Emacs build
  emacsPkg = (pkgs.emacs30-pgtk or pkgs.emacs29-pgtk or pkgs.emacs-gtk);

  # Your Org repo and file
  repoDir  = "${config.home.homeDirectory}/projects/niri_babel_config";
  orgFile  = "niri_config.org";

  # Where the Org file tangles to (source artifacts)
  srcCfg   = "${repoDir}/config.kdl";
  srcKeys  = "${repoDir}/key_bindings.txt";

  # Live Niri dir (target on the Pi)
  niriDir  = "${config.home.homeDirectory}/.config/niri";
  dstCfg   = "${niriDir}/config.kdl";
  dstKeys  = "${niriDir}/key_bindings.txt";
in
{
  home.packages = with pkgs; [
    niri
  ];

  # Ensure target directory exists
  home.file.".config/niri/".recursive = true;

  # Execute (Elisp+Python) -> Tangle -> Copy to ~/.config/niri -> Reload
  home.activation.niriBabel = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    set -euo pipefail

    mkdir -p ${niriDir}

    # Make sure git is on PATH if your Org references it
    export PATH="${pkgs.git}/bin:$PATH"

    # 1) Execute + Tangle the Org source
    ${emacsPkg}/bin/emacs --batch -Q -l org \
      --eval "(setq org-confirm-babel-evaluate nil)" \
      --eval "(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (python . t)))" \
      --eval "(require 'ob-tangle)" \
      --eval "(let* ((file (expand-file-name \"${orgFile}\" \"${repoDir}\"))) \
                (with-current-buffer (find-file-noselect file) \
                  ;; Do not try to *execute* KDL blocks (we only tangle them)
                  (setq-local org-babel-default-header-args:kdl '((:eval . \"no\"))) \
                  (org-babel-execute-buffer) \
                  (org-babel-tangle)))"

    # 2) Copy into ~/.config/niri (back up existing config.kdl first)
    if [ -f ${srcCfg} ]; then
      if [ -f ${dstCfg} ]; then
        cp -f "${dstCfg}" "${dstCfg}.$(date +%Y%m%d-%H%M%S).bak"
      fi
      install -m 0644 -D ${srcCfg} ${dstCfg}
    else
      echo "WARN: ${srcCfg} not found; did tangling produce it?"
    fi

    if [ -f ${srcKeys} ]; then
      install -m 0644 -D ${srcKeys} ${dstKeys}
    else
      echo "NOTE: ${srcKeys} not found (skipping key_bindings.txt)."
    fi
  '';
}
