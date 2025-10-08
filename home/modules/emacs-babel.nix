{ config, pkgs, lib, ... }:

let
  emacsSock = "emacs-prod";
  repoUrl   = "https://github.com/oldfart-maker/emacs_babel_config.git";
  repoDir   = "${config.home.homeDirectory}/projects/emacs_babel_config";
  orgFile   = "${repoDir}/emacs_config.org";
in {
  # need git for clone/pull
  home.packages = [ pkgs.git ];

  # After HM links files and reloads user systemd, sync + tangle
  home.activation.emacsBabelSync = lib.hm.dag.entryAfter [ "reloadSystemd" ] ''
    set -eu

    mkdir -p "${config.home.homeDirectory}/projects"

    if [ -d "${repoDir}/.git" ]; then
      git -C "${repoDir}" pull --ff-only
    else
      git clone "${repoUrl}" "${repoDir}"
    fi

    # make sure the daemon is running
    systemctl --user start ${emacsSock}.service || true

    # wait briefly until the socket answers
    for i in 1 2 3; do
      if emacsclient -s ${emacsSock} -e t >/dev/null 2>&1; then break; fi
      sleep 0.5
    done

    # tangle without prompts
    emacsclient -s ${emacsSock} -e \
      '(let ((org-confirm-babel-evaluate nil))
         (with-current-buffer (find-file-noselect "'"${orgFile}"'")
           (org-babel-tangle)))' >/dev/null || true
  '';
}