{ config, pkgs, lib, ... }:

let
  emacsSock = "emacs-prod";
  repoUrl   = "https://github.com/oldfart-maker/emacs_babel_config.git";
  repoDir   = "${config.home.homeDirectory}/projects/emacs_babel_config";
  orgFile   = "${repoDir}/emacs_config.org";

  syncScript = pkgs.writeShellScript "emacs-babel-sync" ''
    set -eu
    mkdir -p "${config.home.homeDirectory}/projects"

    if [ -d "${repoDir}/.git" ]; then
      git -C "${repoDir}" pull --ff-only
    else
      git clone "${repoUrl}" "${repoDir}"
    fi

    # ensure daemon is up
    systemctl --user start ${emacsSock}.service || true

    # wait briefly for socket
    for i in 1 2 3; do
      if emacsclient -s ${emacsSock} -e t >/dev/null 2>&1; then break; fi
      sleep 0.5
    done

    # tangle quietly
    emacsclient -s ${emacsSock} -e \
      '(let ((org-confirm-babel-evaluate nil))
         (org-babel-tangle-file "'"${orgFile}"'"))' >/dev/null || true
  '';
in {
  home.packages = [ pkgs.git ];

  # Run at login (and available to be started manually)
  systemd.user.services."emacs-babel-sync" = {
    Unit = {
      Description = "Sync + tangle emacs_babel_config";
      After = [ "emacs-prod.service" ];
      Wants = [ "emacs-prod.service" ];
    };
    Service = {
      Type = "oneshot";
      ExecStart = "${syncScript}";
    };
    Install = { WantedBy = [ "default.target" ]; };
  };

  # Also run immediately after each HM switch
  home.activation.emacsBabelSyncNow =
    lib.hm.dag.entryAfter [ "reloadSystemd" ] ''
      systemctl --user start emacs-babel-sync.service || true
    '';
}
