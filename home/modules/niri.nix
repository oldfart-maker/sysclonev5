# niri.nix
{ config, pkgs, lib, ... }:

let
  # --- Emacs to run Babel ---
  emacsPkg = (pkgs.emacs30-pgtk or pkgs.emacs29-pgtk or pkgs.emacs-gtk);

  # --- Repo: sysclonev5 (HM flake) ---
  # Note: This is the repo you run `home-manager switch --flake` from.
  # Home Manager fetches it; we generally do NOT need to clone it again here.
  syscloneRemoteUrl = "https://github.com/oldfart-maker/sysclonev5.git";
  # If you ever want a local checkout for other reasons:
  syscloneWorkDir   = "${config.home.homeDirectory}/projects/sysclonev5";

  # --- Repo: niri_babel_config (Org/Babel source for Niri) ---
  # Note: This is the repo of the org source file that will be used
  # to excute the emacs tangle generating niri's config.kdl.
  niriBabelRemoteUrl = "https://github.com/oldfart-maker/niri_babel_config.git";
  niriBabelBranch    = "main";
  niriBabelWorkDir   = "${config.home.homeDirectory}/projects/niri_babel_config";
  niriBabelOrgFile   = "niri_config.org";

  # Tangled artifacts are written by Emacs into the *work dir*:
  srcCfg   = "${niriBabelWorkDir}/config.kdl";
  srcKeys  = "${niriBabelWorkDir}/key_bindings.txt";

  # Live Niri destination on this machine
  niriDir  = "${config.home.homeDirectory}/.config/niri";
  dstCfg   = "${niriDir}/config.kdl";
  dstKeys  = "${niriDir}/key_bindings.txt";
in
{
  home.packages = with pkgs; [
    niri
  ];

  systemd.user.services.niri = {
    Unit = {
      Description = "A scrollable-tiling Wayland compositor";
      BindsTo = [ "graphical-session.target" ];
      Before  = [ "graphical-session.target" "xdg-desktop-autostart.target" ];
      Wants   = [ "graphical-session-pre.target" "xdg-desktop-autostart.target" ];
      After   = [ "graphical-session-pre.target" ];
    };

    Service = {
      Slice = "session.slice";
      Type  = "notify";
      ExecStart = "${pkgs.niri}/bin/niri --session";
      # Optional niceties:
      Restart = "on-failure";
    };

    # Make it start with your user session
    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };

  systemd.user.targets."niri-shutdown" = {
    Unit = {
      Description = "Shutdown running niri session";
      DefaultDependencies = "no";
      StopWhenUnneeded = true;
      Conflicts = [ "graphical-session.target" "graphical-session-pre.target" ];
      After     = [ "graphical-session.target" "graphical-session-pre.target" ];
    };

    # No Install section: you trigger it manually when needed,
    # e.g. `systemctl --user start niri-shutdown.target`.
  };
  
  # (Optional, but nice to have)
  xdg.enable = true;

  xdg.dataFile."wayland-sessions/niri.desktop".text = ''
    [Desktop Entry]
    Name=Niri
    Comment=A scrollable-tiling Wayland compositor
    Exec=niri-session
    Type=Application
    DesktopNames=niri
  '';

  home.activation.niriBabel = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    set -euo pipefail

    mkdir -p "${niriDir}"
    mkdir -p "${niriBabelWorkDir}"

    export PATH="${pkgs.git}/bin:$PATH"

    # Ensure local checkout of niri_babel_config (clone once; then pull)
    if [ ! -d "${niriBabelWorkDir}/.git" ]; then
      echo "[niriBabel] cloning ${niriBabelRemoteUrl} -> ${niriBabelWorkDir}"
      git clone --branch ${niriBabelBranch} --depth=1 \
        "${niriBabelRemoteUrl}" "${niriBabelWorkDir}"
    else
      echo "[niriBabel] pulling latest in ${niriBabelWorkDir}"
      git -C "${niriBabelWorkDir}" pull --rebase --autostash --ff-only || {
        echo "[niriBabel] git pull failed; leaving repo as-is" >&2
      }
    fi

    # Execute (Elisp+Python) -> Tangle from niri_babel_config
    export PATH="${pkgs.git}/bin:${pkgs.python3}/bin:$PATH"

    ${emacsPkg}/bin/emacs --batch -Q -l org \
       --eval "(setq org-confirm-babel-evaluate nil)" \
       --eval "(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t) (python . t)))" \
       --eval "(require 'ob-tangle)" \
       --eval "(progn
       	           ;; Use python3 explicitly
            	   (setq org-babel-python-command \"python3\")
            	   ;; Quiet python-mode guessing noise in batch
            	   (setq python-indent-guess-indent-offset nil
                   python-indent-offset 4))" \
       --eval "(message \"[niriBabel] python3 -> %s\" (or (executable-find \"python3\") \"<not found>\"))" \
       --eval "(let* ((org (expand-file-name \"${niriBabelOrgFile}\" \"${niriBabelWorkDir}\")))
                 (unless (file-exists-p org) (error \"Org source not found: %s\" org))
                 (with-current-buffer (find-file-noselect org)
                   ;; Don’t execute KDL blocks; just tangle them.
                   (setq-local org-babel-default-header-args:kdl '((:eval . \"no\")))
                   (org-babel-execute-buffer)
                   (org-babel-tangle)))"

    # Deploy to ~/.config/niri with a timestamped backup
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
