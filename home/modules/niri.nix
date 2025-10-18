{ config, pkgs, lib, ... }:

let
  # --- Emacs to run Babel (prefer pgtk if available) ---
  emacsPkg = (pkgs.emacs30-pgtk or pkgs.emacs29-pgtk or pkgs.emacs-gtk or pkgs.emacs);

  # --- Defaults (override via home.sessionVariables if you like) ---
  repoUrlDefault = "https://github.com/oldfart-maker/niri_babel_config.git";
  branchDefault  = "main";
  orgFileDefault = "niri_config.org";

  repoUrl = config.home.sessionVariables.NIRI_REPO_URL   or repoUrlDefault;
  branch  = config.home.sessionVariables.NIRI_REPO_BRANCH or branchDefault;
  orgFile = config.home.sessionVariables.NIRI_ORG_FILE    or orgFileDefault;

  # --- Working copy lives in projects (your choice kept) ---
  workDir = "${config.home.homeDirectory}/projects/niri_babel_config";

  # --- Tangled outputs (your names kept) ---
  srcCfg  = "${workDir}/config.kdl";
  srcKeys = "${workDir}/key_bindings.txt";

  # --- Live Niri destination (your paths kept) ---
  niriDir = "${config.home.homeDirectory}/.config/niri";
  dstCfg  = "${niriDir}/config.kdl";
  dstKeys = "${niriDir}/key_bindings.txt";

in {
  xdg.enable = true;

  # HM-owned tools needed to tangle & useful deps
  home.packages = with pkgs; [
    git
    python3
    wl-clipboard grim slurp
    rofi-wayland
    foot
    alacritty
    ripgrep fd jq
  ];

  # Optional per-host overrides, also export NIRI_TARGET if your org uses it
  home.sessionVariables = {
    NIRI_REPO_URL    = repoUrl;
    NIRI_REPO_BRANCH = branch;
    NIRI_ORG_FILE    = orgFile;
    NIRI_TARGET      = config.networking.hostName or "pi";
  };

  # Single activation: clone/update -> tangle -> install
  home.activation.niriBabel = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    set -euo pipefail

    echo "[niri] repo=${repoUrl} branch=${branch} org=${orgFile}"
    mkdir -p "${niriDir}" "${workDir}"

    export PATH="${pkgs.git}/bin:${pkgs.coreutils}/bin:${pkgs.findutils}/bin:${pkgs.gnused}/bin:${pkgs.python3}/bin:$PATH"

    # Clone or update
    if [ ! -d "${workDir}/.git" ]; then
      echo "[niri] cloning ${repoUrl} -> ${workDir}"
      git clone --branch "${branch}" --depth=1 "${repoUrl}" "${workDir}"
    else
      echo "[niri] updating ${workDir}"
      if ! git -C "${workDir}" pull --rebase --autostash --ff-only; then
        echo "[niri] pull failed; attempting fetch+hard reset"
        git -C "${workDir}" fetch --all -p
        git -C "${workDir}" checkout "${branch}" || true
        git -C "${workDir}" reset --hard "origin/${branch}"
      fi
    fi

    # Sanity: org path via env var (avoid Nix interpolation)
    export ORG_PATH="${workDir}/${orgFile}"
    if [ ! -f "$ORG_PATH" ]; then
      echo "[niri] ERROR: org source not found: $ORG_PATH" >&2
      exit 10
    fi

    echo "[niri] tangling $ORG_PATH"
    "${emacsPkg}/bin/emacs" --batch -Q -l org \
      --eval '(setq inhibit-startup-message t
                   org-confirm-babel-evaluate nil
                   org-babel-python-command "python3")' \
      --eval '(org-babel-do-load-languages
               (quote org-babel-load-languages)
               (quote ((emacs-lisp . t) (python . t))))' \
      --eval '(require (quote ob-tangle))' \
      --eval '(setenv "NIRI_TARGET" (or (getenv "NIRI_TARGET") "pi"))' \
      --eval '(let ((org (or (getenv "ORG_PATH") "")))
                 (unless (and org (file-exists-p org))
                   (error "[niri] ORG_PATH missing or not found: %s" org))
                 (with-current-buffer (find-file-noselect org)
                   ;; Don’t execute KDL blocks; only tangle them.
                   (setq-local org-babel-default-header-args:kdl (list (cons :eval "no")))
                   ;; Execute other blocks that produce files as needed, then tangle.
                   (org-babel-execute-buffer)
                   (org-babel-tangle)))'

    # Deploy with timestamped backup
    if [ -f "${srcCfg}" ]; then
      if [ -f "${dstCfg}" ]; then
        cp -f "${dstCfg}" "${dstCfg}.$(date +%Y%m%d-%H%M%S).bak"
      fi
      install -m 0644 -D "${srcCfg}" "${dstCfg}"
      echo "[niri] installed config.kdl -> ${dstCfg}"
    else
      echo "[niri] WARN: ${srcCfg} not found; did tangling produce it?" >&2
    fi

    if [ -f "${srcKeys}" ]; then
      install -m 0644 -D "${srcKeys}" "${dstKeys}"
      echo "[niri] installed key_bindings.txt -> ${dstKeys}"
    else
      echo "[niri] NOTE: ${srcKeys} not found; skipping key_bindings.txt"
    fi

    if ! command -v niri >/dev/null 2>&1; then
      echo "[niri] NOTE: 'niri' binary not found on PATH. For now, install via:"
      echo "        sudo pacman -Syu niri"
    fi
  '';
}
