{ config, pkgs, lib, ... }:

let
  # --- Basic Niri Babel config source ---
  niriRepoUrl  = "https://github.com/oldfart-maker/niri_babel_config.git";
  niriBranch   = "main";
  niriOrgFile  = "niri_config.org";

  # --- Paths ---
  workDir = "${config.home.homeDirectory}/projects/niri_babel_config";
  srcCfg  = "${workDir}/config.kdl";
  srcKeys = "${workDir}/key_bindings.txt";

  # --- Destination ---
  niriDir = "${config.home.homeDirectory}/.config/niri";
  dstCfg  = "${niriDir}/config.kdl";
  dstKeys = "${niriDir}/key_bindings.txt";

  # --- Emacs package for tangling ---
  emacsPkg = (pkgs.emacs30-pgtk or pkgs.emacs29-pgtk or pkgs.emacs-gtk or pkgs.emacs);

in {
  xdg.enable = true;

  home.packages = with pkgs; [
    git python3 rofi foot alacritty wl-clipboard grim slurp ripgrep fd jq waybar
  ];

  # --- Activation: clone / pull / tangle / copy ---
  home.activation.niriBabel = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    set -euo pipefail

    echo "[niri] cloning/updating repo"
    mkdir -p "${niriDir}" "${workDir}"

    export PATH="${pkgs.git}/bin:${pkgs.python3}/bin:$PATH"

    if [ ! -d "${workDir}/.git" ]; then
      git clone --branch "${niriBranch}" --depth=1 "${niriRepoUrl}" "${workDir}"
    else
      git -C "${workDir}" pull --rebase --autostash --ff-only || {
        echo "[niri] git pull failed; trying fetch+reset"
        git -C "${workDir}" fetch --all -p
        git -C "${workDir}" reset --hard "origin/${niriBranch}"
      }
    fi

    export ORG_PATH="${workDir}/${niriOrgFile}"
    if [ ! -f "$ORG_PATH" ]; then
      echo "[niri] ERROR: org source not found: $ORG_PATH" >&2
      exit 1
    fi

    echo "[niri] tangling $ORG_PATH"
    "${emacsPkg}/bin/emacs" --batch -Q -l org \
      --eval '(setq inhibit-startup-message t org-confirm-babel-evaluate nil org-babel-python-command "python3")' \
      --eval '(org-babel-do-load-languages (quote org-babel-load-languages) (quote ((emacs-lisp . t) (python . t))))' \
      --eval '(require (quote ob-tangle))' \
      --eval '(let ((org (or (getenv "ORG_PATH") "")))
                 (unless (and org (file-exists-p org))
                   (error "[niri] ORG_PATH missing or not found: %s" org))
                 (with-current-buffer (find-file-noselect org)
                   (setq-local org-babel-default-header-args:kdl (list (cons :eval "no")))
                   (org-babel-execute-buffer)
                   (org-babel-tangle)))'

    echo "[niri] copying results to ~/.config/niri"
    mkdir -p "${niriDir}"
    [ -f "${srcCfg}" ] && install -m 0644 -D "${srcCfg}" "${dstCfg}" || echo "[niri] WARN: config.kdl not found"
    [ -f "${srcKeys}" ] && install -m 0644 -D "${srcKeys}" "${dstKeys}" || echo "[niri] NOTE: no key_bindings.txt"

    if ! command -v niri >/dev/null 2>&1; then
      echo "[niri] NOTE: install binary via 'sudo pacman -Syu niri'"
    fi
  '';

  # 1) Clone/update lenovo-dotfiles into a cache dir
  home.activation.dotfilesClone = lib.hm.dag.entryAfter [ "niriBabel" ] ''
    set -Eeuo pipefail

    DOT_REPO_URL="https://github.com/oldfart-maker/lenovo-dotfiles.git"
    DOT_BRANCH="main"
    DOT_CACHE="${config.home.homeDirectory}/.cache/lenovo-dotfiles"

    # Ensure we use the Nix-provided git (don’t rely on system PATH)
    export PATH="${pkgs.git}/bin:${pkgs.coreutils}/bin:${pkgs.findutils}/bin:$PATH"

    echo "[dotfiles] git=$(git --version || echo 'not found')"
    echo "[dotfiles] repo=$DOT_REPO_URL branch=$DOT_BRANCH"
    echo "[dotfiles] cache=$DOT_CACHE"

    # If cache dir exists but isn't a git repo, wipe it (failed prior clone)
    if [ -d "$DOT_CACHE" ] && [ ! -d "$DOT_CACHE/.git" ]; then
      echo "[dotfiles] cache exists but not a repo; removing and recloning"
      rm -rf "$DOT_CACHE"
    fi

    # Preflight: verify remote/branch is reachable
    if ! git ls-remote --heads "$DOT_REPO_URL" "$DOT_BRANCH" >/dev/null 2>&1; then
      echo "[dotfiles] ERROR: cannot reach $DOT_REPO_URL (branch $DOT_BRANCH)" >&2
      exit 20
    fi

    if [ -d "$DOT_CACHE/.git" ]; then
      echo "[dotfiles] updating existing cache"
      git -C "$DOT_CACHE" fetch --all -p
      git -C "$DOT_CACHE" checkout "$DOT_BRANCH" || true
      git -C "$DOT_CACHE" reset --hard "origin/$DOT_BRANCH"
    else
      echo "[dotfiles] cloning into cache"
      mkdir -p "$(dirname "$DOT_CACHE")"
      git clone --branch "$DOT_BRANCH" --depth=1 "$DOT_REPO_URL" "$DOT_CACHE"
    fi

    # Post-checks: repo OK?
    if [ ! -d "$DOT_CACHE/.git" ]; then
      echo "[dotfiles] ERROR: clone/update failed; no .git in $DOT_CACHE" >&2
      exit 21
    fi

    # Post-checks: the expected subtree exists?
    if [ ! -d "$DOT_CACHE/.config/niri" ]; then
      echo "[dotfiles] ERROR: $DOT_CACHE/.config/niri not found after clone" >&2
      echo "[dotfiles] Contents under .config:"; find "$DOT_CACHE/.config" -maxdepth 1 -mindepth 1 -type d -printf "  - %f/\n" || true
      exit 22
    fi

    echo "[dotfiles] Ready: $(find "$DOT_CACHE/.config/niri" -maxdepth 1 -mindepth 1 -type d -printf "%f " | sed "s/^/subdirs: /")"
  '';

  # 2) Rsync selected subdirs into ~/.config/niri/*
  home.activation.dotfilesSyncNiri = lib.hm.dag.entryAfter [ "dotfilesClone" ] ''
    set -euo pipefail
    SRC="${config.home.homeDirectory}/.cache/lenovo-dotfiles/.config/niri"
    DEST="${config.xdg.configHome}/niri"
    mkdir -p "$DEST"

    sync_one() {
      local name="$1"
      if [ -d "$SRC/$name" ]; then
        echo "[niri] syncing $name -> $DEST/$name"
        mkdir -p "$DEST/$name"
        rsync -a --delete "$SRC/$name/" "$DEST/$name/"
      else
        echo "[niri] (skip) $SRC/$name missing"
      fi
    }

    # Bring over only what you want right now
    sync_one scripts
    sync_one alacritty
    sync_one waybar
    sync_one rofi

    # Make scripts runnable (your keybinds call these directly)
    [ -d "$DEST/scripts" ] && chmod -R u+rx "$DEST/scripts"
  '';

}
