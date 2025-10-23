#!/usr/bin/env bash
# tangle-sync.sh — host-side helper to:
#   1) pull/update babel repos (niri + emacs)
#   2) tangle via batch Emacs (no init)
#   3) copy outputs into sysclonev5/generated/
#   4) commit if changed
#
# Usage:
#   tools/tangle-sync.sh           # == --all
#   tools/tangle-sync.sh --niri
#   tools/tangle-sync.sh --emacs
#
# Env:
#   SYSCLONE_REPO  (default: $HOME/projects/sysclonev5)
#   NIRI_SRC_DIR   (default: $HOME/projects/niri_babel_config)
#   EMACS_SRC_DIR  (default: $HOME/projects/emacs_babel_config)
#   NIRI_GIT       (default: git@github.com:oldfart-maker/niri_babel_config.git)
#   EMACS_GIT      (default: git@github.com:oldfart-maker/emacs_babel_config.git)
#   EMACS_BIN      (default: emacs)
#   BRANCH         (default: main)

set -Eeuo pipefail

# --- defaults -------------------------------------------------------------
SYSCLONE_REPO="${SYSCLONE_REPO:-$HOME/projects/sysclonev5}"

NIRI_SRC_DIR="${NIRI_SRC_DIR:-$HOME/projects/niri_babel_config}"
EMACS_SRC_DIR="${EMACS_SRC_DIR:-$HOME/projects/emacs_babel_config}"

NIRI_GIT="${NIRI_GIT:-git@github.com:oldfart-maker/niri_babel_config.git}"
EMACS_GIT="${EMACS_GIT:-git@github.com:oldfart-maker/emacs_babel_config.git}"

EMACS_BIN="${EMACS_BIN:-emacs}"
BRANCH="${BRANCH:-main}"

MODE="${1:---all}"
want_niri=false
want_emacs=false
case "$MODE" in
  --niri)   want_niri=true ;;
  --emacs)  want_emacs=true ;;
  --all|"") want_niri=true; want_emacs=true ;;
  -h|--help) sed -n '1,60p' "$0"; exit 0 ;;
  *) echo "Unknown arg: $MODE (use --niri | --emacs | --all)"; exit 2 ;;
esac

# --- guards ---------------------------------------------------------------
command -v "$EMACS_BIN" >/dev/null || { echo "ERROR: need Emacs on host (EMACS_BIN='$EMACS_BIN')" >&2; exit 1; }
if $want_niri; then
  command -v python3 >/dev/null || { echo "ERROR: need python3 on host for niri tangle" >&2; exit 1; }
fi

# --- helpers --------------------------------------------------------------
log(){ printf '[%s] %s\n' "$(date +%H:%M:%S)" "$*"; }

ensure_repo () {
  local dir="$1" url="${2:-}"
  if [[ -n "$url" && ! -d "$dir/.git" ]]; then
    log "clone $url -> $dir"
    git clone "$url" "$dir"
  fi
  [[ -d "$dir/.git" ]] || { echo "ERROR: $dir is not a git repo"; exit 1; }
  git -C "$dir" fetch --prune
  git -C "$dir" checkout "$BRANCH"
  git -C "$dir" pull --ff-only
}

# Create a temp elisp file, run emacs -Q --batch -l org -l that-file
run_emacs_el () {
  local el_content="$1"
  local tmp
  tmp="$(mktemp --suffix=.el)"
  # shellcheck disable=SC2016
  printf '%s' "$el_content" > "$tmp"
  "$EMACS_BIN" -Q --batch -l org -l "$tmp"
  rm -f "$tmp"
}

tangle_niri () {
  local org="$1"
  run_emacs_el "$(cat <<'ELISP'
(setq inhibit-startup-message t
      org-confirm-babel-evaluate nil
      org-babel-python-command "python3"
      python-indent-offset 4
      python-indent-guess-indent-offset nil)
(require 'ob-tangle)
(require 'ob-python)
(require 'ob-shell)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)
   (python . t)))
(let ((org (or (getenv "ORG_PATH") "")))
  (unless (and org (file-exists-p org))
    (error "[niri] ORG_PATH missing or not found: %s" org))
  (with-current-buffer (find-file-noselect org)
    ;; don't execute kdl blocks; just tangle them
    (setq-local org-babel-default-header-args:kdl (list (cons :eval "no")))
    (org-babel-execute-buffer)
    (org-babel-tangle)))
ELISP
)"
}

tangle_emacs () {
  local org="$1"
  run_emacs_el "$(cat <<'ELISP'
(setq inhibit-startup-message t
      org-confirm-babel-evaluate nil)
(require 'ob-tangle)
(require 'ob-shell)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)))
(let ((org (or (getenv "ORG_PATH") "")))
  (unless (and org (file-exists-p org))
    (error "[emacs] ORG_PATH missing or not found: %s" org))
  (with-current-buffer (find-file-noselect org)
    (org-babel-execute-buffer)
    (org-babel-tangle)))
ELISP
)"
}

# --- ensure repos are present / up to date -------------------------------
ensure_repo "$SYSCLONE_REPO"
$want_niri  && ensure_repo "$NIRI_SRC_DIR"  "$NIRI_GIT"
$want_emacs && ensure_repo "$EMACS_SRC_DIR" "$EMACS_GIT"

mkdir -p "$SYSCLONE_REPO/home/generated/niri" \
         "$SYSCLONE_REPO/home/generated/emacs/modules"

# --- Niri ----------------------------------------------------------------
if $want_niri; then
  log "niri: tangle from $NIRI_SRC_DIR/niri_config.org"
  pushd "$NIRI_SRC_DIR" >/dev/null
  export ORG_PATH="$NIRI_SRC_DIR/niri_config.org"
  tangle_niri "$ORG_PATH"

  # copy outputs
  if [[ -f "$NIRI_SRC_DIR/config.kdl" ]]; then
    install -m 0644 "$NIRI_SRC_DIR/config.kdl" \
      "$SYSCLONE_REPO/home/generated/niri/config.kdl"
  else
    echo "ERROR: niri tangle produced no config.kdl" >&2; exit 1
  fi
  if [[ -f "$NIRI_SRC_DIR/key_bindings.txt" ]]; then
    install -m 0644 "$NIRI_SRC_DIR/key_bindings.txt" \
      "$SYSCLONE_REPO/home/generated/niri/key_bindings.txt"
  fi
  popd >/dev/null
fi

# --- Emacs ---------------------------------------------------------------
if $want_emacs; then
  log "emacs: tangle from $EMACS_SRC_DIR/emacs_config.org"
  pushd "$EMACS_SRC_DIR" >/dev/null
  export ORG_PATH="$EMACS_SRC_DIR/emacs_config.org"
  tangle_emacs "$ORG_PATH"

  # copy outputs
  [[ -f "early-init.el" ]] && install -m 0644 "early-init.el" \
      "$SYSCLONE_REPO/home/generated/emacs/early-init.el"
  [[ -f "init.el" ]] && install -m 0644 "init.el" \
      "$SYSCLONE_REPO/home/generated/emacs/init.el"
  if compgen -G "modules/*.el" >/dev/null; then
    rsync -a --delete "modules/" "$SYSCLONE_REPO/home/generated/emacs/modules/"
  fi
  popd >/dev/null
fi

# --- Commit if changed ---------------------------------------------------
log "sysclonev5: commit if home/generated/ changed"
pushd "$SYSCLONE_REPO" >/dev/null
if ! git diff --quiet -- generated; then
  git add generated
  git commit -m "tangle-sync: update generated configs"
  echo "Committed. Push with: git -C \"$SYSCLONE_REPO\" push"
else
  echo "No changes in home/generated/; nothing to commit."
fi
popd >/dev/null

log "done."
