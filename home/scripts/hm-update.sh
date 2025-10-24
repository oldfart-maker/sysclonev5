#!/usr/bin/env bash
# ~/.local/bin/hm-update  — update local repo and switch HM safely
# Usage:
#   hm-update [--pull|--local|--remote] [--force]
#   --pull    (default) fetch/reset from origin then build local flake
#   --local   build local working tree as-is (no fetch/pull)
#   --remote  build directly from GitHub (without touching local repo)
#   --force   when pulling, hard reset to origin/<branch> and clean untracked
#
# Exit codes:
#   0 success | 2 usage | 3 repo/flake missing | 4 dirty tree (without --force)

set -Eeuo pipefail

### --- CONFIG (edit to taste) ---
repo="${HOME}/projects/sysclonev5"                    # canonical repo path
branch_default="main"
local_flake="path:${repo}/home#username"             # your HM flake entry
remote_flake="github:oldfart-maker/sysclonev5?dir=home&ref=main#username"
### --------------------------------

usage() {
  cat <<EOF
Usage: $(basename "$0") [--pull|--local|--remote] [--force]
  --pull    (default) fetch from origin and update local branch, then switch
  --local   use local working tree without pulling
  --remote  build directly from GitHub flake (ignores local repo)
  --force   with --pull: hard reset to origin/<branch> and clean untracked
EOF
}

mode="pull"
force=0
for a in "$@"; do
  case "$a" in
    --pull)   mode="pull"   ;;
    --local)  mode="local"  ;;
    --remote) mode="remote" ;;
    --force)  force=1       ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown arg: $a"; usage; exit 2 ;;
  esac
done

echo "[hm-update] mode: $mode"

# Guardrails
if [[ "${EUID:-$(id -u)}" -eq 0 ]]; then
  echo "[hm-update] Refusing to run as root." >&2
  exit 2
fi

# Ensure HM CLI exists
if ! command -v home-manager >/dev/null 2>&1; then
  echo "[hm-update] 'home-manager' not found in PATH. Install HM CLI first." >&2
  exit 2
fi

# Remote mode: build straight from GitHub
if [[ "$mode" == "remote" ]]; then
  echo "[hm-update] switching from remote flake: $remote_flake"
  home-manager switch --flake "$remote_flake" -v
  echo "[hm-update] done."
  exit 0
fi

# Local/ Pull modes require the repo
if [[ ! -f "${repo}/home/flake.nix" ]]; then
  echo "[hm-update] ERROR: ${repo}/home/flake.nix not found." >&2
  exit 3
fi

cd "$repo"
branch="$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "$branch_default")"

if [[ "$mode" == "pull" ]]; then
  echo "[hm-update] syncing branch: $branch"
  git fetch --prune --all

  # set upstream if missing
  if ! git rev-parse --abbrev-ref --symbolic-full-name '@{u}' >/dev/null 2>&1; then
    if git show-ref --verify --quiet "refs/remotes/origin/$branch"; then
      git branch --set-upstream-to "origin/$branch" "$branch"
    else
      echo "[hm-update] Remote branch origin/$branch missing; cannot set upstream." >&2
      exit 3
    fi
  fi

  if [[ $force -eq 1 ]]; then
    echo "[hm-update] HARD reset to origin/$branch + clean"
    git reset --hard "origin/$branch"
    git clean -fdx
  else
    # Bail if dirty to avoid accidental commits on the Pi
    if ! git diff --quiet || ! git diff --cached --quiet; then
      echo "[hm-update] Local changes present. Use --force or commit/reset first." >&2
      exit 4
    fi
    echo "[hm-update] Rebase onto origin/$branch"
    git pull --rebase --autostash
  fi

  git submodule update --init --recursive
fi

sha="$(git rev-parse --short HEAD || echo '?')"
echo "[hm-update] building local flake @$sha → $local_flake"

# Build first (fail early without touching your profile)
home-manager build --flake "$local_flake" -v

# Activate exactly what we just built
home-manager switch --flake "$local_flake" -v

# Nice-to-have: show current generation + a couple of key paths
gen_link="${HOME}/.local/state/nix/profiles/home-manager"
gen_path="$(readlink -f "$gen_link" || true)"
echo "[hm-update] active generation: ${gen_path:-unknown}"
echo "[hm-update] done."
