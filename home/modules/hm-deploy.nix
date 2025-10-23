# modules/hm-deploy.nix
{ config, pkgs, lib, ... }:
{
  home.packages = [ pkgs.git pkgs.bashInteractive ];
  home.sessionPath = [ "${config.home.homeDirectory}/.local/bin" ];

  home.file.".local/bin/hm-update" = {
    text = ''
# ~/.local/bin/hm-update
#!/usr/bin/env bash
set -Eeuo pipefail

REPO="$HOME/projects/sysclonev5/home"
USER_ATTR="username"                 # the attribute name in your flake outputs
LOCAL_FLAKE="$REPO#$USER_ATTR"
REMOTE_FLAKE="github:oldfart-maker/sysclonev5?dir=home#$USER_ATTR"

usage() {
  cat <<EOF
Usage: hm-update [--local|--remote] [--force]
  --local   Build from local working tree (no git pull). Great while iterating.
  --remote  Build from GitHub flake (ignores local edits).
  --force   When pulling (default mode), hard reset to origin/<branch>.
No flag = pull from origin then build local flake.
EOF
}

MODE="pull"   # default: pull + build local
FORCE=0
for a in "$@"; do
  case "$a" in
    --local) MODE="local" ;;
    --remote) MODE="remote" ;;
    --force) FORCE=1 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown arg: $a"; usage; exit 2 ;;
  esac
done

echo "[hm-update] repo: $REPO"
echo "[hm-update] mode: $MODE"

if [ "$MODE" = "remote" ]; then
  echo "[hm-update] building remote flake: $REMOTE_FLAKE"
  nix run nixpkgs#home-manager -- switch --flake "$REMOTE_FLAKE" --refresh -v
  exit $?
fi

# local / pull modes need a flake at $REPO
if [ ! -f "$REPO/flake.nix" ]; then
  echo "[hm-update] ERROR: $REPO/flake.nix not found."
  echo " - Create a flake here (~/projects/sysclonev5/home/flake.nix), or use --remote."
  exit 3
fi

cd "$REPO"
BRANCH="$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo main)"

if [ "$MODE" = "pull" ]; then
  echo "[hm-update] syncing branch: $BRANCH"
  git fetch --all --prune
  if ! git rev-parse --abbrev-ref --symbolic-full-name '@{u}' >/dev/null 2>&1; then
    if git show-ref --verify --quiet "refs/remotes/origin/$BRANCH"; then
      git branch --set-upstream-to "origin/$BRANCH" "$BRANCH"
    else
      git push -u origin "$BRANCH"
    fi
  fi
  if [ $FORCE -eq 1 ]; then
    echo "[hm-update] HARD reset to origin/$BRANCH"
    git reset --hard "origin/$BRANCH"
  else
    echo "[hm-update] Rebase onto origin/$BRANCH"
    git pull --rebase --autostash || { echo "[hm-update] Rebase failed"; exit 1; }
  fi
fi

echo "[hm-update] flake inputs:"
nix flake metadata --json . >/dev/null || { echo "[hm-update] invalid flake"; exit 4; }

SHA="$(git rev-parse --short HEAD || echo '?')"
echo "[hm-update] building local flake @ $SHA → $LOCAL_FLAKE"
home-manager switch --flake "$LOCAL_FLAKE" --refresh -v

    '';
    executable = true;
  };
}
