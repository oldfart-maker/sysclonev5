# modules/hm-deploy.nix
{ config, pkgs, lib, ... }:
{
  home.packages = [ pkgs.git pkgs.bashInteractive ];

  # ensure ~/.local/bin is on PATH for new login shells
  home.sessionPath = [ "${config.home.homeDirectory}/.local/bin" ];

  home.file.".local/bin/hm-update" = {
    text = ''
      #!/usr/bin/env bash
      set -Eeuo pipefail

      REPO="$HOME/projects/sysclonev5/home"
      FLAKE="$REPO#username"

      cd "$REPO"

      # Use current branch; keep it in sync with its upstream
      BRANCH="$(git rev-parse --abbrev-ref HEAD)"
      git fetch --all --prune

      if ! git rev-parse --abbrev-ref --symbolic-full-name '@{u}' >/dev/null 2>&1; then
        if git show-ref --verify --quiet "refs/remotes/origin/$BRANCH"; then
          git branch --set-upstream-to "origin/$BRANCH" "$BRANCH"
        else
          git push -u origin "$BRANCH"
        fi
      fi

      # Handle optional flag safely under `set -u`
      arg="${1-}"
      case "$arg" in
        --force)
          echo "[hm-update] HARD reset to origin/$BRANCH"
          git reset --hard "origin/$BRANCH"
          ;;
        ""|*)
          echo "[hm-update] Rebase onto origin/$BRANCH"
          git pull --rebase --autostash || {
            echo "[hm-update] Rebase failed. Resolve or run: hm-update --force"
            exit 1
          }
          ;;
      esac

      echo "[hm-update] building $FLAKE"
      home-manager switch --flake "$FLAKE" --refresh -v
    '';
    executable = true;
  };
}
