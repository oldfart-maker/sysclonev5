home.file.".local/bin/hm-update" = {
  text = ''
    #!/usr/bin/env bash
    set -Eeuo pipefail

    REPO="$HOME/projects/sysclonev5/home"
    FLAKE="$REPO#username"

    cd "$REPO"

    # Figure out the current branch (stay on whatever you're using)
    BRANCH="$(git rev-parse --abbrev-ref HEAD)"

    # Fetch latest and prune stale refs
    git fetch --all --prune

    # Ensure upstream is set to origin/<branch> (create if missing)
    if ! git rev-parse --abbrev-ref --symbolic-full-name '@{u}' >/dev/null 2>&1; then
      if git show-ref --verify --quiet "refs/remotes/origin/$BRANCH"; then
        git branch --set-upstream-to "origin/$BRANCH" "$BRANCH"
      else
        # If remote branch doesn't exist yet, create local tracking
        git push -u origin "$BRANCH"
      fi
    fi

    case "${1:-}" in
      --force)
        echo "[hm-update] HARD resetting to origin/$BRANCH"
        git reset --hard "origin/$BRANCH"
        shift || true
        ;;
      *)
        echo "[hm-update] Rebase onto origin/$BRANCH (keeps local commits)"
        git pull --rebase --autostash || {
          echo "[hm-update] Rebase failed. You can resolve and rerun, or use: hm-update --force"
          exit 1
        }
        ;;
    esac

    echo "[hm-update] building $FLAKE"
    home-manager switch --flake "$FLAKE" --refresh -v
  '';
  executable = true;
};
