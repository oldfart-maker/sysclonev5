# modules/hm-deploy.nix (fixed)
{ config, pkgs, lib, ... }:
let
  repoDir = "${config.home.homeDirectory}/projects/sysclonev5/home";
in
{
  home.packages = [ pkgs.git pkgs.bashInteractive ];

  # Ensure ~/.local/bin is on PATH
  home.sessionPath = [ "${config.home.homeDirectory}/.local/bin" ];

  home.file.".local/bin/hm-update".text = ''
    #!/usr/bin/env bash
    set -Eeuo pipefail
    REPO="${repoDir}"
    FLAKE="${REPO}#username"

    cd "$REPO"
    git fetch --quiet origin || true

    LOCAL=$(git rev-parse @)
    REMOTE=$(git rev-parse @{u} || echo "")
    BASE=$(git merge-base @ @{u} || echo "")

    if [[ -n "$REMOTE" && "$LOCAL" = "$REMOTE" ]]; then
      echo "[hm-update] repo up to date."
    elif [[ -n "$REMOTE" && "$LOCAL" = "$BASE" ]]; then
      echo "[hm-update] pulling changes..."
      git pull --rebase
    else
      echo "[hm-update] local ahead or diverged; not auto-pulling."
    fi

    echo "[hm-update] switching home-manager…"
    home-manager switch --flake "$FLAKE" --refresh -v
  '';
  home.file.".local/bin/hm-update".permissions = "0755";
}
