# modules/hm-deploy.nix
{ config, pkgs, lib, ... }:
let
  repoDir = "${config.home.homeDirectory}/projects/sysclonev5/home";
in
{
  home.packages = [ pkgs.git pkgs.bashInteractive ];

  home.file.".local/bin/hm-update".text = ''
    #!/usr/bin/env bash
    set -Eeuo pipefail
    REPO="${HOME}/projects/sysclonev5/home"
    FLAKE="${REPO}#username"

    cd "$REPO"
    git fetch --quiet origin
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
