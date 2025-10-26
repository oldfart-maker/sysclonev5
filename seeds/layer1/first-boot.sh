#!/usr/bin/env bash
set -Eeuo pipefail

ENV_FILE="/etc/sysclone/firstboot.env"
if [[ -f "$ENV_FILE" ]]; then
  # shellcheck disable=SC1090
  . "$ENV_FILE"
fi
set -u

: "${WIFI_SSID:=}"
: "${WIFI_PASS:=}"
: "${USERNAME:=username}"
: "${USERPASS:=username}"

log(){ printf '%s %s\n' "[L1]" "$*"; }

provision_wifi() {
  [[ -z "$WIFI_SSID" ]] && return 0
  [[ -z "$WIFI_PASS" ]] && { log "--wifi-ssid set but --wifi-pass missing"; return 1; }

  install -d -m 700 /var/lib/iwd
  cat > "/var/lib/iwd/${WIFI_SSID}.psk" <<EOF
[Security]
Passphrase=${WIFI_PASS}
EOF
  chmod 0600 "/var/lib/iwd/${WIFI_SSID}.psk"
  systemctl enable --now iwd >/dev/null 2>&1 || true
  # best effort connect
  if iw dev | awk '/Interface/ {print $2; exit}' | xargs -r -I{} iwctl --passphrase "$WIFI_PASS" station {} connect "$WIFI_SSID"; then
    log "wifi connect issued"
  else
    log "wifi connect failed (continuing)"
  fi
}

ensure_user_and_seatd() {
  # create user if missing
  if ! id -u "$USERNAME" >/dev/null 2>&1; then
    log "creating user '$USERNAME' (wheel)"
    useradd -m -G wheel -s /bin/bash "$USERNAME"
    echo "${USERNAME}:${USERPASS}" | chpasswd
    # enable wheel sudoers if commented
    sed -i -E 's/^\s*#\s*(%wheel\s+ALL=\(ALL:ALL\)\s+ALL)/\1/' /etc/sudoers
  fi

  # groups needed for Wayland/seatd
  for g in seat input video render; do
    getent group "$g" >/dev/null 2>&1 || groupadd -r "$g" || true
  done
  usermod -aG seat,input,video,render "$USERNAME" || true

  # seatd if available
  if systemctl list-unit-files --no-legend | grep -q '^seatd\.service'; then
    systemctl enable --now seatd.service || true
  fi

  log "seatd/groups ensured for $USERNAME"
}

install_dev_tools() {
  log "installing dev tools (cmake, ninja, toolchain, etc.)"

  # Quick online check (best-effort). Skip if no network.
  if ! ping -c1 -W2 1.1.1.1 >/dev/null 2>&1 && \
     ! ping -c1 -W2 archlinux.org >/dev/null 2>&1; then
    log "network not reachable; skipping dev tools install"
    return 0
  fi

  # Refresh package DB; tolerate mirror hiccups
  if ! pacman -Sy --noconfirm >/dev/null 2>&1; then
    log "pacman -Sy failed (retrying once)"
    sleep 2
    pacman -Sy --noconfirm || { log "pacman -Sy failed; continuing without dev tools"; return 0; }
  fi

  local pkgs=(
    base-devel
    cmake ninja pkgconf
    gdb clang
    python jq
  )

  if ! pacman -S --needed --noconfirm "${pkgs[@]}"; then
    log "dev tools install had errors (continuing)"
  fi
}

main() {
  log "first-boot start"
  provision_wifi || true
  ensure_user_and_seatd
  install_dev_tools
  log "first-boot done"
}
main "$@"
