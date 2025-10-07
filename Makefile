# sysclonev5
SHELL := /bin/bash
.ONESHELL:
.DELETE_ON_ERROR:
.SUFFIXES:

-include .sysclone.env

# -------- Config --------
IMG_URL    ?= https://github.com/manjaro-arm/rpi4-images/releases/download/20250915/Manjaro-ARM-minimal-rpi4-20250915.img.xz
CACHE_DIR  ?= cache

# ----- Partition labels (distro defaults; override only if image changes) -----
BOOT_LABEL ?= BOOT_MNJRO
ROOT_LABEL ?= ROOT_MNJRO
export BOOT_LABEL
export ROOT_LABEL

# ----- Wi-Fi (edit once here; inherited by seeding scripts) -----
WIFI_SSID  ?=Hangout
WIFI_PASS  ?=gulfshores
export WIFI_SSID
export WIFI_PASS

# -------- Mount config (sysclonev4 helpers) --------
ROOT_MNT ?= /mnt/sysclone-root
BOOT_MNT ?= /mnt/sysclone-boot

export ROOT_MNT BOOT_MNT DEVICE

# Optional: convenience to "remember" a device once
.PHONY: set-device
set-device: ## Set disk for sd-write: make set-device DEVICE=/dev/sdX
	@set -euo pipefail; \
	: "${DEVICE:?Usage: make set-device DEVICE=/dev/sdX}"; \
	[ -b "$$DEVICE" ] || { echo "[set-device] ERROR: $$DEVICE is not a block device"; exit 1; }; \
	tmp=.sysclone.env.__new__; \
	{ grep -v '^DEVICE=' .sysclone.env 2>/dev/null || true; echo "DEVICE=$$DEVICE"; } > $$tmp; \
	mv $$tmp .sysclone.env; \
	echo "[set-device] DEVICE=$$DEVICE"

.PHONY: set-root
set-root:   ## Set root partition for expand: make set-root ROOT=/dev/sdX2
	@set -euo pipefail; \
	: "${ROOT:?Usage: make set-root ROOT=/dev/sdX2}"; \
	[ -b "$$ROOT" ] || { echo "[set-root] ERROR: $$ROOT is not a block device"; exit 1; }; \
	tmp=.sysclone.env.__new__; \
	{ grep -v '^ROOT=' .sysclone.env 2>/dev/null || true; echo "ROOT=$$ROOT"; } > $$tmp; \
	mv $$tmp .sysclone.env; \
	echo "[set-root] ROOT=$$ROOT"


IMG_XZ  := $(CACHE_DIR)/$(notdir $(IMG_URL))
IMG_RAW := $(IMG_XZ:.xz=)

# Resolve DEVICE from cache if empty (works even if exported empty)
DEVICE_EFFECTIVE := $(or $(strip $(DEVICE)),$(shell test -f .cache/sysclonev4/last-device && cat .cache/sysclonev4/last-device))

BOOT_MOUNT ?= /run/media/$(USER)/BOOT
CONFIRM    ?=

# -------- Help --------
# List any "target: ## description" (auto-discovered)
help:  ## Show available targets + param hints (use: make help TARGET=name)
	@python3 tools/mkhelp.py $(firstword $(MAKEFILE_LIST)) "$(TARGET)"
# -------- Introspection --------
show-config:  ## Show important variables
	@echo "IMG_URL    = $(IMG_URL)"
	@echo "CACHE_DIR  = $(CACHE_DIR)"
	@echo "IMG_XZ     = $(IMG_XZ)"
	@echo "IMG_RAW    = $(IMG_RAW)"
	@echo "DEVICE     = $(DEVICE_EFFECTIVE)"
	@echo "SD_ROOT    = $(ROOT)"
	@echo "BOOT_MOUNT = $(BOOT_MOUNT)"
	@echo "BOOT_LABEL = $(BOOT_LABEL)"
	@echo "ROOT_LABEL = $(ROOT_LABEL)"
	@echo "WIFI_SSID  = $(WIFI_SSID)"
	@echo "WIFI_PASS  = $(if $(strip $(WIFI_PASS)),(set),(unset))"
# -------- Image workflow --------
img-download:  ## Download the image (.xz) into cache/
	@mkdir -p $(CACHE_DIR)
	@if [ ! -f "$(IMG_XZ)" ]; then \
	  echo "[dl] $(IMG_URL) -> $(IMG_XZ)"; \
	  curl -fL --progress-bar "$(IMG_URL)" -o "$(IMG_XZ)"; \
	else echo "[dl] cached: $(IMG_XZ)"; fi

img-unpack: img-download  ## Decompress .xz into a raw .img (once)
	@if [ ! -f "$(IMG_RAW)" ]; then \
	  echo "[xz] unpack -> $(IMG_RAW)"; \
	  xz -dkc "$(IMG_XZ)" > "$(IMG_RAW)"; \
	else echo "[xz] cached: $(IMG_RAW)"; fi

sd-write:  ## Write raw image to SD (DESTRUCTIVE) — pass DEVICE=/dev/sdX CONFIRM=yes
	@[ -b "$(DEVICE_EFFECTIVE)" ] || { echo "[sd-write] ERROR: not a block device: $(DEVICE_EFFECTIVE)"; exit 1; }
	@[ "$(CONFIRM)" = "yes" ] || { echo "Refusing: set CONFIRM=yes"; exit 2; }
	@sudo dd if="$(IMG_RAW)" of="$(DEVICE_EFFECTIVE)" bs=4M status=progress conv=fsync
	@mkdir -p .cache/sysclonev4 && echo "$(DEVICE_EFFECTIVE)" > .cache/sysclonev4/last-device


flash-all: img-unpack sd-write  ## img-download + img-unpack + sd-write

# Convenience: unpack + write + offline expand (re-uses existing targets/vars)
flash-all+expand: img-unpack sd-write img-expand-rootfs-offline  ## unpack, write, expand (offline)


# -------- Versioning --------
tag:  ## Create annotated git tag: make tag VERSION=vX.Y.Z
	@[ -n "$(VERSION)" ] || { echo "Set VERSION=vX.Y.Z"; exit 2; }
	@git tag -a "$(VERSION)" -m "$(VERSION)"

version-bump:  ## Write VERSION file + commit: make version-bump VERSION=vX.Y.Z
	@[ -n "$(VERSION)" ] || { echo "Set VERSION=vX.Y.Z"; exit 2; }
	@echo "$(VERSION)" > VERSION
	@git add VERSION
	@git commit -m "$(VERSION): bump VERSION file"

.PHONY: help show-config img-download img-unpack sd-write flash-all \
        seed-layer1 seed-first-boot-service seed-disable-firstboot \
        tag version-bump

tidy:  ## Remove local backup files (.bak.*) from tools/ and seeds/
	@rm -f tools/*.bak* 2>/dev/null || true
	@find seeds -type f -name '*.bak.*' -delete 2>/dev/null || true
	@echo "[tidy] done"
.PHONY: tidy


# -------------------- Layer 1 (first boot) --------------------
.PHONY: seed-layer1-disable-first-boot seed-layer1-first-boot-service

seed-layer1-disable-first-boot: ensure-mounted ## Layer1: disable any OEM first-boot unit on target
	@echo "[layer1] disable-firstboot"
	sudo env ROOT_MNT="$(ROOT_MNT)" bash tools/seed-disable-firstboot.sh

seed-layer1-first-boot-service: ensure-mounted ## Layer1: install/enable our first-boot service on target
	@echo "[layer1] seed-first-boot-service"
	sudo env ROOT_MNT="$(ROOT_MNT)" sudo env ROOT_MNT="$(ROOT_MNT)" WIFI_SSID="$(WIFI_SSID)" WIFI_PASS="$(WIFI_PASS)" USERNAME="$(USERNAME)" USERPASS="$(USERPASS)" bash tools/seed-first-boot-service.sh

.PHONY: clear-layer1-stamps clear-layer2-stamps clear-all-stamps

## Clear Layer 1 first-boot stamps in $(ROOT_MNT)
clear-layer1-stamps: ## Clear Layer 1 first-boot stamps in $(ROOT_MNT)
	@echo "[clear-layer1-stamps] at $(ROOT_MNT)/var/lib/sysclone"
	@sudo rm -f "$(ROOT_MNT)/var/lib/sysclone/first-boot.done" \
	            "$(ROOT_MNT)/var/lib/sysclone/manjaro-firstboot-disabled" 2>/dev/null || true
	@echo "[clear-layer1-stamps] done"

# ---- Check the status of layer stamps ---
.PHONY: check-stamps show-stamps

show-stamps: ensure-mounted ## List all stamp files under ROOT_MNT/var/lib/sysclone
	@DIR="$(ROOT_MNT)/var/lib/sysclone"; \
	if [ -d "$$DIR" ]; then \
	  echo "[stamps] listing $$DIR"; \
	  sudo ls -la "$$DIR"; \
	else \
	  echo "[stamps] directory missing: $$DIR"; \
	fi

seed-layer1-all: ensure-mounted ## Layer1: disable first-boot + install first-boot service; leaves card unmounted
	@set -euo pipefail; \
	  $(MAKE) clear-layer1-stamps; \
	  $(MAKE) seed-layer1-disable-first-boot; \
	  $(MAKE) seed-layer1-expand-rootfs; \
	  $(MAKE) seed-layer1-net-bootstrap; \
	  $(MAKE) seed-layer1-first-boot-service; \
	  $(MAKE) ensure-unmounted; \
	  echo "[layer1] aggregate done"

.PHONY: seed-layer1-all

# Layer1: stage rootfs expansion for first boot (uses helper if present)
seed-layer1-expand-rootfs: ensure-mounted ## Layer1: stage rootfs grow on first boot
	@set -euo pipefail; \
	  if [ -x tools/seed-expand-rootfs.sh ]; then \
	    echo "[layer1] expand-rootfs via tools/seed-expand-rootfs.sh"; \
	    sudo env ROOT_MNT="$(ROOT_MNT)" bash tools/seed-expand-rootfs.sh; \
	  else \
	    echo "[layer1] WARN: tools/seed-expand-rootfs.sh not found; skipping expansion staging"; \
	  fi

.PHONY: seed-layer1-expand-rootfs

# Show boot/service progress on console + write logs to /boot/sysclone-status/
seed-boot-visibility: ensure-mounted ## Add console output & BOOT logs for first-boot/L2/L2.5
	@set -euo pipefail; \
	  sudo env ROOT_MNT="$(ROOT_MNT)" BOOT_MNT="$(BOOT_MNT)" bash tools/seed-boot-visibility.sh; \
	  $(MAKE) ensure-unmounted; \
	  echo "[boot-visibility] done"
.PHONY: seed-boot-visibility


# --- sysclone host-side rootfs expansion (offline) --------------------------
.PHONY: img-expand-rootfs-offline verify-rootfs-size sd-write+expand

verify-rootfs-size: ensure-mounted ## Quick check (mounted): shows sizes for sanity
	@echo "[make] verify sizes on mounted card"
	@lsblk -e7 -o NAME,SIZE,TYPE,MOUNTPOINTS | sed -n "1,200p"
	@df -h | sed -n "1,200p"
	@echo "[make] .rootfs-expanded stamp:" && ls -l $(ROOT_MNT)/var/lib/sysclone/.rootfs-expanded || true
# ---------------------------------------------------------------------------

# Layer1: bootstrap clock/certs/keyrings/mirrors on first boot (pre-firstboot)
seed-layer1-network-bootstrap: ensure-mounted ## Layer1: stage network/certs bootstrap service
	@echo "[layer1] seed-network-bootstrap"
	sudo env ROOT_MNT="$(ROOT_MNT)" bash tools/seed-layer1-network-bootstrap.sh

.PHONY: seed-layer1-network-bootstrap

# Layer1: stage network/clock/mirrors bootstrap (runs once on target)
seed-layer1-net-bootstrap: ensure-mounted  ## Layer1: seed net/clock/certs bootstrap (on-target)
	@echo "[layer1] net-bootstrap via tools/seed-net-bootstrap.sh"
	sudo env ROOT_MNT="$(ROOT_MNT)" bash tools/seed-net-bootstrap.sh

## Stable mount/unmount by LABEL (no /dev/sdX guessing)
.PHONY: ensure-mounted ensure-unmounted resolve-disk

ensure-mounted: ## mount device via auto-find device
	@echo "[make] mounting $(ROOT_LABEL) -> $(ROOT_MNT) and $(BOOT_LABEL) -> $(BOOT_MNT)"
	@BOOT_LABEL="$(BOOT_LABEL)" ROOT_LABEL="$(ROOT_LABEL)" \
	  BOOT_MOUNT="$(BOOT_MNT)" ROOT_MOUNT="$(ROOT_MNT)" \
	  SUDO="$(SUDO)" bash tools/devices.sh ensure-mounted

ensure-unmounted: ## unmount device via auto-find device
	@echo "[make] unmounting $(ROOT_MNT) and $(BOOT_MNT) (by label)"
	@BOOT_LABEL="$(BOOT_LABEL)" ROOT_LABEL="$(ROOT_LABEL)" \
	  BOOT_MOUNT="$(BOOT_MNT)" ROOT_MOUNT="$(ROOT_MNT)" \
	  SUDO="$(SUDO)" bash tools/devices.sh ensure-unmounted

resolve-disk: # Optional: print the parent disk (e.g. /dev/sdc) resolved from labels/mounts
	@BOOT_LABEL="$(BOOT_LABEL)" ROOT_LABEL="$(ROOT_LABEL)" \
	  BOOT_MOUNT="$(BOOT_MNT)" ROOT_MOUNT="$(ROOT_MNT)" \
	  SUDO="$(SUDO)" bash tools/devices.sh resolve-disk


# --- manual-only expand: pass DEVICE=/dev/sdX (or /dev/mmcblk0, /dev/nvme0n1) ---
.PHONY: img-expand-rootfs-offline 
img-expand-rootfs-offline: ## manual expand: requires DEVICE=/dev/sdX
	@dev="$${ROOT:-$(DEVICE_EFFECTIVE)}"; \
	[ -n "$$dev" ] || { echo "[expand] ERROR: set ROOT=/dev/sdX2 or DEVICE=/dev/sdX"; exit 1; }; \
	echo "[make] offline expand on $$dev"; \
	sudo env DEVICE="$$dev" bash tools/expand-rootfs-manual.sh
