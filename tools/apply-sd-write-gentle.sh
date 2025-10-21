#!/usr/bin/env bash
set -Eeuo pipefail

file="Makefile"
[[ -f "$file" ]] || { echo "ERROR: $file not found (run from repo root)"; exit 1; }

tmp="$(mktemp)"
trap 'rm -f "$tmp"' EXIT

# 1) Insert the tuning block *once*, immediately before the sd-write target.
if ! grep -q '^SD_GENTLE \?=' "$file"; then
  awk '
    BEGIN { inserted=0 }
    # When we encounter the sd-write target for the first time, inject our block before it
    /^sd-write:[[:space:]]*##/ && inserted==0 {
      print "# ---- SD write tuning (defaults to “gentle”) ---------------------------------"
      print "# Toggle all “gentle” behavior at once:"
      print "#   SD_GENTLE ?= 1  # 1 = gentle defaults ON, 0 = OFF"
      print "SD_GENTLE ?= 1"
      print ""
      print "# Base options (apply in both modes)"
      print "SD_BS    ?= 4M"
      print "SD_CONV  ?= fsync     # fsync at end; preserves your old behavior"
      print "SD_OFLAG ?=           # e.g. \"direct\" to bypass page cache (optional)"
      print ""
      print "# Priority/throughput defaults depend on SD_GENTLE"
      print "ifeq ($(strip $(SD_GENTLE)),1)"
      print "# Gentle defaults: deprioritize and limit bandwidth for smooth desktop/audio."
      print "SD_RATE_MB ?= 8                       # cap ~8 MB/s via pv"
      print "SD_IONICE  ?= sudo ionice -c3         # idle I/O"
      print "SD_NICE    ?= nice -n 19              # low CPU priority"
      print "else"
      print "# Fast mode: no throttling/priorities by default."
      print "SD_RATE_MB ?="
      print "SD_IONICE  ?="
      print "SD_NICE    ?="
      print "endif"
      print ""
      print "# Internal helper: choose pv when throttling, else cat"
      print "ifeq ($(strip $(SD_RATE_MB)),)"
      print "SD_SRC_CMD := cat"
      print "else"
      print "SD_SRC_CMD := pv -L $(SD_RATE_MB)M"
      print "endif"
      print ""
      inserted=1
    }
    { print $0 }
  ' "$file" > "$tmp" && mv "$tmp" "$file"
fi

# 2) Replace the original dd line in the sd-write recipe with the gentle pipeline.
#    Only replace if the old line is still present.
if grep -q 'sudo dd if="$(IMG_RAW)" of="$(DEVICE_EFFECTIVE)" bs=4M status=progress conv=fsync' "$file"; then
  awk '
    BEGIN { in_target=0 }
    /^sd-write:[[:space:]]*##/ { in_target=1 }
    # Match the exact old dd line and replace it with the new pipeline (plus sync)
    in_target==1 && $0 ~ /sudo dd if="\$\(IMG_RAW\)" of="\$\(DEVICE_EFFECTIVE\)" bs=4M status=progress conv=fsync/ {
      print "\t@# Read image (optionally rate-limited), then run dd as root with optional low I/O/CPU priority."
      print "\t@$(SD_SRC_CMD) \"$(IMG_RAW)\" | $(SD_IONICE) $(SD_NICE) sudo dd of=\"$(DEVICE_EFFECTIVE)\" \\"
      print "\t\tbs=$(SD_BS) status=progress conv=$(SD_CONV) $(if $(SD_OFLAG),oflag=$(SD_OFLAG),)"
      print "\t@sync"
      in_target=0
      next
    }
    { print $0 }
  ' "$file" > "$tmp" && mv "$tmp" "$file"
fi

# 3) Add the sd-write-fast convenience target if missing.
if ! grep -q '^sd-write-fast:' "$file"; then
  {
    echo ""
    echo ".PHONY: sd-write-fast"
    echo "sd-write-fast: ## Write SD at full speed (no throttling/priorities)"
    echo "	@$(MAKE) sd-write SD_GENTLE=0"
  } >> "$file"
fi

echo "Applied gentle sd-write changes to $file"
