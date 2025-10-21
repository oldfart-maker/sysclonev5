# modules/waybar-niri.nix
{ config, pkgs, lib, ... }:

let
  # Stylix colors (requires stylix.enable = true)
  ch = config.lib.stylix.colors.withHashtag;

  baseDir = "${config.home.homeDirectory}/.config/niri/waybar";
  cfgPath = "${baseDir}/config";
  cssPath = "${baseDir}/style.css";
  colPath = "${baseDir}/colors.css";
in
{
  home.packages = [
    pkgs.waybar
    pkgs.pavucontrol
  ];

  # --- colors.css (GTK CSS variables via @define-color) ---
  home.file."${colPath}".text = ''
    @define-color base00 ${ch.base00};
    @define-color base01 ${ch.base01};
    @define-color base02 ${ch.base02};
    @define-color base03 ${ch.base03};
    @define-color base04 ${ch.base04};
    @define-color base05 ${ch.base05};
    @define-color base06 ${ch.base06};
    @define-color base07 ${ch.base07};
    @define-color base08 ${ch.base08};
    @define-color base09 ${ch.base09};
    @define-color base0A ${ch.base0A};
    @define-color base0B ${ch.base0B};
    @define-color base0C ${ch.base0C};
    @define-color base0D ${ch.base0D};
    @define-color base0E ${ch.base0E};
    @define-color base0F ${ch.base0F};

    @define-color bg       ${ch.base00};
    @define-color bg_alt   ${ch.base01};
    @define-color bg_alt2  ${ch.base02};
    @define-color fg       ${ch.base05};

    @define-color red      ${ch.base08};
    @define-color orange   ${ch.base09};
    @define-color yellow   ${ch.base0A};
    @define-color green    ${ch.base0B};
    @define-color teal     ${ch.base0C};
    @define-color blue     ${ch.base0D};
    @define-color mauve    ${ch.base0E};
    @define-color accent   ${ch.base0D};
  '';

  # --- style.css (imports the palette above) ---
  home.file."${cssPath}".text = ''
    @import url("colors.css");

* {
    font-family: "JetBrains Mono", "Symbols Nerd Font", Iosevka, archcraft, sans-serif;
    font-size: 10px;
}

window#waybar {
    background-color: @background;
    color: @background;
    border-bottom: 2px solid @background-alt;
    transition-property: background-color;
    transition-duration: .5s;
}

window#waybar.hidden {
    opacity: 0.5;
}

window#waybar.empty {
}

window#waybar.solo {
}

#backlight {
	background-image: linear-gradient(to right, @green , @magenta);
}

#clock {
	background-image: linear-gradient(to right, @red , @blue);
}

#workspaces {
	background-color: @background-alt;
	color: @foreground;
	border-radius: 0px 12px 0px 0px;
	padding: 4px;
}

#workspaces button {
	color: @foreground;
	margin: 0px 2px;
	padding: 2px 6px;
}

#workspaces button:hover {
	background-color: @background;
	color: @white;
	border: 0px solid @background;
	border-radius: 12px;
	padding: 2px 6px;
}

#workspaces button.visible {
	background-image: linear-gradient(to right, @blue , @green);
}

#workspaces button.focused {
	background-image: linear-gradient(to right, @red , @magenta);
}

#workspaces button.urgent {
	background-image: linear-gradient(to right, @red , @yellow);
}

#workspaces button.persistent {
	background-image: linear-gradient(to right, @yellow , @magenta);
}

#workspaces button.visible,
#workspaces button.focused,
#workspaces button.urgent,
#workspaces button.persistent {
	color: @background;
	border-radius: 12px;
	padding: 2px 6px;
}

#workspaces button.current_output {
}

#workspaces button#sway-workspace-1 {
}

.modules-left > widget:first-child > #workspaces {
    margin-left: 0;
}

.modules-right > widget:last-child > #workspaces {
    margin-right: 0;
}

#mode {
	background-color: @magenta;
	font-weight: bold;
}

#window {
	background-color: @background-alt;
	color: @white;
}
window#waybar.empty #window {
	background-color: transparent;
}

#tray {
	background-color: @background-alt;
	border-radius: 12px 0px 0px 0px;
	padding: 4px 6px;
}

#tray > .passive {
    -gtk-icon-effect: dim;
}

#tray > .needs-attention {
    -gtk-icon-effect: highlight;
}

#tray > .active {
}

@keyframes gradient {
	0% {
		background-position: 0% 50%;
	}
	50% {
		background-position: 100% 50%;
	}
	100% {
		background-position: 0% 50%;
	}
}

#mpd {
	background-color: @background-alt;
	color: @white;
}

#mpd.disconnected {
	background-color: @red;
	color: @white;
}

#mpd.stopped {
	background-image: linear-gradient(to right, @red , @yellow);
	color: @background;
}

#mpd.playing {
    background: linear-gradient(90deg, @blue 25%, @cyan 50%, @magenta 100%); 
    background-size: 300% 300%;
    animation: gradient 10s ease infinite;
	color: @background;
}

#mpd.paused {
}

#idle_inhibitor {
	background-image: linear-gradient(to right, @magenta, @yellow);
}
#idle_inhibitor.deactivated {
	background-image: linear-gradient(to right, @red , @yellow);
}

#pulseaudio {
	background-image: linear-gradient(to right, @blue , @green);
}

#pulseaudio.bluetooth {
	background-image: linear-gradient(to right, @green , @yellow);
}
#pulseaudio.muted {
	background-image: linear-gradient(to right, @red , @yellow);
}

#network {
	background-image: linear-gradient(to right, @magenta , @cyan);
}

#network.disconnected {
	background-image: linear-gradient(to right, @red , @yellow);
}

#network.disabled {
	background-image: linear-gradient(to right, @red , @red);
	color: @white;
}

#network.linked {
}

#network.ethernet {
}

#network.wifi {
}

#bluetooth {
	background-image: linear-gradient(to right, @yellow , @blue);
}

#bluetooth.disabled{
	background-image: linear-gradient(to right, @red , @red);
	color: @white;
}

#bluetooth.off{
	background-image: linear-gradient(to right, @red , @yellow);
}

#bluetooth.on{
}

#bluetooth.connected{
}

#bluetooth.discoverable{
}

#bluetooth.discovering{
}

#bluetooth.pairable{
}

/** Common style **/
#backlight, 
#battery,
#clock,
#idle_inhibitor,
#mode,
#window,
#mpd,
#pulseaudio,
#network,
#bluetooth
'';
 }

  # --- Waybar JSON config ---
  home.file."${cfgPath}".text = builtins.toJSON {

    "backlight": {
		"interval": 2,
		"align": 0,
		"rotate": 0,
        "format": "{icon} {percent}%",
        "format-icons": ["", "", "", ""],
        "on-click": "",
        "on-click-middle": "",
        "on-click-right": "",
        "on-update": "",
        "on-scroll-up": "light -A 5%",
        "on-scroll-down": "light -U 5%",
        "smooth-scrolling-threshold": 1,
    },

    "bluetooth": {
        "format": " {status}",
        "format-on": " {status}",
        "format-off": " {status}",
        "format-disabled": " {status}",
        "format-connected": " {device_alias}",
        "format-connected-battery": " {device_alias}, {device_battery_percentage}%",
        "tooltip": true,
        "tooltip-format": "{controller_alias}\t{controller_address}",
        "tooltip-format-connected": "{controller_alias}\t{controller_address}\n\n{device_enumerate}",
        "tooltip-format-enumerate-connected": "{device_alias}\t{device_address}",
    },

  "clock": {
	"interval": 60,
	"align": 0,
	"rotate": 0,
        "tooltip-format": "<big>{:%B %Y}</big>\n<tt><small>{calendar}</small></tt>",
        "format": " {:%I:%M %p}",
        "format-alt": " {:%a %b %d, %G}"
    },

# waybar-cpu

# waybar-disk

    "idle_inhibitor": {
         "format": "{icon}",
         "format-icons": {
             "activated": "",
             "deactivated": ""
         },
         "timeout": 30
    },

# waybar-inhibitor
# waybar-keyboard-state
# waybar-memory

    "mpd": {
        "interval": 2,
        "unknown-tag": "N/A",

        "format": "{artist} - {title} {stateIcon}",
        "format-disconnected": " Disconnected",
        "format-paused": "{artist} - {title} {stateIcon}",
        "format-stopped": "Stopped ",
        "state-icons": {
            "paused": "",
            "playing": ""
        },
        "tooltip-format": "MPD (connected)",
        "tooltip-format-disconnected": "MPD (disconnected)",
        "on-click": "mpc toggle",
        "on-click-right": "mpc next",
        "on-update": "",
        "on-scroll-up": "volume --inc",
        "on-scroll-down": "volume --dec",
        "smooth-scrolling-threshold": 1,
    },

    "network": {
		"interval": 5,
        //"interface": "wlan*", // (Optional) To force the use of this interface, set it for netspeed to work
        "format-wifi": " {essid}",
        "format-ethernet": " {ipaddr}/{cidr}",
        "format-linked": " {ifname} (No IP)",
        "format-disconnected": "睊 Disconnected",
        "format-disabled": "睊 Disabled",
        "format-alt": " {bandwidthUpBits} |  {bandwidthDownBits}",
        "tooltip-format": " {ifname} via {gwaddr}",
    },

    "pulseaudio": {
        //"format": "{volume}% {icon} {format_source}",
        "format": "{icon} {volume}% {format_source}",
        "format-muted": " Mute",
        "format-bluetooth": " {volume}% {format_source}",
        "format-bluetooth-muted": " Mute",
        "format-source": " {volume}%",
        "format-source-muted": " Mute",
        "format-icons": {
            "headphone": "",
            "hands-free": "",
            "headset": "",
            "phone": "",
            "portable": "",
            "car": "",
            "default": ["", "", ""]
        },
        "scroll-step": 5.0,
        "on-click": "pulsemixer --toggle-mute",
        "on-click-right": "pulsemixer --toggle-mute",
        "smooth-scrolling-threshold": 1,
    },

# waybar-sndio
# waybar-states

 waybar-niri-window
    "niri/window": {
    	"format": "{}",
    },

"niri/workspaces": {
	"format": "{value}",
},

# waybar-temperature

    "tray": {
        "icon-size": 16,
        "spacing": 10
    },

# waybar-wlr-taskbar
# waybar-wlr-workspaces
'';
 }
