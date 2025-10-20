{ config, pkgs, lib, ... }:

let
  # Stylix color helpers (provided when stylix.enable = true)
  c  = config.lib.stylix.colors;
  ch = config.lib.stylix.colors.withHashtag;

  baseDir = "${config.home.homeDirectory}/.config/niri/waybar";
  cfgPath = "${baseDir}/config";
  cssPath = "${baseDir}/style.css";
  colPath = "${baseDir}/colors.css";
in
{
  # Install waybar (and optional helpers)
  home.packages = [
    pkgs.waybar
    pkgs.pavucontrol
  ];

  # Generate GTK CSS variables for Waybar (no :root / --vars)
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

    /* Friendly aliases */
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

  # Main stylesheet that imports the GTK color defs above
  home.file."${cssPath}".text = ''
    @import url("colors.css");

   * {
      /* Order matters: a mono Nerd font + the Symbols set + emoji as fallback */
      font-family: "JetBrainsMono Nerd Font", "Symbols Nerd Font",
                "Noto Color Emoji", monospace, sans-serif;
      font-size: 12pt;
      color: @fg;
    }

    window#waybar {
      background: @bg;
      color: @fg;
      border-radius: 10px;
    }

    .module {
      padding: 2px 10px;
      border-radius: 8px;
      background: @bg_alt;
    }

    .module:hover { background: @bg_alt2; }

    #clock      { color: @accent; }
    #network    { color: @blue; }
    #battery    { color: @green; }
    #pulseaudio { color: @mauve; }
    #backlight  { color: @yellow; }
    #cpu        { color: @orange; }  /* was 'peach' */
    #memory     { color: @teal; }
    #tray       { color: @fg; }

    #battery.warning  { color: @yellow; }
    #battery.critical { color: @red; }
  '';

  # Waybar JSON config
  home.file."${cfgPath}".text = builtins.toJSON {
    layer = "top";
    position = "top";
    height = 28;
    margin = "6 6 0 6";
    spacing = 8;

    "modules-left"   = [ "cpu" "memory" ];
    "modules-center" = [ "clock" ];
    "modules-right"  = [ "pulseaudio" "backlight" "network" "battery" "tray" ];

    clock = {
      format = "{:%a %b %d  %H:%M}";
      tooltip = true;
      tooltip-format = "{:%A, %B %d, %Y}";
    };

    network = {
      format-wifi         = "  {essid} {signalStrength}%";
      format-ethernet     = "  {ifname}";
      format-disconnected = "󰤮  down";
      tooltip = true;
    };

    battery = {
      states = { warning = 20; critical = 10; };
      format = "{icon}  {capacity}%";
      format-charging = "󰂄  {capacity}%";
      format-plugged  = "  {capacity}%";
      format-icons = [ "󰁺" "󰁼" "󰁾" "󰂀" "󰂂" "󰁹" ];
      tooltip = true;
    };

    pulseaudio = {
      scroll-step = 1;
      format = "{icon}  {volume}%";
      format-muted = "󰝟  mute";
      format-icons = { default = [ "󰕿" "󰖀" "󰕾" ]; headphone = "󰋋"; };
      on-click = "pavucontrol || true";
    };

    backlight = { format = "󰖨  {percent}%"; };
    cpu      = { interval = 3; format = "  {usage}%"; tooltip = false; };
    memory   = { interval = 5; format = "  {used:0.1f}G"; tooltip = false; };
    tray     = { spacing = 8; };
  };
}
