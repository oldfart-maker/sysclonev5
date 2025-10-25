# Managed by Home-Manager — Tide settings
# Runs for interactive shells; sets universals Tide expects.

status is-interactive; or exit

# Prompt composition
set -U tide_left_prompt_items  pwd git
set -U tide_right_prompt_items status cmd_duration time

# Command duration item: show if last cmd > threshold (ms)
set -U tide_cmd_duration_threshold 2000
set -U tide_cmd_duration_decimals 0

# Some common style prefs (examples — adjust to taste)
set -U tide_prompt_add_newline true
set -U tide_pwd_color blue
set -U tide_git_color green
