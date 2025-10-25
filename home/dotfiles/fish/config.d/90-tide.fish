# Minimal non-interactive-safe guard
status is-interactive; or exit

# Example Tide preferences (pick yours)
set -U tide_prompt_add_newline true
set -U tide_left_prompt_items pwd git
set -U tide_right_prompt_items status cmd_duration time
set -U tide_pwd_color blue
# …any other tide_* vars you use
