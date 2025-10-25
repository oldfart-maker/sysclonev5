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
set -U tide_aws_bg_color FF9900
set -U tide_aws_color 232F3E
set -U tide_aws_icon 
set -U tide_bun_bg_color FBF0DF
set -U tide_bun_color 14151A
set -U tide_bun_icon 󰳓
set -U tide_character_color 5FD700
set -U tide_character_color_failure FF0000
set -U tide_character_icon ❯
set -U tide_character_vi_icon_default ❮
set -U tide_character_vi_icon_replace ▶
set -U tide_character_vi_icon_visual V
set -U tide_cmd_duration_bg_color C4A000
set -U tide_cmd_duration_color 000000
set -U tide_cmd_duration_decimals 0
set -U tide_cmd_duration_icon 
set -U tide_cmd_duration_threshold 2000
set -U tide_context_always_display false
set -U tide_context_bg_color 444444
set -U tide_context_color_default D7AF87
set -U tide_context_color_root D7AF00
set -U tide_context_color_ssh D7AF87
set -U tide_context_hostname_parts 1
set -U tide_crystal_bg_color FFFFFF
set -U tide_crystal_color 000000
set -U tide_crystal_icon 
set -U tide_direnv_bg_color D7AF00
set -U tide_direnv_bg_color_denied FF0000
set -U tide_direnv_color 000000
set -U tide_direnv_color_denied 000000
set -U tide_direnv_icon ▼
set -U tide_distrobox_bg_color FF00FF
set -U tide_distrobox_color 000000
set -U tide_distrobox_icon 󰆧
set -U tide_docker_bg_color 2496ED
set -U tide_docker_color 000000
set -U tide_docker_default_contexts default colima
set -U tide_docker_icon 
set -U tide_elixir_bg_color 4E2A8E
set -U tide_elixir_color 000000
set -U tide_elixir_icon 
set -U tide_gcloud_bg_color 4285F4
set -U tide_gcloud_color 000000
set -U tide_gcloud_icon 󰊭
set -U tide_git_bg_color 4E9A06
set -U tide_git_bg_color_unstable C4A000
set -U tide_git_bg_color_urgent CC0000
set -U tide_git_color green
set -U tide_git_color_branch 000000
set -U tide_git_color_conflicted 000000
set -U tide_git_color_dirty 000000
set -U tide_git_color_operation 000000
set -U tide_git_color_staged 000000
set -U tide_git_color_stash 000000
set -U tide_git_color_untracked 000000
set -U tide_git_color_upstream 000000
set -U tide_git_icon 
set -U tide_git_truncation_length 24
set -U tide_git_truncation_strategy
set -U tide_go_bg_color 00ACD7
set -U tide_go_color 000000
set -U tide_go_icon 
set -U tide_java_bg_color ED8B00
set -U tide_java_color 000000
set -U tide_java_icon 
set -U tide_jobs_bg_color 444444
set -U tide_jobs_color 4E9A06
set -U tide_jobs_icon 
set -U tide_jobs_number_threshold 1000
set -U tide_kubectl_bg_color 326CE5
set -U tide_kubectl_color 000000
set -U tide_kubectl_icon 󱃾
set -U tide_left_prompt_frame_enabled true
set -U tide_left_prompt_items pwd git
set -U tide_left_prompt_prefix
set -U tide_left_prompt_separator_diff_color 
set -U tide_left_prompt_separator_same_color 
set -U tide_left_prompt_suffix 
set -U tide_nix_shell_bg_color 7EBAE4
set -U tide_nix_shell_color 000000
set -U tide_nix_shell_icon 
set -U tide_node_bg_color 44883E
set -U tide_node_color 000000
set -U tide_node_icon 
set -U tide_os_bg_color 4D4D4D
set -U tide_os_color 1793D1
set -U tide_os_icon 
set -U tide_php_bg_color 617CBE
set -U tide_php_color 000000
set -U tide_php_icon 
set -U tide_private_mode_bg_color F1F3F4
set -U tide_private_mode_color 000000
set -U tide_private_mode_icon 󰗹
set -U tide_prompt_add_newline true
set -U tide_prompt_add_newline_before true
set -U tide_prompt_color_frame_and_connection 585858
set -U tide_prompt_color_separator_same_color 949494
set -U tide_prompt_icon_connection ─
set -U tide_prompt_min_cols 34
set -U tide_prompt_pad_items true
set -U tide_prompt_transient_enabled false
set -U tide_pulumi_bg_color F7BF2A
set -U tide_pulumi_color 000000
set -U tide_pulumi_icon 
set -U tide_pwd_bg_color 3465A4
set -U tide_pwd_color blue
set -U tide_pwd_color_anchors E4E4E4
set -U tide_pwd_color_dirs E4E4E4
set -U tide_pwd_color_truncated_dirs BCBCBC
set -U tide_pwd_icon 
set -U tide_pwd_icon_home 
set -U tide_pwd_icon_unwritable 
set -U tide_pwd_markers .bzr .citc .git .hg .node-version .python-version .ruby-version .shorten_folder_marker .svn .terraform bun.lockb Cargo.toml composer.json CVS go.mod package.json build.zig
set -U tide_python_bg_color 444444
set -U tide_python_color 00AFAF
set -U tide_python_icon 󰌠
set -U tide_right_prompt_frame_enabled true
set -U tide_right_prompt_items status cmd_duration time
set -U tide_right_prompt_prefix 
set -U tide_right_prompt_separator_diff_color 
set -U tide_right_prompt_separator_same_color 
set -U tide_right_prompt_suffix
set -U tide_ruby_bg_color B31209
set -U tide_ruby_color 000000
set -U tide_ruby_icon 
set -U tide_rustc_bg_color F74C00
set -U tide_rustc_color 000000
set -U tide_rustc_icon 
set -U tide_shlvl_bg_color 808000
set -U tide_shlvl_color 000000
set -U tide_shlvl_icon 
set -U tide_shlvl_threshold 1
set -U tide_status_bg_color 2E3436
set -U tide_status_bg_color_failure CC0000
set -U tide_status_color 4E9A06
set -U tide_status_color_failure FFFF00
set -U tide_status_icon ✔
set -U tide_status_icon_failure ✘
set -U tide_terraform_bg_color 800080
set -U tide_terraform_color 000000
set -U tide_terraform_icon 󱁢
set -U tide_time_bg_color D3D7CF
set -U tide_time_color 000000
set -U tide_time_format '%r'
set -U tide_toolbox_bg_color 613583
set -U tide_toolbox_color 000000
set -U tide_toolbox_icon 
set -U tide_vi_mode_bg_color_default 949494
set -U tide_vi_mode_bg_color_insert 87AFAF
set -U tide_vi_mode_bg_color_replace 87AF87
set -U tide_vi_mode_bg_color_visual FF8700
set -U tide_vi_mode_color_default 000000
set -U tide_vi_mode_color_insert 000000
set -U tide_vi_mode_color_replace 000000
set -U tide_vi_mode_color_visual 000000
set -U tide_vi_mode_icon_default D
set -U tide_vi_mode_icon_insert I
set -U tide_vi_mode_icon_replace R
set -U tide_vi_mode_icon_visual V
set -U tide_zig_bg_color F7A41D
set -U tide_zig_color 000000
set -U tide_zig_icon 

