if status is-interactive
    # Commands to run in interactive sessions can go here
end

function fish_greeting
	 fastfetch
end

function vterm_printf;
    if [ -n "$TMUX" ]
        # tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

function emacs-profile
    set config_dir "$HOME/.config/$argv[1]"

    if test -d "$config_dir"
        command emacs --init-directory "$config_dir" $argv[2..-1]
    else
        echo "Emacs config '$argv[1]' not found in ~/.config"
        return 1
    end
end


if not contains $HOME/.local/bin $PATH
    set PATH $HOME/.local/bin $PATH
end

if not contains $HOME/.local/bin/python-scripts $PATH
    set PATH $HOME/.local/bin/python-scripts $PATH
end
