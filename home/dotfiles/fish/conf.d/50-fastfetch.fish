# Show Fastfetch once per interactive shell
status is-interactive; or exit

# Optional: only in Foot terminals
if test "$TERM" = "foot"
   set -q FASTFETCH_PRINTED; or begin
       set -gx FASTFETCH_PRINTED 1
       command -sq fastfetch; and fastfetch
   end
end

# In all terminals (guarded to print once per shell)
set -q FASTFETCH_PRINTED; or begin
    set -gx FASTFETCH_PRINTED 1
    command -sq fastfetch; and fastfetch
end
