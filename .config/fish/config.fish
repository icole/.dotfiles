if status is-interactive
end

set -x PATH $PATH ~/.emacs.d/bin ~/.local/bin ./node_modules/.bin ~/.dotnet/tools

starship init fish | source
