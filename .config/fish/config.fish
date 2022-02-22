if status is-interactive
end

set -x PATH $PATH ~/.emacs.d/bin ~/.local/bin ./node_modules/.bin

starship init fish | source
