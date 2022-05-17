if status is-interactive
end

set -U fish_user_paths ~/.local/bin ./node_modules/.bin $fish_user_paths

oh-my-posh init fish --config ~/.poshthemes/nord.omp.json | source
