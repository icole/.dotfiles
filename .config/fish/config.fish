if status is-interactive
end

set -U fish_user_paths ~/.local/bin ./node_modules/.bin $fish_user_paths

oh-my-posh init fish --config ~/.poshthemes/blueish.omp.json | source
set -gx VOLTA_HOME "$HOME/.volta"
set -gx PATH "$VOLTA_HOME/bin" $PATH

source /Users/icole/.docker/init-fish.sh || true # Added by Docker Desktop
