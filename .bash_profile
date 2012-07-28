alias ll='ls -l'

# Testing
alias rsp='bundle exec rspec'
alias cuke='bundle exec cucumber'

# Rails
alias ss='script/server'
alias sc='script/console'
alias rc='rails console'
alias rg='rails generate'
alias rdm='rake db:migrate'
alias rdmr='rake db:migrate:redo'

# Git
source ~/git-completion.bash
 
alias gs='git status'
alias gd='git diff'
alias gdc='git diff --cached'
alias ga='git add'
alias ga.='git add .'
alias gp='git push'
alias gpo='git push origin'
alias gpl='git pull'
alias gc='git commit'
alias gb='git branch'
alias gm='git merge'
alias grb='git rebase'
alias gco='git checkout'

# VI
alias vi='vim'
set -o vi

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" # Load RVM function

export BUNDLER_EDITOR=mvim

# Exuberant Ctags
export PATH="/usr/local/bin:$PATH"
export PATH=/Applications/Utilities/:/Applications/Utilities/larceny-0.97-bin-native-ia32-macosx/

if [ -f ~/.host_bash_profile ]; then
  source ~/.host_bash_profile
fi

if [ -f ~/.bashrc ]; then
  source ~/.bashrc
fi
