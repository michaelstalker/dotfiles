# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Rake
alias rake='noglob rake'
alias rr='rake routes'
alias rrg='rake routes | grep'
alias rtg='rake -T | grep'
alias rdbm='rake db:migrate db:test:clone'

# Misc
alias b='bundle'
alias v='vim'
alias psr='ps -ef | grep ruby'

# Git
alias ga.='git add .'
alias gb='git branch'
alias gbm='git branch --merged'
alias gc='git commit'
alias gca='git commit --amend'
alias gcf='git clean -f'
alias gcn='git clean -n'
alias gco='git checkout'
alias gcod='git checkout dev'
alias gd='git diff'
alias gdc='git diff --cached'
alias glr='git pull --rebase'
alias grb='git rebase'
alias gs='git status'
alias gsh='git stash'
alias gsp='git stash pop'
alias wip='gc --no-verify -m "WIP"'

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# JavaScript
alias bower='noglob bower'

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(rails git bundler tmuxinator)

# Source files
source $ZSH/oh-my-zsh.sh

# Turn off auto-correction
unsetopt correct_all

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

# Set environment variables
export EDITOR=vim
export BUNDLER_EDITOR=vim
export PATH=/Applications/Utilities/larceny-0.97-bin-native-ia32-macosx:$PATH
export PATH=/Applications/Utilities:$PATH
export PATH=/usr/bin:$PATH
export PATH=/usr/local/bin:$PATH
export PATH=/bin:$PATH
export PATH=/usr/sbin:$PATH
export PATH=/sbin:$PATH
export PATH=/usr/local/sbin:$PATH
export PATH=/usr/local/share/npm/bin:$PATH
export PATH=/usr/local/git/bin:$PATH
export PATH=/usr/local/bin/ctags:$PATH
export PATH=/usr/X11/bin:$PATH
# export PATH=$HOME/.rbenv/bin:$PATH

source /usr/local/share/chruby/chruby.sh
source /usr/local/share/chruby/auto.sh

# rbenv
# eval "$(rbenv init -)"

if [ -f ~/.host_zshrc ]; then
  source ~/.host_zshrc
fi
