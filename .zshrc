# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"
#
# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
plugins=(rails git bundler tmuxinator)

# Source files
source $ZSH/oh-my-zsh.sh


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
alias gdm='git-delete-merged'

# Git
alias ga.='git add .'
alias gb='git branch'
alias gbm='git branch --merged'
# alias gc='git commit --verbose'
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
alias wip='git commit --no-verify -m "WIP"'
alias glog='git log --graph --abbrev-commit --decorate --date=relative --format=format:"%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)" --all'

# JavaScript
alias bower='noglob bower'

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
export PATH=~/bin:$PATH
export PATH=/usr/sbin:$PATH
export PATH=/sbin:$PATH
export PATH=/usr/local/sbin:$PATH
export PATH=/usr/local/share/npm/bin:$PATH
export PATH=/usr/local/git/bin:$PATH
export PATH=/usr/local/bin/ctags:$PATH
export PATH=/usr/X11/bin:$PATH

if [ -f /usr/local/share/chruby/chruby.sh ]; then
  source /usr/local/share/chruby/chruby.sh
fi

if [ -f /usr/local/share/chruby/auto.sh ]; then
  source /usr/local/share/chruby/auto.sh
fi

if [ -f ~/.bin/tmuxinator.zsh ]; then
  source ~/.bin/tmuxinator.zsh
fi

if [ -f ~/.host_zshrc ]; then
  source ~/.host_zshrc
fi

if which jenv > /dev/null; then
  eval "$(jenv init -)";
fi

export PATH="$HOME/.yarn/bin:$PATH"
[[ -s "$HOME/.avn/bin/avn.sh" ]] && source "$HOME/.avn/bin/avn.sh" # load avn

# FZF
export FZF_DEFAULT_COMMAND='fd --type f'
