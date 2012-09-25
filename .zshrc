# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Example aliases
# alias zshconfig="vim ~/.zshrc"
# alias ohmyzsh="vim ~/.oh-my-zsh"

# Shell
alias la='ls -la'

# Testing
alias cuke='bundle exec cucumber'

# Git
alias gs='git status'
alias gd='git diff'
alias gdc='git diff --cached'
alias gdw='git diff --word-diff'
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

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(bundler rails rails3 rvm)

# Source files
source $ZSH/oh-my-zsh.sh
 
if [ -f ~/.host_zshrc ]; then
  source ~/.host_zshrc
fi

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" # Load RVM function

# Turn off auto-correction
unsetopt correct_all

# Set environment variables
export EDITOR=vim
export BUNDLER_EDITOR=mvim
export PATH=$HOME/pear/bin/:/Applications/Utilities/larceny-0.97-bin-native-ia32-macosx/:/Applications/Utilities/:$HOME/.rvm/gems/ruby-1.9.2-p290/bin:$HOME/.rvm/gems/ruby-1.9.2-p290@global/bin:$HOME/.rvm/rubies/ruby-1.9.2-p290/bin:$HOME/.rvm/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:/usr/local/git/bin
