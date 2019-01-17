# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH
# homebrew path precedence
export PATH="/usr/local/bin:$PATH"

# rbenv
export RBENV_ROOT=/usr/local/var/rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# pyenv
export PYENV_ROOT=/usr/local/var/pyenv
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi

# nvm 
export NVM_DIR=~/.nvm
source $(brew --prefix nvm)/nvm.sh

# jenv
export JENV_ROOT=/usr/local/var/jenv
if which jenv > /dev/null; then eval "$(jenv init -)"; fi

# fastlane
export PATH="$HOME/.fastlane/bin:$PATH"

# default editor
export EDITOR=vim

# Path to your oh-my-zsh installation.
export ZSH=/Users/gjeckell/.oh-my-zsh

ZSH_THEME="agnoster"
DISABLE_UNTRACKED_FILES_DIRTY="true"

plugins=(
  colored-man-pages
)

source $ZSH/oh-my-zsh.sh

# Hide user name when it is me
DEFAULT_USER=gjeckell

# fzf show dotfiles
export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'

# nvm
export NVM_DIR=~/.nvm
source $(brew --prefix nvm)/nvm.sh

# fix lang issue
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# syntax highlight in terminal
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
