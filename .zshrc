# If you come from bash you might have to change your $PATH.
# homebrew path precedence
export PATH="/usr/local/sbin:$PATH"

# rbenv
export RBENV_ROOT=/usr/local/var/rbenv
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# pyenv
export PYENV_ROOT=/usr/local/var/pyenv
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi

# nvm 
export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"
[ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && . "/usr/local/opt/nvm/etc/bash_completion.d/nvm"

# jenv
export JENV_ROOT=/usr/local/var/jenv
if which jenv > /dev/null; then eval "$(jenv init -)"; fi

# default editor
export EDITOR=vim

# swiftenv
export SWIFTENV_ROOT=/usr/local/var/swiftenv
if which swiftenv > /dev/null; then eval "$(swiftenv init -)"; fi

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"
DISABLE_UNTRACKED_FILES_DIRTY="true"

plugins=(
  colored-man-pages
)

source $ZSH/oh-my-zsh.sh

# enable fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# fzf show dotfiles
export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'

# syntax highlight in terminal
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

