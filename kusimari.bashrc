export TERM=xterm-256color

PATH=/usr/local/opt/coreutils/libexec/gnubin:$PATH
PATH=/usr/local/bin:$PATH
PATH=~/bin:~/.cabal/bin:$PATH

MANPATH=/usr/local/opt/coreutils/libexec/gnuman:$MANPATH
alias ls="ls --color=auto -F"
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

export PS1="\[$(tput setaf 2)\]\u\[$(tput setaf 7)\]@\[$(tput setaf 3)\]\h\[$(tput setaf 7)\]: \[$(tput setaf 4)\]\W \[$(tput setaf 7)\]$ \[$(tput sgr0)\]"

#export WORKON_HOME=$HOME/.virtualenvs
#export VIRTUALENVWRAPPER_PYTHON=$(which python)
#/usr/local/bin/python
#export VIRTUALENVWRAPPER_VIRTUALENV=/usr/local/bin/virtualenv
#source /usr/local/bin/virtualenvwrapper.sh
