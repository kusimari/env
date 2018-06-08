export TERM=xterm-256color
export PS1="\[$(tput setaf 2)\]\u\[$(tput setaf 7)\]@\[$(tput setaf 3)\]\h\[$(tput setaf 7)\]: \[$(tput setaf 4)\]\W \[$(tput setaf 7)\]$ \[$(tput sgr0)\]"
alias ls="ls --color=auto -F"
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

if [ "$(uname)" == "Darwin" ]; then
   PATH=/usr/local/opt/coreutils/libexec/gnubin:$PATH
   PATH=/usr/local/bin:$PATH
   PATH=~/bin:~/.cabal/bin:$PATH

   MANPATH=/usr/local/opt/coreutils/libexec/gnuman:$MANPATH

   # export WORKON_HOME=$HOME/.virtualenvs
   # export VIRTUALENVWRAPPER_PYTHON=$(which python)
   # #/usr/local/bin/python
   # export VIRTUALENVWRAPPER_VIRTUALENV=/usr/local/bin/virtualenv
   # source /usr/local/bin/virtualenvwrapper.sh

   if [ -f $(brew --prefix)/etc/bash_completion ]; then
      . $(brew --prefix)/etc/bash_completion
   fi
fi

function docker_alias() {
    curr_directory_mountpoint="${1}"
    work_directory_in_container="${2}"
    args="${@:3}"
    cmd="docker run -it --rm"
    cmd+=" -v $(pwd):${curr_directory_mountpoint}"
    if [ ! -z "${work_directory_in_container}"]
    then
        cmd+=" -w ${work_directory_in_container}"
    fi
    cmd+=" ${args}"
    echo ${cmd}
    eval ${cmd}
}

alias python2.7="docker_alias /directory /directory python:2.7 python"
alias python3="docker_alias /directory /directory python python"
alias markdown="docker_alias /source '' jagregory/pandoc -f markdown"
