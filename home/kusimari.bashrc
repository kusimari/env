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
    pre_image_args="${3}"
    post_image_args="${@:4}"
    cmd="docker run -it --rm"
    cmd+=" -v $(pwd):${curr_directory_mountpoint}"
    if [ ! -z "${work_directory_in_container}" ]
    then
        cmd+=" -w ${work_directory_in_container}"
    fi
    if [ ! -z "${pre_image_args}" ]
    then
        cmd+=" ${pre_image_args}"
    fi
    cmd+=" ${post_image_args}"
    # echo ${cmd}
    eval ${cmd}
}

alias python2.7="docker_alias /home /home '' python:2.7 python"
alias python3="docker_alias /home /home '' python python"
alias jupyter="docker_alias /home /home '-p 8888:8888' jupyter/minimal-notebook"

alias pandoc="docker_alias /source '' '' jagregory/pandoc"
alias markdown="docker_alias /source '' '' jagregory/pandoc -f markdown"

alias stack="docker_alias /home /home '-v ~/.stack:/root/.stack' fpco/stack-build stack"

alias nuclide_remote="docker_alias /home home '-p 9090:9090 -p 9091:22' marcel/nuclide-remote

# alias node="docker_alias /home /home '' '' node node"
# alias npm="docker_alias /home /home '' '' node npm"