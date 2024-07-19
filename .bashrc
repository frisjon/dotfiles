# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

PATH=$PATH:~/.local/bin
export EDITOR=nano

# don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth

# Add to history instead of overriding it
shopt -s histappend

# History lenght
HISTSIZE=1000
HISTFILESIZE=2000

# Window size sanity check
shopt -s checkwinsize

# User/root variables definition
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# Colored XTERM promp
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# Colored prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
  color_prompt=yes
    else
  color_prompt=
    fi
fi

if [ -f ~/.local/bin/git-prompt.sh ];then
  source ~/.local/bin/git-prompt.sh
else
  echo "File ~/.local/bin/git-prompt.sh not found"
fi

# Prompt
export PS1="\[$(tput setaf 3)\]\$(my_pwd)\[$(tput setaf 5)\]\$(__git_ps1 \" [%s]\") \$(if [[ \$? == 0 ]]; then echo \"\[$(tput setaf 2)\]──\"; else echo \"\[$(tput setaf 1)\]──\"; fi)\[$(tput setaf 7)\] "

trap 'echo -ne "\e[0m"' DEBUG

# I this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# Color support
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
fi

# Alias definitions.
if [ -f ~/.aliases ]; then
    . ~/.aliases
fi

# Auto-completion
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# Go back with ..
b() {
  if [ $# -eq 0 ]; then
    cd ".."
  else
    str=""
    count=0
    while [ "$count" -lt "$1" ]; do
      str=$str"../"
      let count=count+1
    done
    cd $str
  fi
}

# Color man pages
man() {
  env \
    LESS_TERMCAP_mb=$(printf "\e[1;31m") \
    LESS_TERMCAP_md=$(printf "\e[1;31m") \
    LESS_TERMCAP_me=$(printf "\e[0m") \
    LESS_TERMCAP_se=$(printf "\e[0m") \
    LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
    LESS_TERMCAP_ue=$(printf "\e[0m") \
    LESS_TERMCAP_us=$(printf "\e[1;32m") \
      man "$@"
}

extract () {
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1     ;;
      *.tar.gz)    tar xzf $1     ;;
      *.bz2)       bunzip2 $1     ;;
      *.rar)       unrar e $1     ;;
      *.gz)        gunzip $1      ;;
      *.tar)       tar xf $1      ;;
      *.tbz2)      tar xjf $1     ;;
      *.tgz)       tar xzf $1     ;;
      *.zip)       unzip $1       ;;
      *.Z)         uncompress $1  ;;
      *.7z)        7z x $1        ;;
      *)           echo "'$1' cannot be extracted via extract()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

my_pwd() {
  PWDPATH="$(dirs +0)"
  if [ "${#PWDPATH}" -gt 32 ];then
    NEWPATH=""
    IFS='/' read -ra DIRS <<< "$PWDPATH"
    LENGTH=${#DIRS[@]}
    for ((c=${#DIRS[@]};c>1;c--));do
      i="${DIRS[LENGTH-c]}"
      NEWPATH=$NEWPATH/"${i:0:3}"
      if [ "${#i}" -gt 3 ];then
        NEWPATH=$NEWPATH"~"
      fi
    done
    NEWPATH=$NEWPATH/"${DIRS[LENGTH-1]}"
    NEWPATH="${NEWPATH/\//}"
    echo "$NEWPATH"
  else
    echo "$PWDPATH"
  fi
}

# set keyboard layout
[[ ! -z !(which setxkbmap) ]] && setxkbmap customo

$HOME/.local/bin/check.sh
. "$HOME/.cargo/env"
