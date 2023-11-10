" let $TERM="screen-256color"
if empty("$TMUX")
    set termguicolors
endif
" https://github.com/zchee/deoplete-jedi/wiki/Setting-up-Python-for-Neovim
let $PYENV_ROOT = systemlist('pyenv root')[0]
" let g:python_host_prog=systemlist('pyenv root')[0].'/versions/neovim2/bin/python'
let g:python3_host_prog=systemlist('pyenv root')[0].'/versions/neovim3/bin/python'

set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vim/vimrc
set undodir=~/.vim/tmp/nvim//
