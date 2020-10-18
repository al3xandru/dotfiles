scriptencoding utf-8

" Organization
" 1. important
" 2. moving around, searching and patterns
" 3. tags
" 4. displaying text
" 5. syntax, highlighting and spelling
" 6. multiple windows
" 7. multiple tab pages
" 8. terminal
" 9. using the mouse
" 10. printing
" 11. messages and info
" 12. selecting text
" 13. editing text
" 14. tabs and indenting
" 15. folding
" 16. diff mode
" 17. mapping
" 18. reading and writing files
" 19. the swap file
" 20. command line editing
" 21. executing external commands
" 22. running make and jumping to errors
" 23. language specific
" 24. multi-byte characters
" 25. various

" 1. important {{{
set nocompatible
set backspace=2
set backspace=indent,eol,start  " allow backspace in insert mode
" Use the OS clipboard by default (on versions compiled with `+clipboard`)
if has('clipboard')
    if has('unnamedplus')
        set clipboard^=unnamed,unnamedplus
    else
        set clipboard=unnamed
    endif
endif
if !has('nvim')
    set esckeys     " allow cursor keys in insert mode
endif
set hidden      " allow buffer switching without saving
set lazyredraw
set modeline
set modelines=5
set mouse=a
set mousehide
set sessionoptions-=options
set sessionoptions+=curdir
set showcmd     " show command in bottom bar
set showmode    " display the current mode
set wildmenu    " enhance command-line completion
set wildignore+=.hg,.git,.svn  " version control
set wildignore+=*.o,*.obj,*.exe,*.dll,*.pyc,*.pyo,*.class,*.luac " compiled
set wildignore+=*.DS_Store,*.sw?
set wildignore+=.idea/**
set wildignore+=*.png,*.jpg,*.gif,*.bmp
set wildignore+=*.egg,*.egg-info,*.gem
set wildignore+=*.zip,*.tar.gz,*.gzip,*.rar
set wildignore+=*.aux,*.toc " Latex intermediary files

" }}}


" 2. moving around, searching and patterns {{{
set ignorecase
set incsearch
set matchtime=3     " tenths of a second to show the matching paren
set nohlsearch
set smartcase
set showmatch
set scrolloff=5
" }}}


" 3. omnicomplete {{{
set infercase
set omnifunc=syntaxcomplete#Complete
" https://github.com/sjl/dotfiles/blob/eea18b00b8c74943f5094fddf91d3c2a7e0a7242/vim/vimrc#L534
" kspell: dictionary completion only when spell enabled set spell 
set complete=.,w,b,u,t,i,kspell
" http://vim.wikia.com/wiki/Make_Vim_completion_popup_menu_work_just_like_in_an_IDE
set completeopt=longest,menu,preview
" }}}


" 4. displaying text {{{
filetype on
filetype plugin on
filetype plugin indent on

set synmaxcol=500
set listchars=tab:▸\ ,trail:·,eol:¬
set showbreak=↪
" set showbreak=⤿

" line numbers {{{
augroup lineno
    autocmd!
    autocmd FocusLost * set norelativenumber number
    autocmd InsertEnter * set norelativenumber number
    autocmd InsertLeave * set relativenumber number
    autocmd Filetype qf setlocal norelativenumber number nowrap
augroup END
set number
set relativenumber
set numberwidth=2   " keep the line number gutter narrow so 3 digits is cozy
" }}}
" }}}


" 5. syntax, highlighting and spelling" {{{
syntax on       " enable syntax processing


" keyworkdprg, dictionary, thesaurus {{{
set dictionary=/usr/share/dict/words
set thesaurus+=~/.vim/mthesaur.txt

if has('mac')  " unix, win32, win64
    set keywordprg=:!open\ dict://\
else
    set keywordprg=:!open\ https://www.google.com/search?q=\
endif
augroup keywordprog
    autocmd!
    autocmd FileType vim setlocal keywordprg=:help
    autocmd FileType java setlocal keywordprg=:DashSearch
augroup END

" }}}
" }}}


" 6. multiple windows {{{
set title
set laststatus=2
" http://got-ravings.blogspot.co.at/2008/08/vim-pr0n-making-statuslines-that-own.html
" Colors: Error, Question, Search, Todo, Visual, WarningMsg
set statusline=%q[%#Error#%{toupper(mode())}%*:b%n]\ %f%m%r
set statusline+=\ %#Todo#%{exists('g:loaded_fugitive')?fugitive#statusline():''}%*
set statusline+=%= "left/right separator
set statusline+=%#Todo#%{exists('g:loaded_obsession')?ObsessionStatus():''}%*
set statusline+=%y[%{&fileencoding?&fileencoding:&encoding}]
set statusline+=%<[%v:%l/%L][a\%03.3b:h\%02.2B]

" GUI {{{
" set guicursor=n-v-c:block-Cursor,ve:ver35-Cursor,o:hor50-Cursor,i-ci:ver25-lCursor,r-cr:hor20-Cursor,sm:block
set guioptions=aAce
set selection=exclusive
if has("gui_running")
    set go-=T
    set go-=l
    set go-=L
    set go-=r " no scrollbar
    set go-=R
    if has("gui_macvim")
        " set gfn=Anonymous_Pro:h12
        " set gfn=Consolas:h12
        " set gfn=Cousine:h11
        " set gfn=Fira_Code_Regular:h12
        " set gfn=Hack:h12
        " set gfn=Inconsolata:h13
        " set gfn=Input_Mono:h11
        " set gfn=Liberation_Mono:h11
        set gfn=Operator_Mono:h12
        " set gfn=PragmataPro_Mono:h13
        " set gfn=Source_Code_Pro:h11
        " set gfn=SF_Mono_Regular:h12 linespace=2
        " add "New Window" menu to MacVim (stupid but needed)
        " aun File.New\ Window
        an <silent> 10.290 File.New\ Window :silent !mvim<CR>
        macm File.New\ Window key=<D-n>
    elseif has("gui_gtk2")
        set gfn=monofur\ 12,SourceCodePro\ 10,Anonymous\ Pro\ 10
    endif

    if !has("gui_vimr")
        set columns=110
        set lines=80
    endif
    if &diff 
        set columns=180
    endif
    " augroup VimTransparency
    "     autocmd!
    "     autocmd FocusGained * set transparency=0
    "     autocmd FocusLost * set transparency=25
    " augroup END
endif
" }}}

" splits
set splitbelow
set splitright

" }}}


" 11. messages and info {{{
set ruler
set noerrorbells
set novisualbell
set t_vb=
if has("gui_macvim")
    set visualbell t_vb=
endif
" }}}


" 14. tabs and indenting {{{
set expandtab       " TABs are spaces
set tabstop=4       " number of visual spaces per TAB
set shiftwidth=4
set softtabstop=4   " number of spaces in TAB when editing
set smartindent
set autoindent
" }}}


" 15. folding {{{
set foldenable
set foldmethod=indent
set foldnestmax=10
set foldlevel=100
" }}}


" 16 diff mode
set diffopt=filler,vertical


" 17. mappings {{{
set timeoutlen=750


" leader = space (default \)
" localleader = ,
" let mapleader="\<space>"
" let maplocalleader=","

" (N)Mappings: n/N with blip, center the line, and directional {{{2
" http://vi.stackexchange.com/questions/2761/set-cursor-colour-different-when-on-a-highlighted-word
nnoremap <expr> n 'Nn'[v:searchforward] . 'zzzv:call <SID>HLNext(0.6)<CR>'
nnoremap <expr> N 'nN'[v:searchforward] . 'zzzv:call <SID>HLNext(0.6)<CR>'

function! <SID>HLNext (blinktime)
    let l:target_pat = '\c\%#'.@/
    let l:ring = matchadd('ErrorMsg', l:target_pat, 101)
    redraw
    exec 'sleep ' . float2nr(a:blinktime * 1000) . 'm'
    call matchdelete(l:ring)
    redraw
endfunction
" Previous implementation {{{3
" nnoremap <expr> n v:searchforward ? 'nzzzv:call <SID>HLNext(0.6)<CR>' : 'Nzzzv:call <SID>HLNext(0.6)<CR>'
" nnoremap <expr> N v:searchforward ? 'Nzzzv:call <SID>HLNext(0.6)<CR>' : 'nzzzv:call <SID>HLNext(0.6)<CR>'
" Plug 'timakro/vim-searchant'
" nnoremap <silent> n nzzzv:call <SID>HLNext(0.6)<CR>
" nnoremap <silent> N Nzzzv:call <SID>HLNext(0.6)<cr>
" 3}}}
" 2}}}
" }}}

" 19. backup & swap file {{{
set backup
set noswapfile
set undofile
set history=10000
set backupdir=~/.vim/tmp/backup// "~/.vim/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim/tmp/swap// "~/.vim/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set undodir=~/.vim/tmp/undo//
set writebackup

if !isdirectory(expand(&backupdir))
    call mkdir(expand(&backupdir), "p")
endif
if !isdirectory(expand(&directory))
    call mkdir(expand(&directory), "p")
endif
if !isdirectory(expand(&undodir))
    call mkdir(expand(&undodir), "p")
endif
"}}}

" 24. multi-byte characters
setglobal fileencoding=utf-8
set encoding=utf-8 nobomb

" autocmds
if has("autocmd")
    augroup vimrc
        autocmd!
        " autosave on focus lost
        autocmd FocusLost, BufLeave * silent! :wall
        " default to markdown if not in diff mode
        autocmd BufEnter * if &filetype == "" && !&diff | setlocal ft=markdown | endif
        " crontab -e
        autocmd BufNewFile,BufRead crontab.* set nobackup nowritebackup

        " json is javascript
        autocmd BufNewFile,BufRead *.json setfiletype json syntax=javascript

        autocmd FileType html,htm setlocal ts=2 sts=2 sw=2 expandtab
        autocmd FileType make setlocal ts=4 sts=4 sw=4 noexpandtab
        autocmd FileType java setlocal omnifunc=javacomplete#Complete
        autocmd FileType python setlocal ts=4 sts=4 sw=4 expandtab autoindent
        autocmd FileType vim setlocal nowrap
        autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
    augroup END
end


" PYENV {{{1
" https://github.com/macvim-dev/macvim/issues/386
if has('mac')
    if executable("pyenv") && !has('nvim')
        " v.1
        " let _cmd = 'pyenv version-name'
        " let _pyenv=substitute(system(_cmd), '[\]\|[[:cntrl:]]', '', 'g')
        " v.2
        " let _cmd = 'python -c "import sys;vt=sys.version_info;sys.stdout.write(\".\".join([str(v) for v in vt[:3]]))"'
        " let _pyver=substitute(system(_cmd), '[\]\|[[:cntrl:]]', '', 'g')
        " check: https://github.com/macvim-dev/macvim/blob/b906f87e8c0ef8585e5aaeb72ca4aadbbd1b8155/src/MacVim/vimrc#L21
        " Python 2
        let _cmd = 'pyenv versions --bare --skip-aliases | grep -E "2\.[[:digit:]]+\.[[:digit:]]+$" | tail -1'
        let _pyenv=substitute(system(_cmd), '[\]\|[[:cntrl:]]', '', 'g')
        let _pyver=substitute(_pyenv, '^\s*\(.*\)', '\1', '')
        let _pyvermaj=join(split(_pyver, "\\.")[0:1], ".")
        let &pythondll=$HOME . "/.pyenv/versions/" . _pyver . "/lib/libpython" . _pyvermaj . ".dylib"
        let &pythonhome=$HOME . "/.pyenv/versions/" . _pyver
        " Python 3
        let _cmd = 'pyenv versions --bare --skip-aliases | grep -E "3\.[[:digit:]]\.[[:digit:]]+$" | tail -1'
        let _pyenv=substitute(system(_cmd), '[\]\|[[:cntrl:]]', '', 'g')
        let _pyver=substitute(_pyenv, '^\s*\(.*\)', '\1', '')
        " let _pyver=_pyenv
        " following line replaced by next one dealing better with versions
        " let _pyvermaj=strpart(_pyver, 0, 3)
        let _pyvermaj=join(split(_pyver, "\\.")[0:1], ".")

        " I don't know how to do set pythondll thus the let &pythondll
        let &pythonthreehome = $HOME .  "/.pyenv/versions/" . _pyver
        let &pythonthreedll = $HOME . "/.pyenv/versions/" . _pyver . "/lib/libpython" . _pyvermaj . ".dylib"

        " echom "&pythonhome     :" . &pythonhome
        " echom "&pythondll      :" . &pythondll
        " echom "&pythonthreehome:" . &pythonthreehome
        " echom "&pythonthreedll :" . &pythonthreedll

        " http://stackoverflow.com/questions/30443836/install-vim-via-homebrew-with-python-and-python3-support
        if _pyvermaj > '3.0' && has('python3')
            " echom "PYTHON3k  :YES"
            let g:jedi#force_py_version = 3
            let g:pymode_python = 'python3'
        else
            " echom "PYTHON3k  :NO"
            let g:jedi#force_py_version = 2
            let g:pymode_python = 'python'
        endif
    endif
endif
"1}}}

" Functions {{{1
" Save current session {{{2
function! <SID>SaveSession() 
    let l:parentDir = getcwd()
    let l:sessionFile = expand("~/.sessions/vim/") .  strftime("%Y-%m-%dT%H%M%S") .  "_" .  join(split(l:parentDir, "/"),"~") . ".vim"
    echom "Parent dir  :" .  l:parentDir
    echom "Session file:" .  l:sessionFile
    " exec "mksession! " . l:parentDir . "/.session.vim"
    " echo "session saved " . l:parentDir . "/.session.vim"
    if isdirectory(l:parentDir . "/.git")
        let l:sessionFile = l:parentDir  .  "/.git/.session.vim"
    endif
    if exists("g:loaded_obsession")
        exec "Obsession " .  l:sessionFile
    else
        exec "mksession! " . l:sessionFile
    endif
    echom "session saved in " . l:sessionFile 
endfunction
command! SaveSession call <SID>SaveSession()
command! SS call <SID>SaveSession()
" 2}}}

" Display a window with big font {{{2
function! <SID>BigWnd() 
    colorscheme nuvola
    " set gfn=Operator_Mono:h24
    set gfn=mononoki:h24
    set colorcolumn=0
    set columns=80
    " edit "~/Desktop/" .  strftime("%Y%m%d-%H%M") .  ".md"
    exec "edit ~/Dropbox/Dox/mydox/myjrnl/tmp-" .  strftime("%Y%m%d-%H%M") .  ".md"
    startinsert
endfunction
command! Bigwnd call <SID>BigWnd()
" 2}}}

" Set GUI font {{{2
function! <SID>Fonts()
    let l:fonts = "Anka/Coder_Narrow Cousine Fira_Code Go_Mono Hack IBM_Plex_Mono Iosevka JetBrains_Mono mononoki Operator_Mono PragmataPro_Mono Source_Code_Pro"
    let l:flst = split(l:fonts)
    echo "Fonts:"
    echo join(l:flst, "\n")
    echo "\n"
    exec "set gfn=" . input("Font: ", "mononoki") . ":h" . input("Size: ", "24")
endfunction
command! SetFont call <SID>Fonts()
"2}}}
"1}}}


" Load vim-plug https://github.com/junegunn/vim-plug
call plug#begin('~/.vim/plugged')
" Alternative plugin managers
" - vundle
" - junegunn/vim-plug 
" - Shougo/neobundle.vim

" colorschemes {{{1
Plug 'vim-scripts/Colour-Sampler-Pack'

" sources {{{2
" https://www.reddit.com/r/neovim/comments/e04207/what_are_the_best_maintained_and_most_extensive/  
" https://www.reddit.com/r/neovim/comments/ehjsbk/looking_for_good_light_colorscheme/
" }}}

" top (supporting text styling) {{{2
" Plug 'morhetz/gruvbox'
Plug 'gruvbox-community/gruvbox'

" light
Plug 'kamwitsta/flatwhite-vim'
Plug 'NLKNguyen/papercolor-theme'

" dark
Plug 'arcticicestudio/nord-vim'
Plug 'joshdick/onedark.vim'
Plug 'trevordmiller/nova-vim'
" }}}

" ok {{{2
Plug 'AlessandroYorba/Alduin'
let g:alduin_Shout_Aura_Whisper = 1
let g:alduin_Shout_Fire_Breath = 1

" light
Plug 'nice/sweater'
Plug 'zefei/cake16'
" 2}}}

" simple light {{{2
Plug 'arzg/vim-plan9'
Plug 'rakr/vim-one'
let g:one_allow_italics = 1

"2}}}
"1}}}


" Extra text objects {{{1
Plug 'kana/vim-textobj-user'
" Function argument object: i, a,
" Shift: <, >,
" Jump: [, ],
Plug 'PeterRincker/vim-argumentative'
" Plug 'argtextobj.vim' 

" Function call object: am im aM iM
Plug 'thalesmello/vim-textobj-methodcall'

" Function object: if af iF aF
Plug 'kana/vim-textobj-function'
Plug 'kamichidu/vim-textobj-function-go'
Plug 'thinca/vim-textobj-function-javascript'
Plug 'thinca/vim-textobj-function-perl'
Plug 'nelstrom/vim-textobj-rubyblock'

" Comment object: ic ac aC
Plug 'glts/vim-textobj-comment'

" URIs: iu au
Plug 'jceb/vim-textobj-uri'
Plug 'wellle/targets.vim'
"
" Line object: il al (DISABLED)
" Plug 'kana/vim-textobj-line'
" Indent object: ii ai aI iI (DISABLED)
" Plug 'michaeljsmith/vim-indent-object' 
" }}}


" Essentials {{{1
"
" Files {{{2
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
let g:fzf_layout = {'up': '~20%'}
" 2}}}


" Netrw {{{2
autocmd FileType netrw setlocal bufhidden=delete "wipe
let g:netrw_banner = 0
let g:netrw_browse_split=0      " re-use the same window
let g:netrw_hide=0
" let g:netrw_home='~'
let g:netrw_preview=0   "horizontal
let g:netrw_alto=0      "aboveleft
let g:netrw_altv=1      "open splits to the right
let g:netrw_liststyle=0 " 3: thin long wide tree; 0: thin list
let g:netrw_winsize=25
" from vinegar
let s:dotfiles = '\(^\|\s\s\)\zs\.\S\+'
let s:escape = 'substitute(escape(v:val, ".$~"), "*", ".*", "g")'
let g:netrw_sort_sequence = '[\/]$,*,\%(' . join(map(split(&suffixes, ','), 'escape(v:val, ".*$~")'), '\|') . '\)[*@]\=$'
let g:netrw_list_hide =
      \ join(map(split(&wildignore, ','), '"^".' . s:escape . '. "$"'), ',') . ',^\.\.\=/\=$' .
      \ (get(g:, 'netrw_list_hide', '')[-strlen(s:dotfiles)-1:-1] ==# s:dotfiles ? ','.s:dotfiles : '')
" 2}}}


" Tags/ctags/omnicomplete (check tagfiles: echo tagfiles()) {{{2

set tags=./.git/tags;,./.tags;,./tags;,~/.vim/.vimtags
Plug 'ludovicchabant/vim-gutentags'
if filereadable("/usr/local/bin/ctags")
    let g:gutentags_ctags_executable = '/usr/local/bin/ctags'
else
    let g:gutentags_ctags_executable = '/usr/bin/ctags'
endif
let g:gutentags_ctags_tagfile = '.tags'
let g:gutentags_generate_on_missing = 0
let g:gutentags_generate_on_new = 0
" }}}


Plug 'mbbill/undotree' " {{{
" nnoremap <leader>u :UndotreeToggle<CR>
let g:undotree_WindowLayout = 2
let g:undotree_SetFocusWhenToggle = 1
" }}}


Plug 'tpope/vim-commentary'   " {{{
" Plug 'al3xandru/nerdcommenter'
" Plug 'scrooloose/nerdcommenter'
" Plug 'tomtom/tcomment_vim'
" }}}


Plug 'w0rp/ale'   " {{{
let g:ale_enabled = 1
let g:ale_completion_delay = 250
let g:ale_completion_enabled = 0
let g:ale_echo_delay = 100
let g:ale_lint_delay = 500
let g:ale_lint_on_enter = 1
let g:ale_lint_on_insert_leave = 0
let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 'never'
let g:ale_list_window_size = 10
let g:ale_open_list = 'on_save'
" let g:ale_set_balloons = 1
let g:ale_set_highlights = 1
let g:ale_set_loclist = 1
let g:ale_set_signs = 1
let g:ale_sign_error = 'E'
let g:ale_sign_info = 'I'
let g:ale_sign_warning = 'W'
let g:ale_sign_style_error = 'ES'
let g:ale_sign_style_warning = 'WS'
let g:ale_linters = {
    \ 'go': ['gometalinter'],
    \ 'python': ['pyflakes',  'pylint'],
    \ }
let g:ale_go_gometalinter_options = '--no-config --disable-all --aggregate --enable=errcheck --enable=golint --enable=gofmt --enable=vet --enable=goimports'
let g:ale_python_pylint_executable = expand('~/.pyenv/versions/neovim2/bin/python')
let g:ale_python_pylint_options = '-m pylint'
let g:ale_python_pyflakes_executable = expand('~/.penv/versions/neovim2/bin/pyflakes')
let g:ale_python_pyflakes_options = '-m pyflakes'
" Alternatives:
" Plug 'scrooloose/syntastic'
" Plug 'maralla/validator.vim'
" }}}


" Search: Ack and Ag {{{2
Plug 'wincent/ferret' " asycn!!!
let g:FerretExecutable='rg,ag,ack'
let g:FerretMap=0
" Possible replacements:
" Plug 'mileszs/ack.vim'
" Plug 'rking/ag.vim'
" Plug 'dyng/ctrlsf.vim'
" Plug 'mhinz/vim-grepper'
" (N)Mappings: ack {{{3
" Using the_silver_searcher to look for word under cursor in current dir
" when using with Ferret there're no quotes
" nnoremap /fa :Ack \b<C-r><C-w>\b <C-r>=getcwd()<CR> --smart-case --type <C-r>=&filetype<CR>
" nnoremap /fA :Ack --<C-r>=&filetype<CR> \b<C-r><C-w>\b --smart-case <C-r>=expand("%:p:h")<CR>

" 3}}}
"}}}


" Snippets {{{2
Plug 'Shougo/neosnippet.vim'
Plug 'Shougo/neosnippet-snippets' "{{{
imap <C-e> <Plug>(neosnippet_expand_or_jump)
smap <C-e> <Plug>(neosnippet_expand_or_jump)
xmap <C-e> <Plug>(neosnippet_expand_target)
let g:neosnippet#snippets_directory=expand("~/.vim/xsnippets/neosnippets")
" let g:neosnippet#expand_word_boundary=0
" }}}
" }}}
" }}}


" Language support {{{1
"
Plug 'sheerun/vim-polyglot' "{{{
let g:polyglot_disabled = ['markdown']
" replacing for now applescript, fatih/vim-go, pangloss/vim-javascript,  hdima/python-syntax
" }}}

" Plug 'applescript.vim'


" Plug 'VimClojure'
let vimclojure#SetupKeyMap = 0


" C/C++ 
" Plug 'OmniCppComplete'


" Go {{{2
" Plug 'fatih/vim-go'
" let g:go_bin_path = expand("~/.golang")
let g:go_echo_command_info = 0
let g:go_fmt_autosave = 1
let g:go_fmt_command = 'goimports'      " alternative gofmt
let g:go_fmt_fail_silently = 0
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_list_type = 'locationlist'
let g:go_metalinter_autosave = 1
let g:go_metalinter_autosave_enabled = ['vet', 'golint', 'errcheck']
let g:go_list_type_commands = {"GoMetalinter": "quickfix"}
let g:tagbar_type_go = {
    \ 'ctagstype' : 'go',
    \ 'kinds'     : [
        \ 'p:package',
        \ 'i:imports:1',
        \ 'c:constants',
        \ 'v:variables',
        \ 't:types',
        \ 'n:interfaces',
        \ 'w:fields',
        \ 'e:embedded',
        \ 'm:methods',
        \ 'r:constructor',
        \ 'f:functions'
    \ ],
    \ 'sro' : '.',
    \ 'kind2scope' : {
        \ 't' : 'ctype',
        \ 'n' : 'ntype'
    \ },
    \ 'scope2kind' : {
        \ 'ctype' : 't',
        \ 'ntype' : 'n'
    \ },
    \ 'ctagsbin'  : 'gotags',
    \ 'ctagsargs' : '-sort -silent'
\ }
augroup go
    autocmd!
    autocmd FileType go nmap <localleader>gs <Plug>(go-implements)
    autocmd FileType go nmap <localleader>gi <Plug>(go-info)
    autocmd FileType go nmap <localleader>gh <Plug>(go-doc)
    autocmd FileType go nmap <localleader>ghv <Plug>(go-doc-vertical)
    autocmd FileType go nmap <localleader>ghb <Plug>(go-doc-browser)
    autocmd FileType go nmap <localleader>ger <Plug>(go-run)
    autocmd FileType go nmap <localleader>geb <Plug>(go-build)
    autocmd FileType go nmap <localleader>get <Plug>(go-test)
    autocmd FileType go nmap <localleader>gec <Plug>(go-coverage)
    " autocmd BufWritePost *.go GoMetaLinter
    autocmd BufNewFile,BufRead *.toml setlocal ts=2 sts=2 sw=2 expandtab
augroup END
"}}}


" HTML Zen Coding
Plug 'mattn/emmet-vim'
let g:user_emmet_install_global=0
augroup emmet
    autocmd!
    autocmd FileType html,css EmmetInstall
augroup END
let g:user_emmet_leader_key='<C-E>'


" Java completion
" Plug 'VictorDenisov/javacomplete'
Plug 'artur-shaik/vim-javacomplete2'


" Javascript
" Plug 'pangloss/vim-javascript'


" Asciidoc {{{2
" Plug 'dahu/vim-asciidoc'
" Plug 'dahu/vimple'
" Plug 'dahu/Asif'
" Plug 'Raimondi/VimRegStyle'
" let g:asciidoc_title_style = 'setext'
" augroup asciidoc
"     autocmd!
"     autocmd BufNewFile,BufRead *.adoc.txt setfiletype asciidoc syntax=asciidoc
" augroup END
"}}}


" Markdown {{{2
augroup markdown
    autocmd!
    autocmd FileType markdown setlocal textwidth=80 wrap foldenable "linebreak
    autocmd FileType markdown nnoremap <buffer><silent> <localleader>me :call <SID>IAWriter()<CR>
    autocmd FileType markdown nnoremap <buffer><silent> <localleader>mg :! $HOME/bin/emarkdown --format=1 <C-R>=expand("%:p")<CR> \| pbcopy<CR>
    autocmd FileType markdown nnoremap <buffer><silent> <localleader>mp :! $HOME/bin/pubmarkup.sh -a vim <C-R>=expand("%:p")<CR><CR>
    " Paste clipboard as blockquote
    autocmd FileType markdown nnoremap <buffer><silent> <localleader>bq p`]gw`[V`[:s/^/> /g<CR>:nohlsearch<CR>o
    " visual selection to blockquote
    autocmd FileType markdown vmap <buffer> bq gq$v`<:s/^/> /g<CR>:nohlsearch<CR>
    " https://sts10.github.io/post/2015-08-02-markdwon-hyperlink-remap-for-vim/
    autocmd FileType markdown vnoremap <buffer> il <esc>`<i[<esc>`>a](<esc>"+]pa) <esc>
augroup END

Plug 'plasticboy/vim-markdown', {'as': 'plasticboy-vim-markdown'} 
set conceallevel=2
let g:vim_markdown_conceal=1
let g:vim_markdown_folding_disabled=0
let g:vim_markdown_folding_level=1
let g:vim_markdown_follow_anchor=0 "use get to jump to anchors
let g:vim_markdown_strikethrough=1
" let g:vim_markdown_no_default_key_mappings=1
nmap <Plug> <Plug>Markdown_MoveToCurHeader
vmap <Plug> <Plug>Markdown_MoveToCurHeader
" augroup markdown_plasticboy
"     autocmd!
"     autocmd FileType markdown nnoremap <buffer><silent> <leader>t :Toc<CR>:q<CR>:lop<CR> 
" augroup END


" Plug 'SidOfc/mkdx'
" let g:mkdx#settings = { 
"     \ 'fold': {'enable': 1, 'components': ['toc']},
"     \ 'highlight': {'enable': 0},
"     \ 'map': {'prefix': '<localleader>'},
"     \ 'tokens': {'fence': '`', 'italic': '*'}
" \ }

" Markdown preview {{{3
" Plug 'greyblake/vim-preview' could not get it to work
Plug 'JamshedVesuna/vim-markdown-preview'
let vim_markdown_preview_toggle=1
let vim_markdown_preview_github=0
let vim_markdown_preview_perl=0
let vim_markdown_preview_pandoc=0
" let vim_markdown_preview_hotkey='<localleader>mp'
augroup markdown_jamshedvesuna
    autocmd FileType markdown nnoremap <buffer><silent> <localleader>mv :call Vim_Markdown_Preview()<CR>
augroup END
"}}}

" Markdown editing {{{3
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
let g:limelight_default_coefficient = 0.7
let g:limelight_paragraph_span = 1
" nmap [of :Limelight<CR>
" nmap ]of :Limelight!<CR>
" nmap cof :Limelight!!<CR>


Plug 'reedes/vim-colors-pencil'
let g:pencil_higher_contrast_ui = 1
let g:pencil_neutral_code_bg = 1
let g:pencil_terminal_italics = 1


let g:iawriter_active = 0
let g:iawriter_save_colorscheme = ""
let g:iawriter_save_bgr = ""
function! <SID>IAWriter()
    if g:iawriter_active == 0
        let g:iawriter_save_colorscheme = g:colors_name
        if exists( "&background" )
            let g:iawriter_save_bgr = &background
        endif
        setlocal spell
        setlocal linespace=3
        setlocal background=light
        setlocal gfn=Operator_Mono:h14
        colorscheme pencil
        Goyo
        Limelight0.7
        " call goyo#execute(0, '')
        " augroup lineno
        "     autocmd!
        "     autocmd FocusLost *.{mk,markdown,mdown,mkdn,mkd,rst}   setlocal norelativenumber nonumber
        "     autocmd InsertEnter *.{mk,markdown,mdown,mkdn,mkd,rst} setlocal norelativenumber nonumber
        "     autocmd InsertLeave *.{mk,markdown,mdown,mkdn,mkd,rst} setlocal norelativenumber nonumber
        " augroup END
        let g:iawriter_active = 1
        echom "IAWriter activated [new: pencil, old: " .  g:iawriter_save_colorscheme . ", background:" . g:iawriter_save_bgr . "]"
    else
        " augroup lineno
        "     autocmd!
        "     autocmd FocusLost * setlocal norelativenumber number
        "     autocmd InsertEnter * setlocal norelativenumber number
        "     autocmd InsertLeave * setlocal relativenumber number
        " augroup END
        " call goyo#execute(0, '') 
        Limelight!
        Goyo
        execute printf("colorscheme %s", g:iawriter_save_colorscheme)
        if g:iawriter_save_bgr
            execute printf("set background=%s", g:iawriter_save_bgr)
        endif
        setlocal gfn=Operator_Mono:h12
        setlocal linespace=1
        setlocal nospell
        let g:iawriter_active = 0
        echom "IAWriter deactivated [new: " . g:iawriter_save_colorscheme . ", old: pencil, background:" . g:iawriter_save_bgr . "]"
    endif
endfunction

"}}}
"}}}


" Node.js https://github.com/joyent/node/wiki/Vim-Plugins


" Python {{{2
" Plug 'lambdalisue/vim-pyenv'
" Plug 'hdima/python-syntax' " disabled with sheerun/vim-polyglot
Plug 'klen/python-mode'
let g:pymode_doc = 1
let g:pymode_doc_bind = 'K'
let g:pymode_folding = 1
let g:pymode_indent = 1
let g:pymode_motion = 1
let g:pymode_options_max_line_length=99
let g:pymode_lint = 0
let g:pymode_lint_checkers = ['pyflakes', 'pep8', 'mccabe']
let g:pymode_lint_ignore = ["E501"]
let g:pymode_syntax_slow_sync = 0
let g:pymode_virtualenv = 1
let g:pymode_run = 0
" let g:pymode_run_bind = '<localleader>ger'
let g:pymode_breakpoint = 1
let g:pymode_breakpoint_bind = '<localleader>gb'
let g:pymode_rope = 0
let g:pymode_rope_completion = 0
let g:pymode_rope_complete_on_dot = 0

Plug 'davidhalter/jedi-vim'
let g:jedi#auto_initialization = 1
let g:jedi#completions_enabled = 1
let g:jedi#completions_command = "<C-Space>"
let g:jedi#use_splits_not_buffers = "bottom"
let g:jedi#popup_on_dot = 0
let g:jedi#show_call_signatures = 2 " 2: show in command line instead of popup
" bindings
let g:jedi#goto_assignments_command = "<localleader>ga"
let g:jedi#goto_definitions_command = "<localleader>gd"
let g:jedi#goto_command="<localleader>gd"
let g:jedi#documentation_command = "<localleader>gh"
let g:jedi#usages_command = "<localleader>gu"
let g:jedi#rename_command = "<localleader>gr"

" https://github.com/macvim-dev/macvim/issues/386
" if !has('gui') ||  !has('mac')
" endif
" if !has('gui') || !has('mac')
" endif
"}}}


" Plug 'derekwyatt/vim-scala'


" Plug 'keith/swift.vim'


Plug 'digitalrounin/vim-yaml-folds'
" }}}


" Enhancements {{{1
" Improve matching chars, parentheses {{{2
Plug 'kien/rainbow_parentheses.vim' " {{{3
augroup rainbowpar
    autocmd!
    autocmd VimEnter * RainbowParenthesesToggle
    autocmd Syntax * RainbowParenthesesLoadRound
    autocmd Syntax * RainbowParenthesesLoadSquare
    autocmd Syntax * RainbowParenthesesLoadBraces
augroup END
" 3}}}
" 2}}}

" Startup buffer {{{2
Plug 'mhinz/vim-startify'
let g:startify_lists = [
    \ {'type': 'bookmarks', 'header': ['   Bookmarks']},
    \ {'type': 'dir', 'header': ['   MRU in ' .  getcwd()]},
    \ {'type': 'files', 'header': ['   Recent files']},
    \ {'type': 'sessions', 'header': ['   Sessions']},
    \ ]
let g:startify_bookmarks = [
    \ '~/Dropbox/Dox/mydox/myjrnl/',
    \ '~/Dropbox/Dox/mydox/myjrnl/02-thoughts.md',
    \ '~/Dropbox/Dox/mydox/myjrnl/03-email_drafts.md',
    \ '~/Dropbox/Dox/mydox/myjrnl/04-blog.md',
    \ '~/Dropbox/Dox/mydox/myjrnl/05-blog_ideas_and_drafts.md',
    \ '/Users/Shared/homepage/h.html',
    \]
let g:startify_files_number = 10
let g:startify_session_autoload = 1
let g:startify_session_dir = '~/.sessions/vim'
let g:startify_skiplist = [
    \ '.git/.*',
    \ '.hg/.*',
    \ ]
let g:startify_change_to_dir = 1
let g:startify_change_to_vcs_root = 0
"}}}

" CamelCase {{{2
" Plug 'camelcasemotion'
" Plug 'kana/vim-smartword'
Plug 'bkad/CamelCaseMotion' 
" Ver.1: Alt - w/b/3/g => ∑/∫/£/© {{{3
" map ∑ <Plug>CamelCaseMotion_w
" map ∫ <Plug>CamelCaseMotion_b
" map £ <Plug>CamelCaseMotion_e
" map © <Plug>CamelCaseMotion_ge
" omap <silent> i∑ <Plug>CamelCaseMotion_iw
" xmap <silent> i∑ <Plug>CamelCaseMotion_iw
" omap <silent> i∫ <Plug>CamelCaseMotion_ib
" xmap <silent> i∫ <Plug>CamelCaseMotion_ib
" omap <silent> i£ <Plug>CamelCaseMotion_ie
" xmap <silent> i£ <Plug>CamelCaseMotion_ie
" end ver.1 3}}}
" Ver.2: w -> CamelCase, orig w -> W, orig.W -> Alt+w {{{3
" these remaps work if I want to
" noremap ∑ W
" noremap ∫ B
" noremap £ E
" noremap © gE
" noremap W w
" noremap B b
" noremap E e
" noremap gE ge
" map w <Plug>CamelCaseMotion_w
" map b <Plug>CamelCaseMotion_b
" map e <Plug>CamelCaseMotion_e
" map ge <Plug>CamelCaseMotion_ge
" end ver.2}}}
" Ver.3
map ]w <Plug>CamelCaseMotion_w
map [w <Plug>CamelCaseMotion_e
map ]W <Plug>CamelCaseMotion_b
map [W <Plug>CamelCaseMotion_ge
" 2}}}


Plug 'yuttie/comfortable-motion.vim'  " smoother scrolling physics


" tpope extensions: surround, unimpaired {{{
Plug 'tpope/vim-surround'
" }}}


" Dash.app {{{
Plug 'rizzatti/funcoo.vim'
Plug 'rizzatti/dash.vim'
" noremap does *not* work with <Plug>
" nmap <leader>k <Plug>DashSearch
" }}}


" Git {{{2
Plug 'tpope/vim-fugitive'
"2}}}
"1}}}


" Experimental {{{1
"}}}

call plug#end()

" colorscheme {{{

" default colorscheme
colorscheme evening " morning zellner

" THEME command: custom colorscheme groups {{{2
" Color scheme sites:
" http://vimcolors.com/
" http://colorswat.ch/
let s:cs_dark = "(rich) onedark nord nova (vimdefault) desert256 molokai dante koehler vividchalk vibrantink molokai tango fnaqeran motus railcast tir_black inkpot"
let s:cs_light = "(rich) ironman nuvola gruvbox (vimdefault) morning zellner (misc) cake16 sweater--- simpleandfriendly summerfruit256 calmbreeze autumnleaf buttercream navajo papayawhip pyte (disabled)"
let s:cs_pastel = "(rich) earendel gruvbox (misc) alduin lucius --- jellybeans railcast2 tango2 wombat wombat256 wombat256mod camo (disabled) kolor"

function! <SID>ChooseColorscheme(args)
    let l:cslist = []
    if len(a:args) == 0
        echo 'Usage: :THEME [all|dark|light|paster]'
        return
    elseif a:args == 'all'
        let paths = split(globpath(&runtimepath, 'colors/*vim'), "\n")
        let l:cslist = map(paths, 'fnamemodify(v:val, ":t:r")')
        echo 'List of all installed colorschemes:'
        echo join(l:cslist, "\n")
        return
    elseif a:args == 'light'
        let l:cslist = split(s:cs_light)
        echo 'Light colorschemes:'
    elseif a:args == 'pastel'
        let l:cslist = split(s:cs_pastel)
        echo 'Pastel colorschemes:'
    elseif a:args == 'dark'
        let l:cslist = split(s:cs_dark)
        echo 'Dark colorschemes:'
    endif
    echo join(l:cslist, "\n")
    echo "\n"
    execute "colorscheme " . input("Choice: ", "", "color")
endfunction
command! -nargs=* THEME call <SID>ChooseColorscheme('<args>')
" }}}

function! s:RandomColorschemeFromList(list)
    let m = len(a:list)
    let tmstmp = str2nr(strftime('%s'))
    let selIdx = float2nr(fmod(tmstmp, m))
    " echom "Timestamp:" .  tmstmp . "; len(list):" . string(m) . "; selected idx:" . string(selIdx)
    let selColorscheme = a:list[selIdx]
    " echom "Timestamp:" .  tmstmp . "; len(list):" . string(m) . "; colorscheme :" . selColorscheme
    return selColorscheme
endfunction

" set colorscheme based on vim flavor {{{2
if has("unix")
    " use if needed to determine the os
    " or use has("mac")
    " let s:uname = system("uname -s")

    if &diff 
        colorscheme ironman
    elseif has("gui_running")
        let color_schemes = ['ironman', 'nuvola', 'gruvbox', 'papercolor', 'cake16', 'sweater', 'plan9', 'github', 'earendel', 'onedark', 'nord', 'nova', 'ironman', 'nuvola', 'gruvbox', 'papercolor']
        let cs = s:RandomColorschemeFromList(color_schemes)
        if cs == 'gruvbox'
            set background=light
        elseif cs == 'papercolor'
            set background=light
        endif

        execute ":colorscheme " .  cs
    elseif has("gui_vimr")
        colorscheme alduin
    elseif has('nvim')
        " colorscheme gruvbox
        let color_schemes = ['ironman', 'gruvbox', 'papercolor', 'onedark','nord', 'alduin']
        let cs = s:RandomColorschemeFromList(color_schemes)
        execute ":colorscheme " .  cs
    else
        " colorscheme gruvbox
        let color_schemes = ['ironman', 'gruvbox', 'papercolor', 'onedark','nord', 'alduin']
        let cs = s:RandomColorschemeFromList(color_schemes)
        execute ":colorscheme " .  cs
    endif
endif
" }}}

" color column & cursor line {{{2
function! <SID>SetColorColumn()
    " highlight ColorColumn ctermbg=235 guibg=#2c2d27
    set colorcolumn=88,101,121,161
    " Following setting colors all columns after 121
    " let &colorcolumn="81,".join(range(121,999),",")
endfunction

" http://vim.wikia.com/wiki/Highlight_current_line
set cursorline
augroup CursorLine
    autocmd!
    autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline
    autocmd WinLeave * setlocal nocursorline
augroup END
function! <SID>SetCursorLineColors()
    hi CursorLine    ctermbg=52 guibg=#424242
    hi CursorLineNr  term=bold ctermfg=226 gui=bold guifg=#ffff00
endfunction

"call <SID>SetCursorLineColors()
call <SID>SetColorColumn()
" }}}

" }}}


" * * * * * * * * * * * * * * * * * * * * * * * * * *
"
" Credits:
"
" http://blog.danielfischer.com/2010/11/19/a-starting-guide-to-vim-from-textmate/
" http://items.sjbach.com/319/configuring-vim-right
" https://github.com/tpope/vim-pathogen
" http://dougireton.com/blog/2013/02/23/layout-your-vimrc-like-a-boss/
" 
" vim: set foldlevel=0 foldmethod=marker :
