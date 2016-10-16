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

" 1. important 
set nocompatible
set backspace=2
set backspace=indent,eol,start  " allow backspace in insert mode
" Use the OS clipboard by default (on versions compiled with `+clipboard`)
if has('clipboard')
    if has('unnamedplus')
        set clipboard=unnamed,unnamedplus
    else
        set clipboard=unnamed
    endif
endif
set esckeys     " allow cursor keys in insert mode
set hidden      " allow buffer switching without saving
set lazyredraw
set modeline
set modelines=5
set mouse=a
set mousehide
set sessionoptions-=options
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


" autosave on focus lost
autocmd FocusLost, BufLeave * silent! :wall


" 2. moving around, searching and patterns "
set incsearch
set hlsearch
set ignorecase
set smartcase
set showmatch
set matchtime=3     " tenths of a second to show the matching paren
set scrolloff=5

" 3. tags
" omnicomplete {{{
set omnifunc=syntaxcomplete#Complete
" https://github.com/sjl/dotfiles/blob/eea18b00b8c74943f5094fddf91d3c2a7e0a7242/vim/vimrc#L534
" kspell: dictionary completion only when spell enabled set spell 
set complete=.,w,b,u,t,i,kspell
" http://vim.wikia.com/wiki/Make_Vim_completion_popup_menu_work_just_like_in_an_IDE
set completeopt=longest,menu,preview
" http://stackoverflow.com/questions/7722177/how-do-i-map-ctrl-x-ctrl-o-to-ctrl-space-in-terminal-vim
inoremap <C-Space> <C-x><C-o>
inoremap <C-@> <C-Space>
" }}}

" 4. displaying text
filetype on
filetype plugin on
filetype plugin indent on

set synmaxcol=500
set listchars=tab:▸\ ,trail:·,eol:¬
set showbreak=↪
" set showbreak=⤿

" line numbers {{{
" http://jeffkreeftmeijer.com/2012/relative-line-numbers-in-vim-for-super-fast-movement/
function! <SID>ToggleLineNo()
    if(&relativenumber == 1)
        set norelativenumber number
    else
        set number relativenumber
    endif
endfunction
" mappings inspired by vim-unimpaired
nmap [o- :set number relativenumber<CR>
nmap ]o- :set norelativenumber number<CR>
nmap <silent>co- :call <SID>ToggleLineNo()<CR>
map <silent><C-_> <esc>:call <SID>ToggleLineNo()<CR>
" nnoremap <silent><C-n> :call <SID>ToggleLineNo()<cr>
augroup lineno
    autocmd!
    autocmd FocusLost * set norelativenumber number
    autocmd InsertEnter * set norelativenumber number
    autocmd InsertLeave * set relativenumber number
    autocmd Filetype qf setlocal norelativenumber number nowrap
augroup END
set number
set relativenumber
" }}}


" 5. syntax, highlighting and spelling"
syntax on

" colorscheme {{{
colorscheme koehler
" http://vimcolors.com/
let s:cs_dark = "desert256 molokai dante koehler vividchalk vibrantink molokai tango fnaqeran motus railcast tir_black inkpot"
let s:cs_light = "simpleandfriendly sweater summerfruit256 autumnleaf buttercream calmbreeze navajo morning papayawhip pyte gruvbox cake16 lucius ironman nuvola" "eclipse
let s:cs_pastel = "alduin jellybeans tango2 wombat wombat256 wombat256mod railcast2 camo earendel flattown lucius  kolor gruvbox" "nefertiti

function! <SID>ChooseColorscheme(args)
    let cslist = []
    if len(a:args) == 0
        echo 'Usage: :THEME [all|dark|light|paster]'
        return
    elseif a:args == 'all'
        let paths = split(globpath(&runtimepath, 'colors/*vim'), "\n")
        let cslist = map(paths, 'fnamemodify(v:val, ":t:r")')
        echo 'List of all installed colorschemes:'
        echo join(cslist, "\n")
        return
    elseif a:args == 'light'
        let cslist = split(s:cs_light)
        echo 'Light colorschemes:'
    elseif a:args == 'pastel'
        let cslist = split(s:cs_pastel)
        echo 'Pastel colorschemes:'
    elseif a:args == 'dark'
        let cslist = split(s:cs_dark)
        echo 'Dark colorschemes:'
    endif
    echo join(cslist, "\n")
    echo "\n"
    execute "colorscheme " . input("Choice: ", "", "color")
endfunction
command! -nargs=* THEME call <SID>ChooseColorscheme('<args>')
" }}}

" color column & cursor line {{{
function! <SID>SetColorColumn()
    " highlight ColorColumn ctermbg=235 guibg=#2c2d27
    set colorcolumn=81,161
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
" }}}

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
augroup END

" 6. multiple windows "
set title
set laststatus=2
set statusline=%t\ %l,%v%=[b%n\ %L:%p%%\ %y]%<\ [a\%03.3b:h\%02.2B]
" hi StatusLine ctermbg=59 ctermfg=69 

" GUI {{{
set guicursor=n-v-c:block-Cursor,ve:ver35-Cursor,o:hor50-Cursor,i-ci:ver25-lCursor,r-cr:hor20-Cursor,sm:block
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

    set columns=165
    set lines=80
endif
" augroup VimTransparency
"     autocmd!
"     autocmd FocusGained * set transparency=0
"     autocmd FocusLost * set transparency=25
" augroup END
" }}}

" splits
set splitbelow
set splitright
"
" automatically open the location/quickfix window after :make, :grep,
" :lvimgrep and friends if there are valid locations/errors
"augroup qf
    "autocmd!
    "autocmd QuickFixCmdPost [^l]* cwindow
    "autocmd QuickFixCmdPost l* lwindow
"augroup END

" 11.messages and info
set ruler
set visualbell t_vb=


" 14. tabs and indenting {{{
set expandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4
set smartindent
set autoindent
" }}}


" 15. folding {{{
set foldenable
set foldmethod=indent
set foldnestmax=10
set foldlevel=100
"HTML folding tag
nnoremap <leader>zha Vatzf
" }}}


" 16 diff mode
set diffopt=filler,vertical


" 17. mappings {{{
set timeoutlen=750

" abbreviations
cnoreabbrev W w
cnoreabbrev Q q


" Change mapleader from <Leader> = \
let mapleader="\<space>"
let maplocalleader="\\"

inoremap jk <esc>
inoremap <silent> <Up> <esc><Up>
inoremap <silent> <Down> <esc><Down>
inoremap <silent> <Left> <esc><Left>
inoremap <silent> <Right> <esc><Right>
" inoremap <Down> <C-o>gj
" inoremap <Up> <C-o>gk

" make vertical line nav better http://stackoverflow.com/questions/20975928/moving-the-cursor-through-long-soft-wrapped-lines-in-vim/21000307#21000307
nnoremap <expr> k (v:count == 0 ? 'gk' : 'k')
nnoremap <expr> j (v:count == 0 ? 'gj' : 'j')
nnoremap <Up> gk
nnoremap <Down> gj
" make ; behave like : (save the Shift)
nnoremap ; :
" nnoremap , ;
nnoremap … ;    " Alt+;
nnoremap <tab> %
vnoremap <tab> %

" blip for next highlight word and center the line {{{
" http://vi.stackexchange.com/questions/2761/set-cursor-colour-different-when-on-a-highlighted-word
" Plugin 'timakro/vim-searchant'
nnoremap <silent> n nzzzv:call <SID>HLNext(0.6)<CR>
nnoremap <silent> N Nzzzv:call <SID>HLNext(0.6)<cr>

function! <SID>HLNext (blinktime)
    let target_pat = '\c\%#'.@/
    let ring = matchadd('ErrorMsg', target_pat, 101)
    redraw
    exec 'sleep ' . float2nr(a:blinktime * 1000) . 'm'
    call matchdelete(ring)
    redraw
endfunction
" }}}

" Show special characters
nmap <silent><leader>hc :set nolist!<CR>
" Disable highlighted search 
nnoremap <silent><leader>hh :nohlsearch<CR>
" Highlight matches http://www.jeffcomput.es/posts/2016/02/vim-tips-helpful-leader-key-commands/
" case sensitive, partial match inclusive
nnoremap <silent><leader>hi :set hlsearch<CR>:let @/='<C-r><C-w>'<CR>
" case sensitive, no partial match
nnoremap <silent><leader>ho :set hlsearch<CR>:let @/='\<<C-r><C-w>\>'<CR>

" Inserts the path of the currently edited file into a command
cnoremap <C-P> <C-R>=expand("%:p:h") . "/" <CR>
" Save current session
function! <SID>SaveSession () 
    let parentDir = getcwd()
    exec "mksession! " . parentDir . "/.session.vim"
    echo "session saved " . parentDir . "/.session.vim"
    " if isdirectory(parentDir . "/.git")
    "     exec "mksession! " . parentDir . "/.git/.session.vim"
    "     echo "session saved " . parentDir . "/.git/.session.vim"
    " else
    "     exec "mksession! " . parentDir . "/.session.vim"
    "     echo "session saved " . parentDir . "/.session.vim"
    " endif
endfunction
cnoremap <C-S> <C-R> call <SID>SaveSession()<CR><CR>
"cnoremap <C-S> <C-R>="mksession! " . getcwd() . "/.session.vim" <CR>
"cnoremap <C-S> <C-R>="mksession! " . expand("%:p:h") . "/.session.vim" <CR>

" Insert a newline in normal mode
nnoremap <S-Enter> O<Esc>
nnoremap <CR> o<Esc>
nnoremap <NL> i<CR><Esc> " Ctrl-j: break the line at cursor


" Buffer switch
nmap <C-a> :bNext<CR>
nmap <C-e> :e#<CR>
" Tab switch
nnoremap <silent><C-S-tab> :silent tabprevious<CR>
nnoremap <silent><C-tab> :silent tabnext<CR>
inoremap <silent><C-S-tab> <ESC>:tabprevious<CR>
inoremap <silent><C-tab> <ESC>:tabnext<CR>
nnoremap <silent><leader>gt :<C-u>tabs<CR>:tabn<space>
" open tag in tab
nnoremap <C-\> <C-w><C-]><C-w>T
inoremap <C-\> <C-w><C-]><C-w>T
" Alt+] on OS X
nnoremap ‘ <C-w><C-]><C-w>T
inoremap ‘ <C-w><C-]><C-w>T

" Window resizing
nmap <silent><C-w>< :vertical resize -10<CR>
nmap <silent><C-w>> :vertical resize +10<CR>
nmap <silent><C-w>- :resize -10<CR>
nmap <silent><C-w>+ :resize +10<CR>

" selections
" reselect pasted text
nnoremap <leader>v V`] 
" https://github.com/henrik/dotfiles/blob/master/vim/config/mappings.vim#L22
" select the text that was last edited/pasted
" http://vimcasts.org/episodes/bubbling-text/
nmap gV `[v`]
" keep selection when indenting
vnoremap > >gv
vnoremap < <gv

" Opens an edit command with the path of the currently edited file filled in
nnoremap <leader>ef :e <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>et :tabe <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>es :split <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>ev :vsplit <C-R>=expand("%:p:h") . "/" <CR>

" bind f and F to perform grep for the word under cursor
" grep results go into quicklist: copen/cclose
nnoremap /fg :grep! -R '<C-r><C-w>' <C-r>=getcwd()<CR>
nnoremap /fG :grep -R '<C-r><C-w>' <C-r>=expand("%:p:h")<CR>
"nnoremap <leader>F :vimgrep! /\C\<<C-r><C-w>\>/gj <C-r>=expand("%:p:h")<CR>
"nnoremap <Leader>F :vimgrep! /\<<C-r><C-w>\>/j

" Using the_silver_searcher to look for word under cursor in current dir
nnoremap /fa :Ag! --<C-r>=&filetype<CR> "\b<C-r><C-w>\b" <C-r>=getcwd()<CR>
nnoremap /fA :Ag! --<C-r>=&filetype<CR> "\b<C-r><C-w>\b" <C-r>=expand("%:p:h")<CR>
" Using Ack to search the word under cursor in the current dir
" nnoremap <Leader>f :Ack! --type=<C-r>=%filetype<CR> "\b<C-r><C-w>\b" <C-r>=expand("%:p:h")<CR>

" bind c[hange] to replace word under cursor
nnoremap <leader>c :%s/\<<C-r><C-w>\>//cg<Left><Left><Left>
nnoremap c* *Ncgn
nnoremap c# #NcgN
nnoremap cg* g*Ncgn
nnoremap cg# g#NcgN
" }}}

" 19. the swap file
set backup
set noswapfile
set undofile
set history=10000
set backupdir=~/.vim/tmp/backup// "~/.vim/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim/tmp/swap// "~/.vim/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set undodir=~/.vim/tmp/undo//

if !isdirectory(expand(&backupdir))
    call mkdir(expand(&backupdir), "p")
endif
if !isdirectory(expand(&directory))
    call mkdir(expand(&directory), "p")
endif
if !isdirectory(expand(&undodir))
    call mkdir(expand(&undodir), "p")
endif

" 24. multi-byte characters
setglobal fileencoding=utf-8
set encoding=utf-8 nobomb

" autocmds
if has("autocmd")
    augroup vimrc
        autocmd!
        " crontab -e
        autocmd BufNewFile,BufRead crontab.* set nobackup | set nowritebackup

        " json is javascript
        autocmd BufNewFile,BufRead *.json setfiletype json syntax=javascript

        autocmd FileType python setlocal ts=4 sts=4 sw=4 expandtab autoindent
        autocmd FileType make setlocal ts=4 sts=4 sw=4 noexpandtab
        autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
        autocmd FileType html setlocal ts=2 sts=2 sw=2 expandtab
        autocmd FileType htm setlocal ts=2 sts=2 sw=2 expandtab
        autocmd FileType java setlocal omnifunc=javacomplete#Complete

        if filereadable(expand("~/.vim/bundle/HTML-AutoCloseTag/ftplugin/html_autoclosetag.vim"))
            autocmd FileType html,htm,xhtml,xml source ~/.vim/bundle/HTML-AutoCloseTag/ftplugin/html_autoclosetag.vim
        end
    augroup END
end



if has('gui') && has('mac')
    if executable("pyenv")
        let cmd = 'pyenv version-name'
        let pyenv=substitute(system(cmd), '[\]\|[[:cntrl:]]', '', 'g')
        " let pyenv=strpart(pyenv, 0, strlen(pyenv)-1)
        let cmd = 'python -c "import sys;vt=sys.version_info;print \"%s.%s.%s\" % (vt[0], vt[1], vt[2])"'
        let pyver=substitute(system(cmd), '[\]\|[[:cntrl:]]', '', 'g')
        let $PYTHONHOME=$HOME . "/.pyenv/versions/" . pyver
        let $PYTHONPATH=$HOME . "/.pyenv/versions/" . pyenv . "/lib/python2.7/site-packages/"
        set pythondll=$PYTHONHOME/lib/libpython2.7.dylib
        " echom "PYENV     :" . pyenv
        " echom "PYTHONVER :" . pyver
        " echom "PYTHONHOME:" . $PYTHONHOME
        " echom "PYTHONPATH:" . $PYTHONPATH
        " echom "PYTHONDLL :" . &pythondll
    endif
endif

" Load Vundle
" Only Plugin settings are allowed until vundle#end()
filetype off " required!

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
" Alternative plugin manager:
" - junegunn/vim-plug 
" - Shougo/neobundle.vim

" Color schemes {{{1
Plugin 'Colour-Sampler-Pack'
Plugin 'AlessandroYorba/Alduin'
let g:alduin_Shout_Aura_Whisper = 1
let g:alduin_Shout_Fire_Breath = 1
Plugin 'blerins/flattown'
Plugin 'jonathanfilip/vim-lucius'
" After enabling: :Lucius[Black|BlackHighContrast|BlackLowContrast|
"   Dark|DarkHighContrast|DarkLowContrast|Light|LightLowContrast|
"   White|WhiteLowContrast]
Plugin 'morhetz/gruvbox'
Plugin 'nice/sweater'
Plugin 'rakr/vim-two-firewatch'
let g:two_firewatch_italics=1
let g:airline_theme='twofirewatch'
Plugin 'zefei/cake16'
Plugin 'zeis/vim-kolor'

" Plugin 'AlessandroYorba/Sierra'
" Plugin 'altercation/vim-colors-solarized'
let g:solarized_termtrans=0
let g:solarized_termcolors=256
let g:solarized_visibility="high"
let g:solarized_contrast="normal"
" Plugin 'colepeters/spacemacs-theme.vim'
" Plugin 'fcpg/vim-fahrenheit'
" Plugin 'jdkanani/vim-material-theme'
" Plugin 'jeetsukumaran/vim-nefertiti'
" Plugin 'mhartington/oceanic-next'
" Plugin 'nanotech/jellybeans.vim'
" Plugin 'NLKNguyen/papercolor-theme'
" Plugin 'rakr/vim-one'
" Plugin 'rodnaph/vim-color-schemes'
" Plugin 'whatyouhide/vim-gotham'
" Plugin '256-grayvim'
" Plugin 'blacklight'
" Plugin 'gregsexton/Atom'
" Plugin 'adampasz/vim-stonewashed'
" }}}


" Extra text objects {{{1
Plugin 'kana/vim-textobj-user'
" Line object: il al
Plugin 'kana/vim-textobj-line'
" Function argument object: i, a,
" Shift: <, >,
" Jump: [, ],
Plugin 'PeterRincker/vim-argumentative'
" Plugin 'argtextobj.vim' 
" Indent object: ii ai aI iI 
Plugin 'michaeljsmith/vim-indent-object' 
" Function call object: am im aM iM
Plugin 'thalesmello/vim-textobj-methodcall'
" Function object: if af iF aF
Plugin 'kana/vim-textobj-function'
Plugin 'kamichidu/vim-textobj-function-go'
Plugin 'thinca/vim-textobj-function-javascript'
Plugin 'thinca/vim-textobj-function-perl'
Plugin 'nelstrom/vim-textobj-rubyblock'
" Comment object: ic ac aC
Plugin 'glts/vim-textobj-comment'
" }}}

"
" Essentials {{{1
"
" Files {{{2
Plugin 'ctrlpvim/ctrlp.vim'
" set runtimepath^=~/.vim/bundle/ctrlp
" let g:loaded_ctrlp = 1
let g:ctrlp_map = '<F7>'
let g:ctrlp_cmd = 'CtrlP'
" let g:ctrlp_by_filename = 0
let g:ctrlp_match_window = 'top,order:ttb,min:1,max:10,results:10'
" only cache if we're over this number of files
let g:ctrlp_use_caching = 2000
nnoremap <unique><leader>f :CtrlP<CR>
nnoremap <unique><leader>] :CtrlPBufTag<CR>
nnoremap <unique><leader>B :CtrlPBuffer<CR>
" https://www.reddit.com/r/vim/comments/4gjbqn/what_tricks_do_you_use_instead_of_popular_plugins/
nnoremap <unique><leader>b :ls<CR>:buffer<space>
" nnoremap gB :ls<CR>:sbuffer<Space>

" Netrw/NERDTree {{{3
" if v:version < 800
    autocmd FileType netrw setlocal bufhidden=delete "wipe
    nnoremap <unique><leader>p :Lex<CR>
    nnoremap <unique><leader>P :Lex <C-R>=expand("%:p:h") . "/"<CR><CR>
    let g:netrw_browse_split=4 " (open in previous window)
    let g:netrw_hide=0
    " let g:netrw_home='~'
    let g:netrw_preview=0 "horizontal
    let g:netrw_alto=0    "aboveleft
    let g:netrw_liststyle=3 " 3: thin long wide tree; 0: thin list
    let g:netrw_winsize=25
    let s:dotfiles = '\(^\|\s\s\)\zs\.\S\+'
    let s:escape = 'substitute(escape(v:val, ".$~"), "*", ".*", "g")'
    let g:netrw_sort_sequence = '[\/]$,*,\%(' . join(map(split(&suffixes, ','), 'escape(v:val, ".*$~")'), '\|') . '\)[*@]\=$'
    let g:netrw_list_hide =
          \ join(map(split(&wildignore, ','), '"^".' . s:escape . '. "$"'), ',') . ',^\.\.\=/\=$' .
          \ (get(g:, 'netrw_list_hide', '')[-strlen(s:dotfiles)-1:-1] ==# s:dotfiles ? ','.s:dotfiles : '')
" else
    " Plugin 'scrooloose/nerdtree'
    " let NERDTreeIgnore=['\.pyc', '\.pyo', '\~$', '\.o$', '\.class$', 
    "     \ '\.egg$', '\.idea$',
    "     \ '\.bzr', '\.git', '\.hg', '\.svn']
    " let NERDChristmasTree=1
    " let NERDTreeBookmarksFile='~/.vim/.NERDTreeBookmarks'
    " let NERDTreeHighlightCursorline=1
    " let NERDTreeQuitOnOpen=1
    " let NERDTreeShowHidden=1
    " let NERDTreeWinPos="left"
    " nnoremap <unique><leader>p :NERDTreeToggle<CR>
    " nnoremap <unique><leader>P :NERDTreeFind<CR>
" endif
" nnoremap <silent> <F8> :NERDTreeToggle<CR>
" nnoremap <silent> <S-F8> :NERDTreeFind<CR>
" }}}
" 2}}}


" Buffers {{{2
" Plugin 'jeetsukumaran/vim-buffergator'
" let g:buffergator_viewport_split_policy = "T"
" let g:buffergator_autodismiss_on_select = 1
" let g:buffergator_suppress_keymaps = 1
" nnoremap <leader>gt :<C-u>BuffergatorTabsToggle<CR>
" nnoremap <leader>gb :<C-u>BuffergatorToggle<CR>
" Replaced by CtrlPBuffer 
" Plugin 'jlanzarotta/bufexplorer'
" }}}


" Tags/ctags/omnicomplete (check tagfiles: echo tagfiles()) {{{2
Plugin 'majutsushi/tagbar'
if filereadable("/usr/local/bin/ctags")
    let g:tagbar_ctags_bin = '/usr/local/bin/ctags'
else
    let g:tagbar_ctags_bin = '/usr/bin/ctags'
endif
let g:tagbar_autoclose = 1
let g:tagbar_show_linenumbers = 0
let g:tagbar_hide_nonpublic = 0
nnoremap <silent><leader>t :TagbarToggle<CR>
" nnoremap <silent> <F9> :TagbarToggle<CR>

set tags=./.git/tags;,./.tags;,./tags;,~/.vim/.vimtags
Plugin 'ludovicchabant/vim-gutentags'
if filereadable("/usr/local/bin/ctags")
    let g:gutentags_ctags_executable = '/usr/local/bin/ctags'
else
    let g:gutentags_ctags_executable = '/usr/bin/ctags'
endif

let g:gutentags_tagfile = '.tags'
let g:gutentags_generate_on_missing = 0
let g:gutentags_generate_on_new = 0

" Plugin 'AndrewRadev/tagfinder.vim'
" augroup TagFinder
"   autocmd!
"   autocmd FileType * DefineTagFinder Class c,class,e,s,t,u
"   autocmd FileType * DefineTagFinder Method f,m,method,F,singleton\ method
" augroup END

" Plugin 'AutoTag'
" Plugin 'xolox/vim-misc'
" Plugin 'xolox/vim-easytags'
if filereadable("/usr/local/bin/ctags")
    let g:easytags_cmd = '/usr/local/bin/ctags'
else
    let g:easytags_cmd = '/usr/bin/ctags'
endif
let g:easytags_async = 1
let g:easytags_dynamic_files = 1
let g:easytags_auto_highlight = 0
let g:easytags_include_members = 1
let g:easytags_updatetime_min = 60000
let g:easytags_syntax_keyword = 'always'  " 'always'  'auto'
" if using Universal ctags
let g:easytags_suppress_ctags_warning = 1
"let g:easytags_languages = {
            "\ 'language': {
            "\   'cmd': g:easytags_cmd,
            "\   'args': [],
            "\   'fileoutput_opt': '-f',
            "\   'stdout_opt': '-f-',
            "\   'recurse_flag': '-R'
            "\ }
"\}

Plugin 'Shougo/neocomplete.vim'
augroup neocomplete
    autocmd!
    autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
    autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
augroup END
" }}}


Plugin 'beloglazov/vim-online-thesaurus'


Plugin 'mbbill/undotree'
nnoremap <leader>u :UndotreeToggle<CR>
let g:undotree_WindowLayout = 2
let g:undotree_SetFocusWhenToggle = 1


Plugin 'junegunn/vim-peekaboo'
let g:peekaboo_delay=800


Plugin 'tpope/vim-commentary'
" Plugin 'al3xandru/nerdcommenter'
" Plugin 'scrooloose/nerdcommenter'
" Plugin 'tomtom/tcomment_vim'


Plugin 'scrooloose/syntastic'
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
let g:syntastic_auto_jump = 0
"let g:syntastic_error_symbol = "✗"
"let g:syntastic_warning_symbol = "⚠"


" Search: Ack and Ag {{{2
Plugin 'mileszs/ack.vim'
Plugin 'rking/ag.vim'
"}}}


" Snippets {{{2
if has("python")
    Plugin 'SirVer/ultisnips'
else
    Plugin 'MarcWeber/vim-addon-mw-utils'
    Plugin 'tomtom/tlib_vim'
    Plugin 'garbas/vim-snipmate'
endif
Plugin 'honza/vim-snippets'
"}}}
"}}}

"
" Language support {{{1
"
Plugin 'applescript.vim'


Plugin 'VimClojure'
let vimclojure#SetupKeyMap = 0


" C/C++ 
Plugin 'OmniCppComplete'


" Go {{{2
Plugin 'fatih/vim-go'
let g:go_bin_path = expand("~/.golang")
let g:go_fmt_autosave = 1
let g:go_fmt_command = 'gofmt'
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
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
augroup END
"}}}


" HTML Zen Coding
Plugin 'mattn/emmet-vim'
let g:user_emmer_install_global=0
augroup emmet
    autocmd!
    autocmd FileType html,css EmmetInstall
augroup END
let g:user_emmet_leader_key='<C-X>'


" Java completion
" Plugin 'VictorDenisov/javacomplete'
Plugin 'artur-shaik/vim-javacomplete2'


" Markdown {{{2
Plugin 'plasticboy/vim-markdown', {'name': 'plasticboy-vim-markdown'}
let g:vim_markdown_folding_disabled=0
let g:vim_markdown_folding_level=1

" Markdown preview {{{3
"Plugin 'greyblake/vim-preview' could not get it to work
function! <SID>MarkdownPreview(file)
    if has("unix")
        let l:uname = system("uname -s")
        if l:uname =~ "Darwin"
            if filereadable("/Applications/Marked\ 2.app/Contents/Info.plist")
                execute ":silent !open -a 'Marked 2.app' '" . a:file . "'"
                " nnoremap <silent><localleader>mp :silent !open -a 'Marked 2.app' '%:p'<CR>
            else
                execute ":silent !open -a Google Chrome.app' '" . a:file . "'"
                " nnoremap <silent><localleader>mp :silent !open -a 'Google Chrome.app' '%:p'<CR>
            endif
        endif
    endif
endfunction
"}}}
" Markdown editing {{{3
Plugin 'junegunn/goyo.vim'
Plugin 'junegunn/limelight.vim'
let g:limelight_default_coefficient = 0.7
let g:limelight_paragraph_span = 1
nmap [of :Limelight<CR>
nmap ]of :Limelight!<CR>
nmap cof :Limelight!!<CR>


Plugin 'reedes/vim-colors-pencil'
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
        augroup lineno
            autocmd!
            autocmd FocusLost *.{mk,markdown,mdown,mkdn,mkd,rst}   setlocal norelativenumber nonumber
            autocmd InsertEnter *.{mk,markdown,mdown,mkdn,mkd,rst} setlocal norelativenumber nonumber
            autocmd InsertLeave *.{mk,markdown,mdown,mkdn,mkd,rst} setlocal norelativenumber nonumber
        augroup END
        let g:iawriter_active = 1
        echom "IAWriter activated [new: pencil, old: " .  g:iawriter_save_colorscheme . ", background:" . g:iawriter_save_bgr . "]"
    else
        augroup lineno
            autocmd!
            autocmd FocusLost * setlocal norelativenumber number
            autocmd InsertEnter * setlocal norelativenumber number
            autocmd InsertLeave * setlocal relativenumber number
        augroup END
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
augroup markdown
    autocmd!
    autocmd BufRead,BufNewFile *.{mk,markdown,mdown,mkdn,mkd,rst} set filetype=markdown
    autocmd FileType markdown setlocal textwidth=80 wrap linebreak foldenable
    autocmd FileType markdown nmap <leader>t :Toc<CR>:q<CR>:lop<CR> 
    autocmd FileType markdown nnoremap <silent><localleader>me :call <SID>IAWriter()<CR>
    autocmd FileType markdown nnoremap <silent><localleader>mp :call <SID>MarkdownPreview('%:p')<CR>
    " autocmd BufRead,BufNewFile *.{md,markdown,mdown,mkd,mkdn} call s:setupMarkdownPreview()
augroup END

"}}}
"}}}

" Node.js https://github.com/joyent/node/wiki/Vim-Plugins


" Python {{{2
Plugin 'lambdalisue/vim-pyenv'
Plugin 'hdima/python-syntax'
Plugin 'klen/python-mode'
let g:pymode_doc = 1
let g:pymode_doc_bind = 'K'
let g:pymode_folding = 1
let g:pymode_indent = 1
let g:pymode_motion = 1
let g:pymode_options_max_line_length=99
let g:pymode_lint = 1
let g:pymode_lint_checkers = ['pyflakes', 'pep8', 'mccabe']
let g:pymode_lint_ignore = "E501"
let g:pymode_syntax_slow_sync = 0
let g:pymode_virtualenv = 1
let g:pymode_run = 0
" let g:pymode_run_bind = '<localleader>ger'
let g:pymode_breakpoint = 1
let g:pymode_breakpoint_bind = '<localleader>gb'
let g:pymode_rope = 0
let g:pymode_rope_completion = 0
let g:pymode_rope_complete_on_dot = 0

Plugin 'davidhalter/jedi-vim'
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


Plugin 'derekwyatt/vim-scala'

Plugin 'keith/swift.vim'

" Tasklist
Plugin 'TaskList.vim'
augroup tasklist
    autocmd!
    autocmd BufWinEnter -TaskList_* setlocal norelativenumber number nowrap
augroup END
map <unique><localleader>t <Plug>TaskList

" Taskpaper
Plugin 'davidoc/taskpaper.vim'
let g:task_paper_date_format="%Y-%m-%d %H:%M%p"
" }}}

"
" Org mode {{{1
Plugin 'jceb/vim-orgmode'
let g:org_heading_shade_leading_stars=1
let g:org_indent=1
let g:org_todo_keywords = [
    \ [ 'TODO(t)', 'NEXT(n)', '|', 'DONE(d)' ],
    \ [ 'WIPR(p)', 'WAIT(w)', '|', 'DONE(d)', 'FILED(f)', 'SKIP(x)'],
\ ]
let g:org_agenda_files = ['~/Dropbox/Dox/active/*.org']
" autocmd BufNewFile,BufRead *.org setfiletype org
autocmd FileType org setlocal textwidth=0 nowrap nolinebreak
augroup org
    autocmd!
    autocmd FileType org inoremap <M-C-Left> <C-o>:silent! :py ORGMODE.plugins[u"EditStructure"].promote_heading(including_children=False)<CR>
    autocmd FileType org nnoremap <silent><M-C-Left> :py ORGMODE.plugins[u"EditStructure"].promote_heading(including_children=False)<CR>
    autocmd FileType org inoremap <M-S-Left> <C-o>:silent! :py ORGMODE.plugins[u"EditStructure"].promote_heading(including_children=True)<CR>
    autocmd FileType org nnoremap <silent><M-S-Left> :py ORGMODE.plugins[u"EditStructure"].promote_heading(including_children=True)<CR>
    autocmd FileType org inoremap <M-C-Right> <C-o>:silent! :py ORGMODE.plugins[u"EditStructure"].demote_heading(including_children=False)<CR>
    autocmd FileType org nnoremap <silent><M-C-Right> :py ORGMODE.plugins[u"EditStructure"].demote_heading(including_children=False)<CR>
    autocmd FileType org inoremap <M-S-Right> <C-o>:silent! :py ORGMODE.plugins[u"EditStructure"].demote_heading(including_children=True)<CR>
    autocmd FileType org nnoremap <silent><M-S-Right> :py ORGMODE.plugins[u"EditStructure"].demote_heading(including_children=True)<CR>
augroup END

" Orgmode helping plugins
Plugin 'tpope/vim-speeddating'
Plugin 'vim-scripts/utl.vim'
nmap <silent>glo :Utl openLink underCursor edit<CR>
nmap <silent>glt :Utl openLink underCursor tabe<CR>
nmap <silent>glv :Utl openLink underCursor vsplit<CR>
nmap <silent>gls :Utl openLink underCursor split<CR>
nmap <silent>gly :Utl copyLink underCursor<CR>
Plugin 'mattn/calendar-vim'
" }}}

" Scratch files, notes, outliner etc. {{{1
" :Pad
Plugin 'fmoralesc/vim-pad'
let g:pad#dir='/Users/apopescu/Dropbox/Dox/nvall'
let g:pad#default_file_extension='.md'
let g:pad#search_backend='ag'
let g:pad#query_dirnames=0
let g:pad#query_filenames=1
let g:pad#rename_files=0
let g:pad#read_nchars_from_files=0
let g:pad#title_first_line=0
let g:pad#window_height=12
let g:pad#ignored_extensions=["plist", "pdf", "odt", "docx", "doc"]
"let g:pad#open_in_split=0
let g:pad#set_mappings=0
nmap <leader>ql <Plug>(pad-list)
nmap <leader>qn <Plug>(pad-new)
nmap <leader>qs <Plug>(pad-search)

" :Note
" Plugin 'xolox/vim-misc'
" Plugin 'xolox/vim-notes'
let g:notes_directories=['~/Dropbox/Dox/nvall']
let g:notes_suffix='.md'
let g:title_sync='no'
let g:notes_smart_quotes=0
let g:notes_list_bullets=['*', '-', '+']
let g:notes_word_boundaries=1
let g:notes_unicode_enabled=1
let g:notes_conceal_code=0

" :scratch :Sscratch
" Plugin 'duff/vim-scratch'

" outliner .otl
" Plugin 'vimoutliner/vimoutliner'

" wiki
" Plugin 'vimwiki/vimwiki'
"}}}

"
" Enhancements {{{1
"
"{{{2
" Improved %
Plugin 'edsono/vim-matchit'

Plugin 'kien/rainbow_parentheses.vim'
augroup rainbowpar
    autocmd!
    autocmd VimEnter * RainbowParenthesesToggle
    autocmd Syntax * RainbowParenthesesLoadRound
    autocmd Syntax * RainbowParenthesesLoadSquare
    autocmd Syntax * RainbowParenthesesLoadBraces
augroup END

Plugin 'jiangmiao/auto-pairs'
" Alt+0 and Alt+9
let g:AutoPairsShortcutJump='º'
let g:AutoPairsShortcutFastWrap='ª'
"Plugin 'Raimondi/delimitMate'
"let delimitMate_expand_cr=1

"Plugin 'Townk/vim-autoclose'

Plugin 'HTML-AutoCloseTag'
" Disable: let b:mapped_auto_closetag = 1
"}}}


Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
let g:airline_theme='twofirewatch' "kolor papercolor light kalisi molokai bubblegum solarized durant luna
let g:airline_section_c='b%n %f%m %#__accent_red#%{airline#util#wrap(airline#parts#readonly(),0)}%#__restore__#'
let g:airline_section_z='%4l:%-3c %3p%%'   
let g:airline_mode_map={
       \ '__' : '-',
       \ 'n'  : 'N',
       \ 'i'  : 'I',
       \ 'R'  : 'R',
       \ 'c'  : 'C',
       \ 'v'  : 'V',
       \ 'V'  : 'V',
       \ '' : 'V',
       \ 's'  : 'S',
       \ 'S'  : 'S',
       \ '' : 'S',
       \ }
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'


" CamelCase 
" Plugin 'camelcasemotion'
Plugin 'bkad/CamelCaseMotion' 
" Alt - w/b/3/E
map ∑ <Plug>CamelCaseMotion_w
map ∫ <Plug>CamelCaseMotion_b
map £ <Plug>CamelCaseMotion_e
map ´ <Plug>CamelCaseMotion_ge
omap <silent> i∑ <Plug>CamelCaseMotion_iw
xmap <silent> i∑ <Plug>CamelCaseMotion_iw
omap <silent> i∫ <Plug>CamelCaseMotion_ib
xmap <silent> i∫ <Plug>CamelCaseMotion_ib
omap <silent> i£ <Plug>CamelCaseMotion_ie
xmap <silent> i£ <Plug>CamelCaseMotion_ie

Plugin 'easymotion/vim-easymotion'
" Disable default mappings
let g:EasyMotion_do_mapping=0
let g:EasyMotion_use_upper=1
let g:EasyMotion_keys = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
let g:EasyMotion_inc_highlight=1
let g:EasyMotion_move_highlight=1
let g:EasyMotion_landing_highlight=0
let g:EasyMotion_add_search_history=0
"map ' <Plug>(easymotion-prefix)
nmap <leader>s <Plug>(easymotion-sn)
nmap <leader><leader>s <Plug>(easymotion-sn)
vmap <leader><leader>s <Plug>(easymotion-sn)
nmap <leader><leader>w <Plug>(easymotion-bd-w)
vmap <leader><leader>w <Plug>(easymotion-bd-w)
nmap <leader><leader>e <Plug>(easymotion-bd-e)
vmap <leader><leader>e <Plug>(easymotion-bd-e)
nmap <leader><leader>n <Plug>(easymotion-next)
vmap <leader><leader>n <Plug>(easymotion-next)
nmap <leader><leader>p <Plug>(easymotion-next)
vmap <leader><leader>p <Plug>(easymotion-next)
nmap <leader><leader>t <Plug>(easymotion-bd-t)
vmap <leader><leader>t <Plug>(easymotion-bd-t)
nmap <leader><leader>f <Plug>(easymotion-bd-f)
vmap <leader><leader>f <Plug>(easymotion-bd-f)
nmap ,s <Plug>(easymotion-sn)
nmap ,w <Plug>(easymotion-bd-w)
nmap ,e <Plug>(easymotion-bd-e)
nmap ,f <Plug>(easymotion-bd-f)
nmap ,t <Plug>(easymotion-bd-t)
nmap ,n <Plug>(easymotion-next)
vmap ,s <Plug>(easymotion-sn)
vmap ,w <Plug>(easymotion-bd-w)
vmap ,e <Plug>(easymotion-bd-e)
vmap ,f <Plug>(easymotion-bd-f)
vmap ,t <Plug>(easymotion-bd-t)
vmap ,n <Plug>(easymotion-next)


Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-unimpaired'


" Dash.app
Plugin 'rizzatti/funcoo.vim'
Plugin 'rizzatti/dash.vim'
" noremap do *not* work with <Plug>
nmap <leader>h <Plug>DashSearch

" Git {{{2
Plugin 'airblade/vim-gitgutter'
let g:gitgutter_max_signs = 250
let g:gitgutter_realtime = 0
" nnoremap <silent><leader>G :GitGutterSignsToggle<CR>
nmap <silent> [og :GitGutterSignsEnable<CR>
nmap <silent> ]og :GitGutterSignsDisable<CR>
nmap <silent> cog :GitGutterSignsToggle<CR>
" Plugin 'mhinz/vim-signify'


Plugin 'tpope/vim-fugitive'
" Magit for Vim
" Plugin 'jreybert/vimagit'
"}}}
"}}}

"
" Experimental {{{1
" Plugin 'maxbrunsfeld/vim-yankstack'
" let g:yankstack_map_keys = 0
" nmap π <Plug>yankstack_substitute_older_paste " Alt+p
" nmap ∏ <Plug>yankstack_substitute_newer_paste " Alt+Shift+p

Plugin 'mhinz/vim-startify'
let g:startify_list_order = [
    \ ['Bookmarks:'],
    \ 'bookmarks', 
    \ ['Most recent in current directory:'],
    \ 'dir', 
    \ ['Recent files:'], 
    \ 'files',
    \ ['Sessions:'], 
    \ 'sessions',
    \ ]
let g:startify_bookmarks = [
    \ '~/Dropbox/Dox/active/',
    \ '~/Dropbox/Dox/active/01-weekly.taskpaper',
    \ '~/Dropbox/Dox/active/02-thoughts.md',
    \ '~/Dropbox/Dox/active/03-email_drafts.md',
    \ '~/Dropbox/Dox/active/04-notes.org',
    \ '~/.vimrc',
    \]
let g:startify_session_autoload = 1
let g:startify_skiplist = [
    \ '.git/.*',
    \ '.hg/.*',
    \ ]
let g:startify_change_to_dir = 1

Plugin 'chrisbra/NrrwRgn'

Plugin 'terryma/vim-expand-region'
let g:expand_region_text_objects = {
    \ 'iw' :0,
    \ 'i"' :0,
    \ 'i''' :0,
    \ 'ia' :0,
    \ 'i]' :1,
    \ 'ib' :1,
    \ 'iB' :1,
    \ 'is' :0,
    \ 'il' :0,
    \ 'ii' :0,
    \ 'if' :0,
    \ 'ip' :0,
    \ 'ie' :0,
    \}
"}}}

call vundle#end()
filetype plugin indent on " required!

" set color scheme
" kolor flatttown inkpot liquidcarbon kolor desert256 dante navajo papayawhip
" alduin camo gruvbox simpleandfriendly summerfruit256 
if has("unix")
    " use if needed to determine the os
    " or use has("mac")
    let s:uname = system("uname -s")

    if has("gui_running")
        colorscheme lucius
        LuciusWhiteHighContrast
    else
        colorscheme summerfruit256
    endif
endif
" call <SID>SetCursorLineColors()
call <SID>SetColorColumn()


" * * * * * * * * * * * * * * * * * * * * * * * * * *
" Old, unused {{{ 
" Unite: can replace CtrlP, Tagbar
" Note: vimproc requires compiling a c file
" Plugin 'Shougo/vimproc.vim'
" Plugin 'Shougo/unite.vim'
" Plugin 'Shougo/unite-outline'
" Plugin 'Shougo/vimfiler.vim'
" Plugin 'Shougo/neoyank.vim'
" Plugin 'Shougo/unite-help'
" Plugin 'tsukkee/unite-tag'
" nnoremap <leader>u  :<C-u>Unite -start-insert file_rec/async<CR>
" nnoremap <leader>p  :<C-u>Unite -start-insert file_rec/async<CR>
" nnoremap <leader>gt :<C-u>Unite tab<CR>
" nnoremap <leader>ff :<C-u>Unite -start-insert file/async<CR>
" nnoremap <leader>fb :<C-u>Unite buffer bookmark<CR>
" nnoremap <leader>b :<C-u>Unite buffer bookmark<CR>
" nnoremap <leader>fo :<C-u>Unite -vertical -winwidth=35 -direction=belowright outline<CR>
" nnoremap <leader>fh :<C-u>Unite history/yank<CR>
" nnoremap <silent> <F5> <Plug>(unite-redraw)
"
" Plugin 'AutoClose'
" Disable:
" let g:autoclose_loaded = 1
" let g:autoclose_on = 1

" Plugin 'project.tar.gz'
" Plugin 'thisivan/vim-taglist'
" let Tlist_Ctags_Cmd='/usr/local/bin/ctags'
" let Tlist_Show_One_File=1
" let Tlist_Auto_Highlight_Tag=1
" let Tlist_Use_Right_Window=0
" let Tlist_Close_On_Select=1
" let Tlist_GainFocus_On_ToggleOpen=1
" let Tlist_Sort_Type="name"
" nnoremap <silent> <F9> :TlistToggle<CR>
" map <silent> <Leader>t :TlistToggle<CR>

" Plugin 'git://git.wincent.com/command-t.git'
" CommandT settings
" disabled; using CtrlP
" let g:command_t_loaded = 1
" map <unique> <Leader>t :CommandT<CR>
" let g:CommandTCancelMap='<C-x>'
"
" Plugin 'Vim-JDE'
" Plugin 'javacomplete'
"
" YouCompleteMe requires the same version of
" Python to be used for vim, MacVim, and itself
" both at compile time and runtime 
" Plugin 'Valloric/YouCompleteMe'
" let g:ycm_auto_trigger = 0
" }}}
"
" * * * * * * * * * * * * * * * * * * * * * * * * * *
"
" Credits:
"
" http://blog.danielfischer.com/2010/11/19/a-starting-guide-to-vim-from-textmate/
" http://items.sjbach.com/319/configuring-vim-right
" https://github.com/tpope/vim-pathogen
" http://dougireton.com/blog/2013/02/23/layout-your-vimrc-like-a-boss/
" 
" vim: set foldlevel=0 foldmethod=marker:
