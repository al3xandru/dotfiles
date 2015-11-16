" http://blog.danielfischer.com/2010/11/19/a-starting-guide-to-vim-from-textmate/
" http://items.sjbach.com/319/configuring-vim-right
" https://github.com/tpope/vim-pathogen
" http://dougireton.com/blog/2013/02/23/layout-your-vimrc-like-a-boss/
" 
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
"call pathogen#infect()

set lazyredraw
set nocompatible
set backspace=2
" Allow backspace in insert mode
set backspace=indent,eol,start
" Use the OS clipboard by default (on versions compiled with `+clipboard`)
set clipboard=unnamed
set sessionoptions-=options

" Enhance command-line completion
set wildmenu
set wildignore+=.hg,.git,.svn  " version control
set wildignore+=*.o,*.obj,*.exe,*.dll,*.pyc,*.pyo,*.class,*.luac " compiled
set wildignore+=*.DS_Store,*.sw?
set wildignore+=.idea/**
set wildignore+=*.png,*.jpg,*.gif,*.bmp
set wildignore+=*.egg,*.egg-info,*.gem
set wildignore+=*.zip,*.tar.gz,*.gzip,*.rar
set wildignore+=*.aux,*.toc " Latex intermediary files
" Allow cursor keys in insert mode
set esckeys

" autosave on focus lost
au FocusLost * :wa

" 2. moving around, searching and patterns "
set incsearch
set hlsearch
set ignorecase
set smartcase
set showmatch
set scrolloff=5

" 3. tags
" omnicomplete
set omnifunc=syntaxcomplete#Complete
" https://github.com/sjl/dotfiles/blob/eea18b00b8c74943f5094fddf91d3c2a7e0a7242/vim/vimrc#L534
set complete=.,w,b,u,t
" http://vim.wikia.com/wiki/Make_Vim_completion_popup_menu_work_just_like_in_an_IDE
set completeopt=longest,menu,preview
" http://stackoverflow.com/questions/7722177/how-do-i-map-ctrl-x-ctrl-o-to-ctrl-space-in-terminal-vim
inoremap <C-Space> <C-x><C-o>
inoremap <C-@> <C-Space>

" 4. displaying text
filetype on
filetype plugin on
filetype plugin indent on

set synmaxcol=500
set listchars=tab:▸\ ,trail:·,eol:¬
set showbreak=↪

" 5. syntax, highlighting and spelling"
" Pastel: desert256 jellybeans wombat256 ir_black molokai
" Dark: dante, koehler, vividchalk, vibrantink, molokai, tango, fnaqeran,
" marollocio, macvim, motus, railcast, tir_black
" Light: buttercream, papayawhip, navajo, inkpot, sweater
" Grey: inkpot, camo, earendel, lucius
" Pastel: desert256 jellybeans wombat256 ir_black molokai

syntax on
colorscheme koehler

set colorcolumn=80,120
" http://vim.wikia.com/wiki/Highlight_current_line
set cursorline
augroup CursorLine
  autocmd!
  autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline
augroup END
function! SetCursorLineColors()
    hi CursorLine    ctermbg=52 guibg=#5f0000
    hi CursorLineNr  term=bold ctermfg=226 gui=bold guifg=#ffff00
endfunction
call SetCursorLineColors()

set matchtime=3
set dictionary=/usr/share/dict/words
set thesaurus+=~/.vim/mthesaur.txt

" 6. multiple windows "
set title
set laststatus=2
set statusline=%t\ %l,%v%=[b%n\ %L:%p%%\ %y]%<\ [a\%03.3b:h\%02.2B]
hi StatusLine ctermbg=59 ctermfg=69 

" GUI "
set guicursor=n-v-c:block-Cursor,ve:ver35-Cursor,o:hor50-Cursor,i-ci:ver25-lCursor,r-cr:hor20-Cursor,sm:block
set guioptions=aAce
set selection=exclusive
if has("gui_running")
    set go-=T
    set go-=l
    set go-=L
    set go-=r
    set go-=R
    if has("gui_macvim")
        "set gfn=Anonymous\ Pro:h12
        "set gfn=Consolas:h12
        "set gfn=Cousine:h11
        set gfn=Hack:h11
        "set gfn=Inconsolata:h13
        "set gfn=Input\ Mono:h11
        "set gfn=monofur:h15
        "set gfn=ProFontX:h12
        "set gfn=Source\ Code\ Pro:h11
    elseif has("gui_gtk2")
        set gfn=monofur\ 12,SourceCodePro\ 10,Anonymous\ Pro\ 10
    endif

    set columns=120
    set lines=80
endif
function! FullScrHoriz()
    set fuopt+=maxhorz
    set fullscreen
endfunction
function! BackFullScrHoriz()
    set fuopt-=maxhorz
    set nofu
endfunction
command! Fullscr call FullScrHoriz()
command! Nofullscr call BackFullScrHoriz()

" splits
set splitbelow
set splitright

" 11.messages and info
set number
set ruler
set visualbell t_vb=

" http://jeffkreeftmeijer.com/2012/relative-line-numbers-in-vim-for-super-fast-movement/
function! ToggleLineNo()
    if(&relativenumber == 1)
        set norelativenumber
        set number
    else
        set nonumber
        set relativenumber
    endif
endfunction
nnoremap <C-n> :call ToggleLineNo()<cr>
augroup lineno
    autocmd!
    autocmd FocusLost * set norelativenumber | set number
    autocmd InsertEnter * set norelativenumber | set number
    autocmd InsertLeave * set relativenumber | set nonumber
augroup END
set relativenumber

" 14. tabs and indenting
set expandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4
set smartindent
set autoindent


" 15. folding
set foldenable
set foldmethod=indent
set foldnestmax=10
set foldlevel=100
"HTML folding tag
nnoremap <leader>zha Vatzf


" 16 diff mode
set diffopt=filler,vertical
" 17. mapping

" abbreviations
cnoreabbrev W w
cnoreabbrev Q q

" Change mapleader from <Leader> = \
let mapleader=","
let maplocalleader="\\"

inoremap jk <esc>
" make vertical line nav better
nnoremap j gj
nnoremap k gk
" make ; behave like : (save the Shift)
nnoremap ; :
nnoremap <tab> %
vnoremap <tab> %

" vertical resize
nmap <C-w>< :vertical resize +20<CR>
nmap <C-w>> :vertical resize -20<CR>

" Inserts the path of the currently edited file into a command
" Command mode: Ctrl+P
cnoremap <C-P> <C-R>=expand("%:p:h") . "/" <CR>
cnoremap <C-S> <C-R>="mksession! " . expand("%:p:h") . "/.session.vim" <CR>

" Insert a newline in normal mode
nnoremap <S-Enter> O<Esc>
nnoremap <CR> o<Esc>
nnoremap <NL> i<CR><Esc> " Ctrl-j: break the line at cursor


" Buffer switch
nmap <C-a> :bNext<CR>
nmap <C-e> :e#<CR>
" Tab switch
nnoremap <C-S-tab> :tabprevious<CR>
nnoremap <C-tab> :tabnext<CR>
inoremap <C-S-tab> <ESC>:tabprevious<CR>
inoremap <C-tab> <ESC>:tabnext
" Window switch
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Show special characters
nmap <silent> <leader>c :set nolist!<CR>
" disable highlighted search 
nnoremap <leader><space> :nohlsearch<CR>
"reselect pasted text
nnoremap <leader>v V`] 

" Opens an edit command with the path of the currently edited file filled in
nnoremap <leader>ee :e <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>et :tabe <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>es :split <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>ev :vsplit <C-R>=expand("%:p:h") . "/" <CR>

" bind f and F to vimgrep word under cursor
nnoremap <leader>F :vimgrep! /\C\<<C-r><C-w>\>/gj 
"nnoremap <Leader>F :vimgrep! /\<<C-r><C-w>\>/j
" Using the_silver_searcher to look for word under cursor in current dir
nnoremap <leader>f :Ag! --<C-r>=&filetype<CR> "\b<C-r><C-w>\b" <C-r>=expand("%:p:h")<CR>
"nnoremap <Leader>g :execute 'Ag! --' . &filetype . ' "\b"' . expand("<cword>") . '"\b" ' . fnameescape(expand("%:p:h"))
" Using Ack to search the word under cursor in the current dir
" nnoremap <Leader>f :Ack! --type=<C-r>=%filetype<CR> "\b<C-r><C-w>\b" <C-r>=expand("%:p:h")<CR>

" bind r to replace word under cursor
nnoremap <leader>r :%s/\<<C-r><C-w>\>//cg<Left><Left><Left>
" display :Errors
nnoremap <leader>l :Errors<CR>


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




" Load Vundle
" Only Plugin settings are allowed until vundle#end()
filetype off " required!

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'

" www.vim.org/scripts
Plugin 'AutoTag'
set tags=./tags;/


Plugin 'HTML-AutoCloseTag'
" Disable:
" let b:mapped_auto_closetag = 1

" color schemes
Plugin 'Colour-Sampler-Pack'
Plugin '256-grayvim'
Plugin 'blacklight'
Plugin 'MochaLatte'
Plugin 'blerins/flattown'
Plugin 'gregsexton/Atom'
Plugin 'nice/sweater'
Plugin 'zefei/cake16'
Plugin 'zeis/vim-kolor'

Plugin 'altercation/vim-colors-solarized'
let g:solarized_termtrans=0
let g:solarized_termcolors=256
let g:solarized_visibility="high"
let g:solarized_contrast="normal"


" GitHub
" Scratch files :scratch :Sscratch
Plugin 'duff/vim-scratch'
Plugin 'fmoralesc/vim-pad'
" https://github.com/fmoralesc/vim-pad/issues/81
let g:pad#dir='~/Dropbox/Dox/nvall'
let g:pad#default_file_extension='md'
let g:pad#search_backend='ag'
let g:pad#query_dirnames=0
let g:pad#query_filenames=1
let g:pad#rename_files=0
let g:pad#read_nchars_from_files=50
let g:pad#title_first_line=0
let g:pad#window_height=12
let g:pad#ignored_extensions=["plist", "pdf", "odt", "docx", "doc"]
"let g:pad#open_in_split=0


Plugin 'Townk/vim-autoclose'
" Disable: 
" let g:loaded_AutoClose = 1


Plugin 'kien/ctrlp.vim'
" CtrlP
let g:loaded_ctrlp = 0
set runtimepath^=~/.vim/bundle/ctrlp
nnoremap <unique> <leader>s :CtrlP<CR>
nnoremap <unique> <leader>t :CtrlPBufTag<CR>
let g:ctrlp_map = '<F7>'
let g:ctrlp_cmd = 'CtrlP'


Plugin 'kien/rainbow_parentheses.vim'
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces



Plugin 'scrooloose/nerdcommenter'


Plugin 'scrooloose/nerdtree'
"let NERDTreeWinPos='right'
nnoremap <silent> <F8> :NERDTreeToggle<CR>
nnoremap <silent> <S-F8> :NERDTreeFind<CR>
nnoremap <unique> <leader>p :NERDTreeToggle<CR>
nnoremap <leader>fp :NERDTreeFind<CR>
let NERDTreeIgnore=['\.pyc', '\.pyo', '\~$', '\.o$', '\.class$', 
    \ '\.egg$', '\.idea$',
    \ '\.bzr', '\.git', '\.hg', '\.svn']
let NERDTreeQuitOnOpen=1
let NERDChristmasTree=1
let NERDTreeHighlightCursorline=1

" snippets
if has("python")
    Plugin 'SirVer/ultisnips'
else
    Plugin 'MarcWeber/vim-addon-mw-utils'
    Plugin 'tomtom/tlib_vim'
    Plugin 'garbas/vim-snipmate'
endif
Plugin 'honza/vim-snippets'


Plugin 'scrooloose/syntastic'

" Unite: can replace CtrlP, Tagbar
" Note: vimproc requires compiling a c file
"Plugin 'Shougo/vimproc.vim'
"Plugin 'Shougo/unite.vim'
"Plugin 'Shougo/unite-outline'
"Plugin 'Shougo/vimfiler.vim'
"Plugin 'Shougo/neoyank.vim'
"Plugin 'tsukkee/unite-tag'
"Plugin 'Shougo/unite-help'
"nnoremap <leader>uf :<C-u>Unite -start-insert file/async<CR>
"nnoremap <leader>u  :<C-u>Unite -start-insert file_rec/async<CR>
"nnoremap <leader>ud :<C-u>Unite -start-insert file_rec/async<CR>
"nnoremap <leader>ub :<C-u>Unite buffer bookmark<CR>
"nnoremap <leader>ut :<C-u>Unite tab<CR>
"nnoremap <leader>uo :<C-u>Unite -vertical -winwidth=35 -direction=belowright outline<CR>
"nnoremap <leader>uy :<C-u>Unite history/yank<CR>
Plugin 'Shougo/neocomplete.vim'
augroup neocomplete
    autocmd!
    autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
    autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
augroup END




Plugin 'edsono/vim-matchit'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'

Plugin 'majutsushi/tagbar'
let g:tagbar_autoclose = 1
let g:tagbar_showlinenumbers = 1
nnoremap <silent> <leader>o :TagbarToggle<CR>
nnoremap <silent> <F9> :TagbarToggle<CR>

" Dash.app
Plugin 'rizzatti/funcoo.vim'
Plugin 'rizzatti/dash.vim'
" noremap do *not* work with <Plug>
nmap <leader>h <Plug>DashSearch



Plugin 'bling/vim-airline'
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


Plugin 'easymotion/vim-easymotion'
" Disable default mappings
let g:EasyMotion_do_mapping=0
"map ' <Plug>(easymotion-prefix)
nmap "s <Plug>(easymotion-s2)
vmap "s <Plug>(easymotion-s2)
nmap "t <Plug>(easymotion-bd-t)
vmap "t <Plug>(easymotion-bd-t)
nmap "f <Plug>(easymotion-bd-f)
vmap "f <Plug>(easymotion-bd-f)
nmap "w <Plug>(easymotion-bd-wl)
vmap "w <Plug>(easymotion-bd-wl)
nmap "e <Plug>(easymotion-bd-el)
vmap "e <Plug>(easymotion-bd-el)


" This could be replaced by CtrlP
Plugin 'jlanzarotta/bufexplorer'


Plugin 'airblade/vim-gitgutter'
let g:gitgutter_max_signs = 250
let g:gitgutter_realtime = 0
nnoremap <silent><leader>G :GitGutterSignsToggle<CR>



Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-easytags'
let g:easytags_cmd = '/usr/local/bin/ctags'
let g:easytags_async = 1
let g:easytags_dynamic_files = 1
let g:easytags_auto_highlight = 0
let g:easytags_include_members = 1
let g:easytags_updatetime_min = 60000
let g:easytags_syntax_keyword = 'auto'  " 'always'  'auto'


Plugin 'mileszs/ack.vim'
Plugin 'rking/ag.vim'

" Language support
Plugin 'applescript.vim'

Plugin 'VimClojure'
let vimclojure#SetupKeyMap = 0

Plugin 'fatih/vim-go'
augroup vim_go
    autocmd!
    autocmd FileType go nmap <leader>gs <Plug>(go-implements)
    autocmd FileType go nmap <leader>gi <Plug>(go-info)
    autocmd FileType go nmap <leader>gh <Plug>(go-doc)
    autocmd FileType go nmap <leader>ghv <Plug>(go-doc-vertical)
    autocmd FileType go nmap <leader>ghb <Plug>(go-doc-browser)
    autocmd FileType go nmap <leader>ger <Plug>(go-run)
    autocmd FileType go nmap <leader>geb <Plug>(go-build)
    autocmd FileType go nmap <leader>get <Plug>(go-test)
    autocmd FileType go nmap <leader>gec <Plug>(go-coverage)
augroup END
let g:go_fmt_autosave = 0
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

" HTML Zen Coding
Plugin 'mattn/emmet-vim'

" Node.js https://github.com/joyent/node/wiki/Vim-Plugins

" Python
Plugin 'hdima/python-syntax'
Plugin 'klen/python-mode'
let g:pymode_options_max_line_length=99
let g:pymode_lint_checkers = ['pyflakes', 'pep8', 'mccabe']
let g:pymode_lint_ignore = "E501"
let g:pymode_syntax_slow_sync = 0

Plugin 'davidhalter/jedi-vim'
let g:jedi#use_splits_not_buffers = "top"
let g:jedi#popup_on_dot = 0
"let g:jedi#show_call_signatures = 1
let g:jedi#completions_command = "<C-Space>"
let g:jedi#goto_assignments_command = "<leader>ga"
let g:jedi#goto_definitions_command = "<leader>gd"
let g:jedi#documentation_command = "<leader>gh"
let g:jedi#usages_command = "<leader>gu"
let g:jedi#rename_command = "<leader>gr"
"let g:jedi#completions_enabled = 0

"breaks ]c in vimdiff Plugin 'jsatt/python_fn'
"not very useful Plugin 'ikirudennis/python.vim' 


Plugin 'derekwyatt/vim-scala'

" OmniCompletion
Plugin 'OmniCppComplete'

" outliner .otl
Plugin 'vimoutliner/vimoutliner'

" Java completion
Plugin 'VictorDenisov/javacomplete'


" Markdown
" Basics
Plugin 'plasticboy/vim-markdown', {'name': 'plasticboy-vim-markdown'}
" Markdown preview
"Plugin 'greyblake/vim-preview' could not get it to work
if has("unix")
    let s:uname = system("uname -s")
    if s:uname =~ "Darwin"
        function! s:setupMarkdownPreview()
            if filereadable("/Applications/Marked\ 2.app/Contents/Info.plist")
                nnoremap <silent><leader>mp :silent !open -a 'Marked 2.app' '%:p'<CR>
            else
                nnoremap <silent><leader>mp :silent !open -a 'Google Chrome.app' '%:p'<CR>
            endif
        endfunction

        augroup vimrc_md
            autocmd!
            autocmd BufRead,BufNewFile *.{md,markdown,mdown,mkd,mkdn} call s:setupMarkdownPreview()
        augroup END
    endif
endif
" Markdown editing
Plugin 'junegunn/goyo.vim'

Plugin 'reedes/vim-colors-pencil'
let g:pencil_higher_contrast_ui = 1
let g:pencil_neutral_code_bg = 1
let g:pencil_terminal_italics = 1

let g:iawriter_active = 0
let g:iawriter_save_colorscheme = ""
let g:iawriter_save_bgr = ""
function! IAWriter()
    if g:iawriter_active == 0
        let g:iawriter_save_colorscheme = g:colors_name
        if exists( "&background" )
            let g:iawriter_save_bgr = &background
        endif
        set background=light
        colorscheme pencil
        call goyo#execute(0, '')
        let g:iawriter_active = 1
        echom "IAWriter activated [new: pencil, old: " .  g:iawriter_save_colorscheme . "]"
    else
        call goyo#execute(0, '') 
        execute printf("colorscheme %s", g:iawriter_save_colorscheme)
        if g:iawriter_save_bgr
            execute printf("set background=%s", g:iawriter_save_bgr)
        endif
        let g:iawriter_active = 0
        echom "IAWriter deactivated [new: " . g:iawriter_save_colorscheme . ", old: pencil]"
    endif
endfunction
nnoremap <silent><leader>me :call IAWriter()<cr>


" Taskpaper
Plugin 'davidoc/taskpaper.vim'
let g:task_paper_date_format="%Y-%m-%d %H:%M%p"


call vundle#end()
filetype plugin indent on " required!

if has("unix")
    let s:uname = system("uname -s")
    " kolor flatttown inkpot liquidcarbon kolor desert256 dante navajo papayawhip
    if s:uname =~ "Darwin"
        colorscheme flattown
    else
        colorscheme navajo 
    endif
endif
call SetCursorLineColors()

" **************
" Old, unused 
" Plugin 'AutoClose'
" Disable:
" let g:autoclose_loaded = 1
" let g:autoclose_on = 1

" Plugin 'project.tar.gz'
" Plugin 'msanders/snipmate.vim'
" Plugin 'thisivan/vim-taglist'
"let Tlist_Ctags_Cmd='/usr/local/bin/ctags'
"let Tlist_Show_One_File=1
"let Tlist_Auto_Highlight_Tag=1
"let Tlist_Use_Right_Window=0
"let Tlist_Close_On_Select=1
"let Tlist_GainFocus_On_ToggleOpen=1
"let Tlist_Sort_Type="name"
"nnoremap <silent> <F9> :TlistToggle<CR>
"map <silent> <Leader>t :TlistToggle<CR>

" Plugin 'git://git.wincent.com/command-t.git'
" CommandT settings
" disabled; using CtrlP
" let g:command_t_loaded = 1
" map <unique> <Leader>t :CommandT<CR>
" let g:CommandTCancelMap='<C-x>'

" PeepCode
" Plugin 'mrchrisadams/vim-peepopen'
" let g:peepopen_loaded = 1  "disabled

" Markdown
" Plugin 'file:///Users/alex/.dotfiles/...'
" Plugin 'tpope/vim-markdown'
" Plugin 'jtratner/vim-flavored-markdown'
" Plugin 'greyblake/vim-preview'
" Plugin 'waylan/vim-markdown-extra-preview'

" Plugin 'Vim-JDE'
" Plugin 'javacomplete'
"
" YouCompleteMe requires the same version of
" Python to be used for vim, MacVim, and itself
" both at compile time and runtime 
" Plugin 'Valloric/YouCompleteMe'
" let g:ycm_auto_trigger = 0
