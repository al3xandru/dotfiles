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
autocmd FocusLost, BufLeave * silent! :wall

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
" kspell: dictionary completion only when spell enabled set spell 
set complete=.,w,b,u,t,i,kspell
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

set colorcolumn=81,121
" http://vim.wikia.com/wiki/Highlight_current_line
set cursorline
augroup CursorLine
  autocmd!
  autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline
augroup END
function! SetCursorLineColors()
    hi CursorLine    ctermbg=52 guibg=#424242
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
        "set gfn=Anonymous_Pro:h12
        "set gfn=Consolas:h12
        "set gfn=Cousine:h11
        set gfn=Hack:h12
        "set gfn=Inconsolata:h13
        "set gfn=Input_Mono:h11
        "set gfn=Liberation_Mono:h11
        "set gfn=monofur:h15
        "set gfn=ProFontX:h12
        "set gfn=Source\ Code\ Pro:h11
    elseif has("gui_gtk2")
        set gfn=monofur\ 12,SourceCodePro\ 10,Anonymous\ Pro\ 10
    endif

    set columns=120
    set lines=80
endif
"function! FullScrHoriz()
    "set fuopt+=maxhorz
    "set fullscreen
"endfunction
"function! BackFullScrHoriz()
    "set fuopt-=maxhorz
    "set nofu
"endfunction
"command! Fullscr call FullScrHoriz()
"command! Nofullscr call BackFullScrHoriz()

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
nnoremap <silent><C-n> :call ToggleLineNo()<cr>
augroup lineno
    autocmd!
    autocmd FocusLost * set norelativenumber | set number
    autocmd InsertEnter * set norelativenumber | set number
    autocmd InsertLeave * set relativenumber | set nonumber
    autocmd Filetype qf setlocal norelativenumber number nowrap
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
set timeoutlen=750
" abbreviations
cnoreabbrev W w
cnoreabbrev Q q

" Change mapleader from <Leader> = \
let mapleader=","
let maplocalleader="\\"

inoremap jk <esc>
inoremap <silent> <Up> <esc><Up>
inoremap <silent> <Down> <esc><Down>
inoremap <silent> <Left> <esc><Left>
inoremap <silent> <Right> <esc><Right>

" make vertical line nav better
nnoremap j gj
nnoremap k gk
" make ; behave like : (save the Shift)
nnoremap ; :
nnoremap , ;
nnoremap … ;
nnoremap <tab> %
vnoremap <tab> %

" http://vi.stackexchange.com/questions/2761/set-cursor-colour-different-when-on-a-highlighted-word
nnoremap <silent> n n:call HLNext(0.4)<CR>
nnoremap <silent> N N:call HLNext(0.4)<cr>

function! HLNext (blinktime)
    let target_pat = '\c\%#'.@/
    let ring = matchadd('ErrorMsg', target_pat, 101)
    redraw
    exec 'sleep ' . float2nr(a:blinktime * 1000) . 'm'
    call matchdelete(ring)
    redraw
endfunction

" Inserts the path of the currently edited file into a command
" Command mode: Ctrl+P
cnoremap <C-P> <C-R>=expand("%:p:h") . "/" <CR>
cnoremap <C-S> <C-R>="mksession! " . getcwd() . "/.session.vim" <CR>
"cnoremap <C-S> <C-R>="mksession! " . expand("%:p:h") . "/.session.vim" <CR>

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
" window vertical resize
nmap <silent><C-w>< :vertical resize -10<CR>
nmap <silent><C-w>> :vertical resize +10<CR>
nmap <silent><C-w>- :resize -10<CR>
nmap <silent><C-w>+ :resize +10<CR>
" open tag in tab
nnoremap <C-\> <C-w><C-]><C-w>T
inoremap <C-\> <C-w><C-]><C-w>T
nnoremap ‘ <C-w><C-]><C-w>T
inoremap ‘ <C-w><C-]><C-w>T

" Show special characters
nmap <silent> <leader>ch :set nolist!<CR>
" disable highlighted search 
nnoremap <silent><leader><space> :nohlsearch<CR>
"reselect pasted text
nnoremap <leader>v V`] 

" Opens an edit command with the path of the currently edited file filled in
nnoremap <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>et :tabe <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>es :split <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>ev :vsplit <C-R>=expand("%:p:h") . "/" <CR>

" bind f and F to perform searches for the word under cursor
" grep results go into quicklist: copen/cclose
nnoremap /g :grep! -R '<C-r><C-w>' <C-r>=getcwd()<CR><CR><Bar>:copen<CR>
nnoremap /fi :grep! -R '<C-r><C-w>' <C-r>=getcwd()<CR>
nnoremap /fc :grep -R '<C-r><C-w>' <C-r>=expand("%:p:h")<CR>
"nnoremap <leader>F :vimgrep! /\C\<<C-r><C-w>\>/gj <C-r>=expand("%:p:h")<CR>
"nnoremap <Leader>F :vimgrep! /\<<C-r><C-w>\>/j

" Using the_silver_searcher to look for word under cursor in current dir
nnoremap /a :Ag! --<C-r>=&filetype<CR> "\b<C-r><C-w>\b" <C-r>=getcwd()<CR>
nnoremap /fd :Ag! --<C-r>=&filetype<CR> "\b<C-r><C-w>\b" <C-r>=expand("%:p:h")<CR>
" Using Ack to search the word under cursor in the current dir
" nnoremap <Leader>f :Ack! --type=<C-r>=%filetype<CR> "\b<C-r><C-w>\b" <C-r>=expand("%:p:h")<CR>

" bind r to replace word under cursor
nnoremap <leader>r :%s/\<<C-r><C-w>\>//cg<Left><Left><Left>
" display :Errors
"nnoremap <leader>l :Errors<CR>


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


" Color schemes {{{1
Plugin 'Colour-Sampler-Pack'
Plugin '256-grayvim'
Plugin 'blacklight'
Plugin 'MochaLatte'
Plugin 'altercation/vim-colors-solarized'
let g:solarized_termtrans=0
let g:solarized_termcolors=256
let g:solarized_visibility="high"
let g:solarized_contrast="normal"
Plugin 'blerins/flattown'
Plugin 'gregsexton/Atom'
Plugin 'jonathanfilip/vim-lucius'
" After enabling: :Lucius[Black|BlackHighContrast|BlackLowContrast|
"   Dark|DarkHighContrast|DarkLowContrast|Light|LightLowContrast|
"   White|WhiteLowContrast]
Plugin 'nice/sweater'
Plugin 'zefei/cake16'
Plugin 'zeis/vim-kolor'
" }}}


" Extra text objects {{{1
" Line: l
Plugin 'kana/vim-textobj-user'
Plugin 'kana/vim-textobj-line'
" CamelCase 
"Plugin 'camelcasemotion'
"Plugin 'bkad/CamelCaseMotion' 
"nmap <leader>cw <Plug>CamelCaseMotion_w
"vmap <leader>cw <Plug>CamelCaseMotion_w
"nmap <leader>ce <Plug>CamelCaseMotion_e
"vmap <leader>ce <Plug>CamelCaseMotion_e
"nmap <leader>cb <Plug>CamelCaseMotion_b
"vmap <leader>cb <Plug>CamelCaseMotion_b
"Function arguments: a
Plugin 'argtextobj.vim' 
" Indent: i 
Plugin 'michaeljsmith/vim-indent-object' 
" }}}


"
" Important {{{1
"
Plugin 'kien/ctrlp.vim'
set runtimepath^=~/.vim/bundle/ctrlp
let g:loaded_ctrlp = 0
let g:ctrlp_map = '<F7>'
let g:ctrlp_cmd = 'CtrlP'
nnoremap <unique> <leader>p :CtrlP<CR>
nnoremap <unique> <leader>P :CtrlPBufTag<CR>
nnoremap <unique> <leader>b :CtrlPBuffer<CR>
" Replaced by CtrlPBuffer Plugin 'jlanzarotta/bufexplorer'


Plugin 'majutsushi/tagbar'
let g:tagbar_autoclose = 1
let g:tagbar_showlinenumbers = 1
nnoremap <silent> <leader>o :TagbarToggle<CR>
nnoremap <silent> <F9> :TagbarToggle<CR>


"Plugin 'al3xandru/nerdcommenter'
" Plugin 'scrooloose/nerdcommenter'
Plugin 'tpope/vim-commentary'
" Plugin 'tomtom/tcomment_vim'


Plugin 'scrooloose/nerdtree'
"let NERDTreeWinPos='right'
let NERDTreeIgnore=['\.pyc', '\.pyo', '\~$', '\.o$', '\.class$', 
    \ '\.egg$', '\.idea$',
    \ '\.bzr', '\.git', '\.hg', '\.svn']
let NERDTreeQuitOnOpen=1
let NERDChristmasTree=1
let NERDTreeHighlightCursorline=1
nnoremap <unique> <leader>t :NERDTreeToggle<CR>
nnoremap <leader>T :NERDTreeFind<CR>
nnoremap <silent> <F8> :NERDTreeToggle<CR>
nnoremap <silent> <S-F8> :NERDTreeFind<CR>


Plugin 'scrooloose/syntastic'
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
"let g:syntastic_error_symbol = "✗"
"let g:syntastic_warning_symbol = "⚠"
"}}}

"
" Tags/ctags/omnicomplete (check tagfiles: echo tagfiles()) {{{1
"
set tags=./.git/tags;,./.tags;,./tags;,./TAGS;,~/.vim/.vimtags
"Plugin 'AutoTag'
Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-easytags'
let g:easytags_cmd = '/usr/local/bin/ctags'
let g:easytags_async = 1
let g:easytags_dynamic_files = 1
let g:easytags_auto_highlight = 0
let g:easytags_include_members = 1
let g:easytags_updatetime_min = 60000
let g:easytags_syntax_keyword = 'auto'  " 'always'  'auto'
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
"}}}

" Search: Ack and Ag {{{1
Plugin 'mileszs/ack.vim'
Plugin 'rking/ag.vim'
"}}}

"
" Snippets {{{1
"
if has("python")
    Plugin 'SirVer/ultisnips'
else
    Plugin 'MarcWeber/vim-addon-mw-utils'
    Plugin 'tomtom/tlib_vim'
    Plugin 'garbas/vim-snipmate'
endif
Plugin 'honza/vim-snippets'
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
let g:go_fmt_autosave = 1
let g:go_fmt_command = 'goimports'
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


" Java completion
Plugin 'VictorDenisov/javacomplete'


" Markdown {{{2
Plugin 'plasticboy/vim-markdown', {'name': 'plasticboy-vim-markdown'}
" Markdown preview {{{3
"Plugin 'greyblake/vim-preview' could not get it to work
if has("unix")
    let s:uname = system("uname -s")
    if s:uname =~ "Darwin"
        function! s:setupMarkdownPreview()
            if filereadable("/Applications/Marked\ 2.app/Contents/Info.plist")
                nnoremap <silent><localleader>mp :silent !open -a 'Marked 2.app' '%:p'<CR>
            else
                nnoremap <silent><localleader>mp :silent !open -a 'Google Chrome.app' '%:p'<CR>
            endif
        endfunction

    endif
endif
"}}}
" Markdown editing {{{3
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
        augroup lineno
            autocmd!
            autocmd FocusLost *.{mk,markdown,mdown,mkdn,mkd,rst}   set norelativenumber | set nonumber
            autocmd InsertEnter *.{mk,markdown,mdown,mkdn,mkd,rst} set norelativenumber | set nonumber
            autocmd InsertLeave *.{mk,markdown,mdown,mkdn,mkd,rst} set norelativenumber | set nonumber
        augroup END
        let g:iawriter_active = 1
        echom "IAWriter activated [new: pencil, old: " .  g:iawriter_save_colorscheme . ", background:" . g:iawriter_save_bgr . "]"
    else
        augroup lineno
            autocmd!
            autocmd FocusLost * set norelativenumber | set number
            autocmd InsertEnter * set norelativenumber | set number
            autocmd InsertLeave * set relativenumber | set nonumber
        augroup END
        call goyo#execute(0, '') 
        execute printf("colorscheme %s", g:iawriter_save_colorscheme)
        if g:iawriter_save_bgr
            execute printf("set background=%s", g:iawriter_save_bgr)
        endif
        let g:iawriter_active = 0
        echom "IAWriter deactivated [new: " . g:iawriter_save_colorscheme . ", old: pencil, background:" . g:iawriter_save_bgr . "]"
    endif
endfunction
augroup markdown
    autocmd!
    autocmd FileType markdown setlocal textwidth=80
    autocmd FileType markdown nnoremap <silent><localleader>me :call IAWriter()<CR>
    autocmd BufRead,BufNewFile *.{md,markdown,mdown,mkd,mkdn} call s:setupMarkdownPreview()
augroup END

"}}}
"}}}

" Node.js https://github.com/joyent/node/wiki/Vim-Plugins


" Python {{{2
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
let g:jedi#goto_assignments_command = "<localleader>ga"
let g:jedi#goto_definitions_command = "<localleader>gd"
let g:jedi#documentation_command = "<localleader>gh"
let g:jedi#usages_command = "<localleader>gu"
let g:jedi#rename_command = "<localleader>gr"
"let g:jedi#completions_enabled = 0
"}}}


Plugin 'derekwyatt/vim-scala'

Plugin 'swift'

" Taskpaper
Plugin 'davidoc/taskpaper.vim'
let g:task_paper_date_format="%Y-%m-%d %H:%M%p"
" }}}

"
" Scratch files, notes, outliner etc. {{{1
" :scratch :Sscratch
Plugin 'duff/vim-scratch'

" :Pad
Plugin 'fmoralesc/vim-pad'
let g:pad#dir='~/Dropbox/Dox/nvall'
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
nmap <leader>qql <Plug>(pad-list)
nmap <leader>qqn <Plug>(pad-new)
nmap <leader>qqs <Plug>(pad-search)

" :Note
"Plugin 'xolox/vim-notes'
let g:notes_directories=['~/Dropbox/Dox/nvall']
let g:notes_suffix='.md'
let g:title_sync='no'
let g:notes_smart_quotes=0
let g:notes_list_bullets=['*', '-', '+']

" outliner .otl
Plugin 'vimoutliner/vimoutliner'
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
let g:EasyMotion_use_upper=1
let g:EasyMotion_keys = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ;'
let g:EasyMotion_inc_highlight=1
let g:EasyMotion_move_highlight=1
let g:EasyMotion_landing_highlight=0
let g:EasyMotion_add_search_history=0
"map ' <Plug>(easymotion-prefix)
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


Plugin 'tpope/vim-surround'


" Dash.app
Plugin 'rizzatti/funcoo.vim'
Plugin 'rizzatti/dash.vim'
" noremap do *not* work with <Plug>
nmap <leader>h <Plug>DashSearch

" Git {{{2
Plugin 'airblade/vim-gitgutter'
let g:gitgutter_max_signs = 250
let g:gitgutter_realtime = 0
nnoremap <silent><leader>G :GitGutterSignsToggle<CR>


Plugin 'tpope/vim-fugitive'
"}}}
"}}}

"
" Experimental {{{1
"
" Unite: can replace CtrlP, Tagbar
" Note: vimproc requires compiling a c file
Plugin 'Shougo/vimproc.vim'
Plugin 'Shougo/unite.vim'
Plugin 'Shougo/unite-outline'
Plugin 'Shougo/vimfiler.vim'
Plugin 'Shougo/neoyank.vim'
Plugin 'Shougo/unite-help'
Plugin 'tsukkee/unite-tag'
nnoremap <leader>u  :<C-u>Unite -start-insert file_rec/async<CR>
nnoremap <leader>gt :<C-u>Unite tab<CR>
nnoremap <leader>ff :<C-u>Unite -start-insert file/async<CR>
nnoremap <leader>fb :<C-u>Unite buffer bookmark<CR>
nnoremap <leader>fo :<C-u>Unite -vertical -winwidth=35 -direction=belowright outline<CR>
nnoremap <leader>fh :<C-u>Unite history/yank<CR>
"}}}

call vundle#end()
filetype plugin indent on " required!

" set color scheme
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

" * * * * * * * * * * * * * * * * * * * * * * * * * *
" Old, unused {{{ 
" Plugin 'AutoClose'
" Disable:
" let g:autoclose_loaded = 1
" let g:autoclose_on = 1

" Plugin 'project.tar.gz'
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


" Plugin 'Vim-JDE'
" Plugin 'javacomplete'
"
" YouCompleteMe requires the same version of
" Python to be used for vim, MacVim, and itself
" both at compile time and runtime 
" Plugin 'Valloric/YouCompleteMe'
" let g:ycm_auto_trigger = 0
" }}}

" vim: set foldmethod=marker:
