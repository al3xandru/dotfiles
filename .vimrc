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

set nocompatible
set backspace=2
" Allow backspace in insert mode
set backspace=indent,eol,start
" Use the OS clipboard by default (on versions compiled with `+clipboard`)
set clipboard=unnamed
" Enhance command-line completion
set wildmenu
set wildignore+=.hg,.git,.svn  " version control
set wildignore+=*.o,*.obj,*.exe,*.dll,*.pyc,*.class,*.luac " compiled
set wildignore+=*.DS_Store
" Allow cursor keys in insert mode
set esckeys


" 2. moving around, searching and patterns "
set incsearch
set hlsearch
set smartcase
set wildignore+=*.o,*.obj,.git,.svn,.hg,*.class,*.pyo,*.pyc,*.so,*.dll,*.swp,*.zip,*.tar.gz,*.exe
set scrolloff=5

" 3. tags
" omnicomplete
set omnifunc=syntaxcomplete#Complete
" http://vim.wikia.com/wiki/Make_Vim_completion_popup_menu_work_just_like_in_an_IDE
set completeopt=longest,menuone
" http://stackoverflow.com/questions/7722177/how-do-i-map-ctrl-x-ctrl-o-to-ctrl-space-in-terminal-vim
inoremap <C-Space> <C-x><C-o>
inoremap <C-@> <C-Space>

" 4. displaying text
filetype on
filetype plugin on
filetype plugin indent on

set listchars=tab:▸\ ,trail:·,eol:¬

" 5. syntax, highlighting and spelling"
" Pastel: desert256 jellybeans wombat256 ir_black molokai
" Dark: dante, koehler, vividchalk, vibrantink, molokai, tango, fnaqeran,
" marollocio, macvim, motus, railcast, tir_black
" Light: buttercream, papayawhip, navajo, inkpot
" Grey: inkpot, camo, earendel, lucius
" Pastel: desert256 jellybeans wombat256 ir_black molokai

syntax on
colorscheme koehler

set colorcolumn=80,120
" http://vim.wikia.com/wiki/Highlight_current_line
set cursorline
augroup CursorLine
  au!
  au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  au WinLeave * setlocal nocursorline
augroup END
function! SetCursorLineColors()
    hi CursorLine    ctermbg=88 guibg=#870000
    hi CursorLineNr  term=bold ctermfg=226 gui=bold guifg=#ffff00
endfunction
call SetCursorLineColors()

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
if has("gui_macvim")
    set columns=150
    set lines=70
    "set gfn=Anonymous\ Pro:h12
    "set gfn=Cousine:h11
    "set gfn=Inconsolata:h13
    set gfn=Input\ Mono:h12
    "set gfn=monofur:h15
    "set gfn=ProFontX:h12
    "set gfn=Source\ Code\ Pro:h11
end
if has("gui_gtk2")
    set columns=120
    set gfn=monofur\ 12,SourceCodePro\ 10,Anonymous\ Pro\ 10
end

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


" 11.messages and info
set number
set ruler
set visualbell t_vb=


" 14. tabs and indenting
set expandtab
set tabstop=4
set shiftwidth=4
set softtabstop=4
set smartindent


" 15. folding
set foldenable
set foldmethod=indent
set foldlevel=100


" 17. mapping

" abbreviations
cnoreabbrev W w
cnoreabbrev Q q

" Change mapleader from <Leader> = \
" let mapleader=","

" make vertical line nav better
nnoremap j gj
nnoremap k gk

" vertical resize
nmap <C-w>> :vertical resize +20<CR>
nmap <C-w>< :vertical resize -20<CR>

" disable highlighted search 
nnoremap <Leader>S :nohlsearch<CR>

" Opens an edit command with the path of the currently edited file filled in
map <Leader>e :e <C-R>=expand("%:p:h") . "/" <CR>
map <Leader>te :tabe <C-R>=expand("%:p:h") . "/" <CR>
map <Leader>se :split <C-R>=expand("%:p:h") . "/" <CR>
map <Leader>ve :vsplit <C-R>=expand("%:p:h") . "/" <CR>

" Inserts the path of the currently edited file into a command
" Command mode: Ctrl+P
cnoremap <C-P> <C-R>=expand("%:p:h") . "/" <CR>

" Insert a newline in normal mode
nnoremap <S-Enter> O<Esc>
nnoremap <CR> o<Esc>
nnoremap <NL> i<CR><Esc> " Ctrl-j: break the line at cursor

" Show special characters
nmap <silent> <Leader>c :set nolist!<CR>

" Buffer switch
nmap <C-a> :bNext<CR>
nmap <C-e> :e#<CR>

" bind f and F to vimgrep word under cursor
"nnoremap <Leader>f :vimgrep! /\<<C-r><C-w>\>/j *<CR>:cw<CR>
"nnoremap <Leader>F :vimgrep! /\<<C-r><C-w>\>/j
" Using the_silver_searcher to look for word under cursor in current dir
nnoremap <Leader>f :Ag! --<C-r>=&filetype<CR> "\b<C-r><C-w>\b" <C-r>=expand("%:p:h")<CR>
"nnoremap <Leader>g :execute 'Ag! --' . &filetype . ' "\b"' . expand("<cword>") . '"\b" ' . fnameescape(expand("%:p:h"))
" Using Ack to search the word under cursor in the current dir
" nnoremap <Leader>f :Ack! --type=<C-r>=%filetype<CR> "\b<C-r><C-w>\b" <C-r>=expand("%:p:h")<CR>

" bind r to replace word under cursor
nnoremap <Leader>r :%s/\<<C-r><C-w>\>//c<Left><Left>


" 19. the swap file
set backupdir=~/.vim/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp


" 24. multi-byte characters
setglobal fileencoding=utf-8
set encoding=utf-8 nobomb

" autocmds
if has("autocmd")
    " crontab -e
    au BufNewFile,BufRead crontab.* set nobackup | set nowritebackup

    " json is javascript
    autocmd BufNewFile,BufRead *.json setfiletype json syntax=javascript
    
    autocmd FileType python setlocal ts=4 sts=4 sw=4 expandtab
    autocmd FileType make setlocal ts=4 sts=4 sw=4 noexpandtab
    autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
    autocmd FileType html setlocal ts=2 sts=2 sw=2 expandtab
    autocmd FileType htm setlocal ts=2 sts=2 sw=2 expandtab
    autocmd FileType java setlocal omnifunc=javacomplete#Complete

    if filereadable(expand("~/.vim/bundle/HTML-AutoCloseTag/ftplugin/html_autoclosetag.vim"))
        autocmd FileType html,htm,xhtml,xml source ~/.vim/bundle/HTML-AutoCloseTag/ftplugin/html_autoclosetag.vim
    end
end


" Load Vundle
filetype off " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'

" www.vim.org/scripts
Bundle 'AutoTag'
set tags=./tags;/


Bundle 'HTML-AutoCloseTag'
" Disable:
" let b:mapped_auto_closetag = 1


Bundle 'Colour-Sampler-Pack'
Bundle '256-grayvim'
Bundle 'blacklight'
Bundle 'MochaLatte'
Bundle 'blerins/flattown'
Bundle 'gregsexton/Atom'
Bundle 'nice/sweater'
Bundle 'zefei/cake16'
Bundle 'zeis/vim-kolor'

if has("unix")
    let s:uname = system("uname -s")
    " inkpot liquidcarbon kolor desert256 dante navajo papayawhip
    if s:uname =~ "Darwin"
        colorscheme kolor
    else
        colorscheme navajo 
    endif
endif
call SetCursorLineColors()


" GitHub
Bundle 'Townk/vim-autoclose'
" Disable: 
" let g:loaded_AutoClose = 1


Bundle 'kien/ctrlp.vim'
" CtrlP
let g:loaded_ctrlp = 0
set runtimepath^=~/.vim/bundle/ctrlp
map <unique> <Leader>s :CtrlP<CR>
let g:ctrlp_map = '<F7>'
let g:ctrlp_cmd = 'CtrlP'


Bundle 'scrooloose/nerdcommenter'


Bundle 'scrooloose/nerdtree'
" NERDtree settings
"let NERDTreeWinPos='right'
nnoremap <silent> <F8> :NERDTreeToggle<CR>
map <unique> <Leader>p :NERDTreeToggle<CR>
nmap <Leader>ps :NERDTreeFind<CR>
let NERDTreeIgnore=['\.pyc', '\.pyo', '\~$', '\.o$', '\.class$']
let NERDTreeQuitOnOpen=1
let NERDChristmasTree=1
let NERDTreeHighlightCursorline=1


Bundle 'SirVer/ultisnips'
Bundle 'honza/vim-snippets'
Bundle 'scrooloose/syntastic'


Bundle 'davidoc/taskpaper.vim'
" Taskpaper
" let g:task_paper_styles={'done': 'ctermfg=208 ctermbg=208', 'today': 'ctermfg=92 ctermbg=59', 'progress': '', 'highlight': 'term=bold ctermfg=DarkBlue ctermbg=LightYellow' }
" command! -nargs=+ HiLink hi def link <args>
" HiLink taskpaperListItem    Comment
" HiLink taskpaperDone        Identifier
" HiLink taskpaperComment     NonText
" delcommand HiLink '


Bundle 'altercation/vim-colors-solarized'
let g:solarized_termtrans=0
let g:solarized_termcolors=256
let g:solarized_visibility="high"
let g:solarized_contrast="normal"


Bundle 'edsono/vim-matchit'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-surround'

Bundle 'majutsushi/tagbar'
let g:tagbar_autoclose = 1
let g:tagbar_showlinenumbers = 1
map <silent> <Leader>t :TagbarToggle<CR>
nmap <silent> <F9> :TagbarToggle<CR>

" Dash.app
Bundle 'rizzatti/funcoo.vim'
Bundle 'rizzatti/dash.vim'
:nmap <silent> <Leader>h <Plug>DashSearch


Bundle 'jacekd/vim-iawriter'
function! IAWriter()
    colorscheme iawriter
    set background=light
    set gfn=Cousine:h12         " font to use
    set guioptions-=r           " remove right scrollbar
    set laststatus=0            " don't show status line
    set noruler                 " don't show ruler
    set nonu                    " don't show line numbers
    set linebreak               " break the lines on words
    " set linespace=5
    " set fuoptions=bacground:#00f5f6f6   " macvim specific setting for editor's background color
    " set fullscreen                      " go to fullescreen editing mode
endfunction

"if has("autocmd")
"    " turn-on distraction free writing mode for markdown files
"    au BufNewFile,BufRead *.{md,mdown,mkd,mkdn,markdown,mdwn} call IAWriter()
"endif

Bundle 'bling/vim-airline'
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


Bundle 'Lokaltog/vim-easymotion'
map ' <Plug>(easymotion-prefix)
nmap 's <Plug>(easymotion-s2)
nmap 'S <Plug>(easymotion-s)


Bundle 'corntrace/bufexplorer'
Bundle 'airblade/vim-gitgutter'
let g:gitgutter_max_signs = 250


Bundle 'xolox/vim-misc'
Bundle 'xolox/vim-easytags'
let g:easytags_cmd = '/usr/local/bin/ctags'
let g:easytags_async = 1
let g:easytags_dynamic_files = 1
let g:easytags_auto_highlight = 0
let g:easytags_include_members = 1
let g:easytags_updatetime_min = 60000


Bundle 'mileszs/ack.vim'
Bundle 'rking/ag.vim'


" OmniCompletion
Bundle 'OmniCppComplete'


Bundle 'davidhalter/jedi-vim'
let g:jedi#use_splits_not_buffers = "top"
let g:jedi#popup_on_dot = 0
"let g:jedi#show_call_signatures = 1
let g:jedi#completions_command = "<C-Space>"
let g:jedi#goto_assignments_command = "<Leader>pa"
let g:jedi#goto_definitions_command = "<Leader>pd"
let g:jedi#documentation_command = "<Leader>ph"
let g:jedi#usages_command = "<Leader>pn"
let g:jedi#rename_command = "<Leader>pr"
"let g:jedi#completions_enabled = 0


Bundle 'VictorDenisov/javacomplete'


Bundle 'VimClojure'
let vimclojure#SetupKeyMap = 0


Bundle 'jnwhiteh/vim-golang'
set runtimepath+=$GOROOT/misc/vim
autocmd FileType go autocmd BufWritePre <buffer> Fmt


Bundle 'derekwyatt/vim-scala'

" Node.js https://github.com/joyent/node/wiki/Vim-Plugins

filetype plugin indent on " required!

" Old, unused 
" Bundle 'AutoClose'
" Disable:
" let g:autoclose_loaded = 1
" let g:autoclose_on = 1

" Bundle 'project.tar.gz'
" Bundle 'msanders/snipmate.vim'
" Bundle 'thisivan/vim-taglist'
"let Tlist_Ctags_Cmd='/usr/local/bin/ctags'
"let Tlist_Show_One_File=1
"let Tlist_Auto_Highlight_Tag=1
"let Tlist_Use_Right_Window=0
"let Tlist_Close_On_Select=1
"let Tlist_GainFocus_On_ToggleOpen=1
"let Tlist_Sort_Type="name"
"nnoremap <silent> <F9> :TlistToggle<CR>
"map <silent> <Leader>t :TlistToggle<CR>

" Bundle 'git://git.wincent.com/command-t.git'
" CommandT settings
" disabled; using CtrlP
" let g:command_t_loaded = 1
" map <unique> <Leader>t :CommandT<CR>
" let g:CommandTCancelMap='<C-x>'

" PeepCode
" Bundle 'mrchrisadams/vim-peepopen'
" let g:peepopen_loaded = 1  "disabled

" Markdown
" Bundle 'file:///Users/alex/.dotfiles/...'
" Bundle 'tpope/vim-markdown'
" Bundle 'plasticboy/vim-markdown'
" Bundle 'jtratner/vim-flavored-markdown'
" Bundle 'greyblake/vim-preview'
" Bundle 'waylan/vim-markdown-extra-preview'

" Bundle 'Vim-JDE'
" Bundle 'javacomplete'
"
" YouCompleteMe requires the same version of
" Python to be used for vim, MacVim, and itself
" both at compile time and runtime 
" Bundle 'Valloric/YouCompleteMe'
" let g:ycm_auto_trigger = 0
