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
call pathogen#infect()

set nocompatible
set backspace=2
" Allow backspace in insert mode
set backspace=indent,eol,start
" Use the OS clipboard by default (on versions compiled with `+clipboard`)
set clipboard=unnamed
" Enhance command-line completion
set wildmenu
" Allow cursor keys in insert mode
set esckeys


" 2. moving around, searching and patterns "
set incsearch
set hlsearch
set wildignore+=*.o,*.obj,.git,.svn,*.class,*.pyo,*.pyc

" 4. displaying text
filetype on
filetype plugin on
filetype plugin indent on

set listchars=tab:▸\ ,trail:·,eol:¬


" 5. syntax, highlighting and spelling"
" Dark: darkblue, koehler, vividchalk, vibrantink, molokai, tango, relaxedgreen
" Light: buttercream, papayawhip, winter
" Grey: camo, earendel, inkpot, lucius
" Pastel: desert256 jellybeans wombat256 ir_black molokai

syntax on
colorscheme desert256 

let g:solarized_termtrans=1
let g:solarized_termcolors=16
let g:solarized_visibility="high"
let g:solarized_contrast="high"

set thesaurus+=~/.vim/mthesaur.txt


" 6. multiple windows "
set title
set statusline=%t\ %l,%v%=[b%n\ %L:%p%%\ %Y]%<\ [a\%03.3b:h\%02.2B]
hi StatusLine ctermbg=59 ctermfg=69 

" GUI "
set guioptions=aAce
set selection=exclusive
set gfn=Consolas:h12
if has("gui_macvim")
    set columns=125
    set lines=50
end

" do not display the menu bar
" set go-=T
"set gfn=Inconsolata:h13
"set gfn=Bitstream\ Vera\ Sans\ Mono:h11
"set gfn=Anonymous\ Pro:h12
"set columns=150

"if (has("gui_macvim") || &t_Co == 256)
"    colorscheme desert256
"end

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
let mapleader=","

" Opens an edit command with the path of the currently edited file filled in
" Normal mode: <Leader>e
map <Leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

" Opens a tab edit command with the path of the currently edited file filled in 
" Normal mode: <Leader>t
map <Leader>te :tabe <C-R>=expand("%:p:h") . "/" <CR>

" Inserts the path of the currently edited file into a command
" Command mode: Ctrl+P
cmap <C-P> <C-R>=expand("%:p:h") . "/" <CR>
map <silent> <Leader>pp <C-R>=expand("%:p:h") . "/" <CR>

" Insert a newline in normal mode
nnoremap <NL> i<CR><ESC>

" Show special characters
nmap <silent> <leader>ss :set nolist!<CR>


" 19. the swap file
set backupdir=~/.vim/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp


" 24. multi-byte characters
setglobal fileencoding=utf-8
set encoding=utf-8 nobomb

" autocmds
if has("autocmd")
    autocmd FileType python setlocal ts=4 sts=4 sw=4 expandtab
    autocmd FileType make setlocal ts=4 sts=4 sw=4 noexpandtab
    autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
    autocmd FileType html setlocal ts=2 sts=2 sw=2 expandtab
    autocmd FileType htm setlocal ts=2 sts=2 sw=2 expandtab
end

function! IAWriter()
    colorscheme iawriter
    set background=light
    set gfn=Cousine:h14         " font to use
    " set lines=40 columns=100    " size of the editable area
    set guioptions-=r           " remove right scrollbar
    set laststatus=0            " don't show status line
    set noruler                 " don't show ruler
    set nonu                    " don't show line numbers
    set linebreak               " break the lines on words
    " set fuoptions=bacground:#00f5f6f6   " macvim specific setting for editor's background color
    " set fullscreen                      " go to fullescreen editing mode
endfunction

if has("autocmd")
    " turn-on distraction free writing mode for markdown files
    au BufNewFile,BufRead *.{md,mdown,mkd,mkdn,markdown,mdwn} call IAWriter()

    " crontab -e
    au BufNewFile,BufRead crontab.* set nobackup | set nowritebackup

    " json is javascript
    autocmd BufNewFile,BufRead *.json setfiletype json syntax=javascript
endif

" NERDtree settings
"let NERDTreeWinPos='right'
nnoremap <silent> <F8> :NERDTreeToggle<CR>
map <Leader>p :NERDTreeToggle<CR>
let NERDTreeIgnore=['\.pyc', '\.pyo', '\~$', '\.o$', '\.class$']
let NERDTreeQuitOnOpen=1
let NERDChristmasTree=1
let NERDTreeHighlightCursorline=1

" Taglist settings
let Tlist_Ctags_Cmd='/usr/local/bin/ctags'
let Tlist_Show_One_File=1
let Tlist_Auto_Highlight_Tag=1
let Tlist_Use_Right_Window=0
let Tlist_Close_On_Select=0
let Tlist_GainFocus_On_ToggleOpen=1
let Tlist_Sort_Type="name"
nnoremap <silent> <F9> :TlistToggle<CR>
map <silent> <Leader>s :TlistToggle<CR>

" CommandT settings
let g:CommandTCancelMap='<C-x>'

" PeepCode
" disabled
let g:peepopen_loaded = 1

" Taskpaper
" let g:task_paper_styles={'done': 'ctermfg=208 ctermbg=208', 'today': 'ctermfg=92 ctermbg=59', 'progress': '', 'highlight': 'term=bold ctermfg=DarkBlue ctermbg=LightYellow' }
" command! -nargs=+ HiLink hi def link <args>
" HiLink taskpaperListItem    Comment
" HiLink taskpaperDone        Identifier
" HiLink taskpaperComment     NonText
" delcommand HiLink
