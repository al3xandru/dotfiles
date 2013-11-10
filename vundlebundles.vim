filetype off " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" www.vim.org/scripts
Bundle 'HTML-AutoCloseTag'
" Disable:
" let b:mapped_auto_closetag = 1


Bundle 'AutoClose'
" Disable:
let g:autoclose_loaded = 1
let g:autoclose_on = 1

Bundle 'project.tar.gz'

Bundle 'Colour-Sampler-Pack'
" Dark: darkblue, koehler, vividchalk, vibrantink, molokai, tango, relaxedgreen
" Light: buttercream, papayawhip, winter
" Grey: camo, earendel, inkpot, lucius
" Pastel: desert256 jellybeans wombat256 ir_black molokai

colorscheme desert256

let g:solarized_termtrans=1
let g:solarized_termcolors=16
let g:solarized_visibility="high"
let g:solarized_contrast="high"


" GitHub
Bundle 'gmarik/vundle'
Bundle 'Townk/vim-autoclose'
" Disable: 
" let g:loaded_AutoClose = 1


Bundle 'kien/ctrlp.vim'
" CtrlP
let g:loaded_ctrlp = 0
map <unique> <Leader>t :CtrlP<CR>
set runtimepath^=~/.vim/bundle/ctrlp
let g:ctrlp_map = '<c-t>'
let g:ctrlp_cmd = 'CtrlP'


Bundle 'scrooloose/nerdcommenter'


Bundle 'scrooloose/nerdtree'
" NERDtree settings
"let NERDTreeWinPos='right'
nnoremap <silent> <F8> :NERDTreeToggle<CR>
map <unique> <Leader>p :NERDTreeToggle<CR>
let NERDTreeIgnore=['\.pyc', '\.pyo', '\~$', '\.o$', '\.class$']
let NERDTreeQuitOnOpen=0
let NERDChristmasTree=1
let NERDTreeHighlightCursorline=1


Bundle 'msanders/snipmate.vim'
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
Bundle 'edsono/vim-matchit'
Bundle 'tpope/vim-surround'


Bundle 'thisivan/vim-taglist'
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


" PeepCode
Bundle 'mrchrisadams/vim-peepopen'
let g:peepopen_loaded = 1  "disabled


" Dash.app
Bundle 'rizzatti/funcoo.vim'
Bundle 'rizzatti/dash.vim'

" Markdown
" Bundle 'file:///Users/alex/.dotfiles/...'
" Bundle 'tpope/vim-markdown'
" Bundle 'plasticboy/vim-markdown'
" Bundle 'jtratner/vim-flavored-markdown'
" Bundle 'greyblake/vim-preview'
" Bundle 'waylan/vim-markdown-extra-preview'


" External Git
Bundle 'git://git.wincent.com/command-t.git'
" CommandT settings
" disabled; using CtrlP
let g:command_t_loaded = 1
" map <unique> <Leader>t :CommandT<CR>
" let g:CommandTCancelMap='<C-x>'





filetype plugin indent on " required!
