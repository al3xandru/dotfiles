" Vim filetype plugin
" Language:		Markdown
" Maintainer:		Tim Pope <vimNOSPAM@tpope.org>

if exists("b:did_ftplugin")
  finish
endif

runtime! ftplugin/html.vim ftplugin/html_*.vim ftplugin/html/*.vim
unlet! b:did_ftplugin

setlocal comments=fb:*,fb:-,fb:+,n:> commentstring=>\ %s
setlocal formatoptions+=tcqln
setlocal formatlistpat=^\\s*\\d\\+\\.\\s\\+\\\|^[-*+]\\s\\+

let b:undo_ftplugin .= "|setl cms< com< fo<"

"
" Inspired by textile.vim by Tim Harper (tim.theenchanter.com)
"

command! -nargs=0 MarkdownToFile call MarkdownBufferToFile()
command! -nargs=0 MarkdownToTab call MarkdownBufferToTab()
command! -nargs=0 MarkdownPreview call MarkdownBufferPreview()
command! -nargs=0 MarkdownPublish call MarkdownPublishBuffer()

noremap <buffer> <LocalLeader>mkp :MarkdownPreview<CR>
noremap <buffer> <LocalLeader>mkf :MarkdownToFile<CR>
noremap <buffer> <LocalLeader>mkt :MarkdownToTab<CR>
noremap <buffer> <LocalLeader>mkpub :MarkdownPublish<CR>

cnoreabbrev mkpub MarkdownPublish
cnoreabbrev mkp MarkdownPreview
cnoreabbrev mkt MarkdownToTab
cnoreabbrev mkf MarkdownToFile
setlocal ignorecase
setlocal wrap
setlocal lbr

function! ToMarkdown(opts)
  let text = join(getbufline(bufname("%"), 1, '$'), "\n")
  let cmd = "markup.sh " . a:opts . " -"
  let html = system(cmd, text)
  return html
endfunction

function! MarkdownBufferPreview()
  let opts = "--type=m1 --browser"
  let outp = ToMarkdown(opts)
  echo outp
endfunction

function! MarkdownBufferToFile()
  let filename = input("Filename:", substitute(bufname("%"), "$", ".html", ""), "file")
  let opts = "--type=m1 -o " . filename
  call ToMarkdown(opts)
  echo "Rendered to '" . filename . "'"
endfunction

function! MarkdownBufferToTab()
  let options = "--type=m1 --console"
  let html = ToMarkdown(options)
  tabnew
  call append("^", split(html, "\n"))
  set syntax=html
endfunction

function! MarkdownPublishBuffer()
  let opts = "--type=m1 --tumblr"
  let html = ToMarkdown(opts)
  echo html
endfunction


" vim:set sw=2:
