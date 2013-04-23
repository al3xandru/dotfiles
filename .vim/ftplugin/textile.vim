" textile.vim
"
" Tim Harper (tim.theenchanter.com)

command! -nargs=0 TextileRenderFile call TextileRenderBufferToFile()
command! -nargs=0 TextileRenderTab call TextileRenderBufferToTab()
command! -nargs=0 TextilePreview call TextileRenderBufferToPreview()
noremap <buffer> <LocalLeader>ttp :TextilePreview<CR>
noremap <buffer> <LocalLeader>ttf :TextileRenderFile<CR>
noremap <buffer> <LocalLeader>ttt :TextileRenderTab<CR>
setlocal ignorecase
setlocal wrap
setlocal lbr

function! TextileRender(lines)
  if (system('which ruby') == "")
    throw "Could not find ruby!"
  end

  let text = join(a:lines, "\n")
  let html = system("ruby -e \"def e(msg); puts msg; exit 1; end; begin; require 'rubygems'; rescue LoadError; e('rubygems not found'); end; begin; require 'redcloth'; rescue LoadError; e('RedCloth gem not installed.  Run this from the terminal: sudo gem install RedCloth'); end; puts(RedCloth.new(\\$stdin.read).to_html(:textile))\"", text)
  return html
endfunction

function! TextileRenderFile(lines, filename)
  let html = TextileRender(getbufline(bufname("%"), 1, '$'))
  let html = "<html><head><meta http-equiv='Content-Type' content='text/html; charset=utf-8'/><link rel='stylesheet' href='file:///Users/alex/.vim/markdownpreview_plugin/markdownprev2.css'></link><title>" . bufname("%") . "</title><body><div id='article'><div class='page'>\n" . html . "\n</div></div></body></html>"
  return writefile(split(html, "\n"), a:filename)
endfunction

function! TextileRenderBufferToPreview()
  let filename = "/tmp/textile-preview.html"
  call TextileRenderFile(getbufline(bufname("%"), 1, '$'), filename)

  " Modify this line to make it compatible on other platforms
  call system("open -a Safari ". filename)
endfunction

function! TextileRenderBufferToFile()
  let filename = input("Filename:", substitute(bufname("%"), "textile$", "html", ""), "file")
  call TextileRenderFile(getbufline(bufname("%"), 1, '$'), filename)
  echo "Rendered to '" . filename . "'"
endfunction

function! TextileRenderBufferToTab()
  let html = TextileRender(getbufline(bufname("%"), 1, '$'))
  tabnew
  call append("^", split(html, "\n"))
  set syntax=html
endfunction

