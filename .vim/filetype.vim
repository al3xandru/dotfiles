au BufNewFile,BufRead *.applescript set filetype=applescript
au BufNewFile,BufRead *.scpt set filetype=applescript
" markdown filetype file
"augroup markdown
  "au! BufRead,BufNewFile *.md	setfiletype md
  "au! BufRead,BufNewFile *.md	setfiletype markdown
  "au! BufRead,BufNewFile *.md set filetype=markdown
"augroup END
