if exists("did_load_filetypes_userafter")
  finish
endif
let did_load_filetypes_userafter = 1
augroup filetypedetect
  au BufNewFile,BufRead *.applescript setfiletype=applescript
  " json is javascript
  autocmd BufNewFile,BufRead *.json setfiletype json syntax=javascript
  au BufNewFile,BufRead *.scpt setfiletype=applescript
augroup END
