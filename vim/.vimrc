set nocompatible
filetype off

if has('vim_starting')
  set runtimepath+=~/.vim/bundle/neobundle.vim
  call neobundle#begin(expand('~/.vim/bundle/'))
endif
runtime bundle/vim-pathogen/autoload/pathogen.vim

" originalrepos on github
NeoBundle 'Shougo/neobundle.vim'
NeoBundle 'VimClojure'
NeoBundle 'Shougo/vimshell'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/neosnippet'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'jpalardy/vim-slime'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'kana/vim-smartinput'
""NeoBundle 'https://bitbucket.org/kovisoft/slimv'
NeoBundle 'thinca/vim-quickrun'
NeoBundle 'nathanaelkane/vim-indent-guides'
NeoBundle 'Yggdroot/indentLine'
"NeoBundle 'andviro/flake8-vim'
NeoBundle 'cohalz/c.vim'
NeoBundle 'Shougo/vimfiler'
NeoBundle 'vim-scripts/java.vim'
NeoBundle 'davidhalter/jedi-vim'
NeoBundle 'Shougo/neocomplete'

let g:quickrun_config={'*': {'split': ''}}
""set list lcs=tab:\|\  
let g:indentLine_color_term = 240
""let g:indentLine_color_gui = '#AAAAAA'
let g:indentLine_color_gui = 'gray'
let g:indentLine_char = '|'

""Vimfiler
autocmd VimEnter * VimFiler -split -simple -winwidth=30 -no-quit
let g:vimfiler_safe_mode_by_default = 0
let g:unite_enable_start_insert = 0
""autocmd BufNewFile *.c 0r $HHOME/.vim/template/c.txt
""autocmd BufNewFile *.vim 0r  put=''

""neocomplete
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_ignore_case = 1
let g:neocomplete#enable_smart_case = 1


filetype plugin indent on     " required!
filetype indent on
syntax on
let &t_ti.="\e[1 q"
let &t_SI.="\e[1 q"
let &t_EI.="\e[1 q"
let &t_te.="\e[1 q"
""inoremap { {}<LEFT>
""inoremap [ []<LEFT>
""inoremap ( ()<LEFT>
""inoremap " ""<LEFT>
""inoremap ' ''<LEFT>
""vnoremap { "zdi^V{<C-R>z}<ESC>
""vnoremap [ "zdi^V[<C-R>z]<ESC>
""vnoremap ( "zdi^V(<C-R>z)<ESC>
""vnoremap " "zdi^V"<C-R>z^V"<ESC>
""vnoremap ' "zdi'<C-R>z'<ESC>
set tabstop=4
set shiftwidth=4
set autoindent
set backspace=start,eol,indent
set fileencoding=utf-8
set number
set et
set splitbelow

NeoBundleCheck
call neobundle#end()
