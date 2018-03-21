filetype off " required for Vundle

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle. Required!
Bundle 'gmarik/Vundle.vim'

" Language, framework, and tool support
Bundle 'kchmck/vim-coffee-script'
Bundle 'pangloss/vim-javascript'
Bundle 'mxw/vim-jsx'
Bundle 'tpope/vim-rails'
Bundle 'elixir-lang/vim-elixir'
Bundle 'wlangstroth/vim-racket'
Bundle 'vim-syntastic/syntastic'
Bundle 'slim-template/vim-slim'
Bundle 'tpope/vim-projectionist'
Bundle 'tpope/vim-dispatch'
Bundle 'tpope/vim-classpath'
Bundle 'tpope/vim-fireplace'
Bundle 'tpope/vim-repeat'
Bundle 'guns/vim-clojure-highlight'
Bundle 'guns/vim-clojure-static'
Bundle 'sjl/tslime.vim'

" Miscellaneous bundles
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-fugitive'
Bundle 'scrooloose/nerdtree'
Bundle 'tpope/vim-commentary'
Bundle 'terryma/vim-multiple-cursors'
Bundle 'AndrewRadev/linediff.vim'
Bundle 'tpope/vim-surround'
Bundle 'airblade/vim-gitgutter'
Bundle 'junegunn/fzf'
Bundle 'junegunn/fzf.vim'
Bundle 'rking/ag.vim'
Bundle 'tmhedberg/matchit'
Bundle 'ruby-matchit'
Bundle 'AnsiEsc.vim'
Bundle 'xmledit'
Bundle 'nathanaelkane/vim-indent-guides'
Bundle 'vim-niji'
Bundle 'paredit.vim'
Bundle 'junegunn/vim-easy-align'
Bundle 'godlygeek/tabular'
Bundle 'plasticboy/vim-markdown'

" Color schemes
Bundle 'nanotech/jellybeans.vim'

call vundle#end() " required by Vundle
filetype plugin indent on " required by Vundle

let mapleader = ','

try
  colorscheme jellybeans
catch " deal with it
endtry

imap kj <Esc>
map <Space> :

"" Windows
" Traversing windows
map <Down> <C-W>j
map <Up> <C-W>k
map <Right> <C-W>l
map <Left> <C-W>h

" Resizing panes
map <C-l> :30winc ><CR>
map <C-h> :30winc <<CR>
map <C-j> :20winc +<CR>
map <C-k> :20winc -<CR>

" Tagbar
map <leader>t :TagbarToggle<CR>

" Traversing tabs
imap <D-1> <ESC>1gt
imap <D-2> <ESC>2gt
imap <D-3> <ESC>3gt
imap <D-4> <ESC>4gt
imap <D-5> <ESC>5gt
imap <D-6> <ESC>6gt
imap <D-7> <ESC>7gt
imap <D-8> <ESC>8gt
imap <D-9> <ESC>9gt
nmap <D-1> 1gt
nmap <D-2> 2gt
nmap <D-3> 3gt
nmap <D-4> 4gt
nmap <D-5> 5gt
nmap <D-6> 6gt
nmap <D-7> 7gt
nmap <D-8> 8gt
nmap <D-9> 9gt

" NERDTree
map <leader>d :NERDTreeToggle<CR>:set rnu<CR>
map <leader>f :NERDTreeFind<CR>:set rnu<CR>

map <leader>n :set nornu number<CR>
map <leader>r :set rnu nonumber<CR>
map <leader>= <C-w>=
map <leader>b :Gblame<CR>
map <leader>v :tabedit $MYVIMRC<CR>

map <leader>s :Ag <C-r><C-w><CR>
map <leader>c :tabc<CR>

"" Vim EasyAlign
" Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
xmap <Enter> <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" CtrlP
" let g:ctrlp_custom_ignore = '\vcoverage\/'

" The Silver Searcher
" if executable('ag')
"   let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
"   let g:ctrlp_use_caching = 0 " ag is fast; we don't need to cache
" endif
map <C-p> :Files<CR>

fun! <SID>StripTrailingWhitespaces()
    let line = line(".")
    let column = col(".")
    %s/\s\+$//e
    call cursor(line, column)
endfun

if has("autocmd")
  "Strip trailing whitespace when saving"
  autocmd BufWritePre * :call <SID>StripTrailingWhitespaces()
endif

if has("gui_running")
  set guioptions=egmrt
  colorscheme desert
endif

set softtabstop=2 shiftwidth=2 expandtab
set exrc " enable per-directory .vimrc files
set nocompatible
set lbr! " Wrap lines at word
set clipboard=unnamed " Share system clipboard.
set showmatch " show matching brackets
set autoindent
set smartindent
set guifont=Monaco:h14 " change the font in the GUI
set guioptions-=T " Get rid of the annoying toolbar
set diffopt+=iwhite " Ignore whitespace when diffing
set textwidth=0 " This should default to 0
set wrapmargin=0 " This should default to 0
set relativenumber
set backupdir=~/tmp
set directory=~/tmp
set undodir=~/tmp
set nowritebackup
set noundofile " this should have been off by default...

syntax on " syntax highlighting on

" Automatically fix a misspelling
ab teh the
ab boostrap bootstrap

" Source files. Refactor this.
source $VIMRUNTIME/vimrc_example.vim

if filereadable($HOME . '/.host_vimrc')
  source $HOME/.host_vimrc " Keep computer-specific Vim commands here
endif

" vp doesn't replace paste buffer
function! RestoreRegister()
  let @" = s:restore_reg
  return ''
endfunction
function! s:Repl()
  let s:restore_reg = @"
  return "p@=RestoreRegister()\<cr>"
endfunction
vmap <silent> <expr> p <sid>Repl()

" Syntastic
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_javascript_checkers = ['eslint']

let g:jsx_ext_required = 0

" Markdown
let g:vim_markdown_folding_disabled = 1

" Groovy
au BufNewFile,BufRead Jenkinsfile setf groovy
