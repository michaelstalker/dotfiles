filetype off " required! (for Bundler?)

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/vundle'

" GitHub bundles
Bundle 'mileszs/ack.vim'
Bundle 'kchmck/vim-coffee-script'
Bundle 'tpope/vim-endwise'
Bundle 'pangloss/vim-javascript'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-rails.git'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/nerdcommenter'
Bundle 'tpope/vim-cucumber'
Bundle 'godlygeek/tabular'
Bundle 'elixir-lang/vim-elixir'

" is this the same as tabular?
Bundle 'Raimondi/delimitMate'
Bundle 'jc00ke/thor.vim'
Bundle 'tsaleh/vim-matchit'
Bundle 'AndrewRadev/linediff.vim'
Bundle 'majutsushi/tagbar'
Bundle 'tpope/vim-surround'
Bundle 'kien/ctrlp.vim'

" What is this?
Bundle 'briancollins/vim-jst'
" Bundle 'Lokaltog/vim-easymotion'
" Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}

" vim-scripts repos
Bundle 'ruby-matchit'
Bundle 'vimwiki'
Bundle 'AnsiEsc.vim'
" Bundle 'L9'
" Bundle 'FuzzyFinder'

filetype plugin indent on " required!

colorscheme newdesert

imap kk <Esc>
map <Space> :

" Traversing windows
map <Down> <C-W>j
map <Up> <C-W>k
map <Right> <C-W>l
map <Left> <C-W>h
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Tagbar
map <D-t> :TagbarToggle<CR>

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
imap <D-d> <ESC>:NERDTreeToggle<CR>i
map <D-d> :NERDTreeToggle<CR>
map <leader>d :NERDTreeToggle<CR>

map <leader>v :tabedit $MYVIMRC<CR>
let mapleader = '\'

" CtrlP
map <leader>t :CtrlP<CR>
map <C-t> :CtrlP<CR>
"let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_custom_ignore = '\vcoverage\/'
"let g:ctrlp_custom_ignore = {
"  \ 'dir':  '\v^coverage\/',
"  \ }
" let g:ctrlp_regexp = 1

" Source the vimrc file after saving it
if has("autocmd")
  autocmd BufWritePost .vimrc source $MYVIMRC
endif

if has("gui_running")
  set guioptions=egmrt
  colorscheme desert
endif

set softtabstop=2 shiftwidth=2 expandtab
set exrc " enable per-directory .vimrc files
set nocompatible
set number	" Show numbers on left
set lbr!	" Wrap lines at word
set clipboard+=unnamed	" Share system clipboard. Does this work?
set showmatch " show matching brackets
set autoindent
set smartindent
set guifont=Monaco:h14 " change the font in the GUI
set guioptions-=T " Get rid of the annoying toolbar
set diffopt+=iwhite " Ignore whitespace when diffing
set textwidth=0

syntax on " syntax highlighting on

" Automatically fix a misspelling
ab teh the

" Source files. Refactor this.
source $VIMRUNTIME/vimrc_example.vim

if filereadable($HOME . '/.host_vimrc')
  source $HOME/.host_vimrc " Keep computer-specific Vim commands here
endif

set nobackup " keep gVim from leaving ~ backup files
