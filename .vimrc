filetype off " required!

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
Bundle 'briancollins/vim-jst'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-rails.git'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/nerdcommenter'
Bundle 'tpope/vim-cucumber'
Bundle 'godlygeek/tabular'
Bundle 'Raimondi/delimitMate'
Bundle 'jc00ke/thor.vim'
Bundle 'tsaleh/vim-matchit'
" Bundle 'Lokaltog/vim-easymotion'
" Bundle 'rstacruz/sparkup', {'rtp': 'vim/'}

" vim-scripts repos
Bundle 'taglist.vim'
Bundle 'ruby-matchit'
" Bundle 'L9'
" Bundle 'FuzzyFinder'

" non github repos
Bundle 'git://git.wincent.com/command-t.git'
" Use control P instead someday

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

" Traversing tabs
nmap <D-1> 1gt
nmap <D-2> 2gt
nmap <D-3> 3gt
nmap <D-4> 4gt
nmap <D-5> 5gt
nmap <D-6> 6gt
nmap <D-7> 7gt
nmap <D-8> 8gt
nmap <D-9> 9gt
imap <D-1> <ESC>1gt
imap <D-2> <ESC>2gt
imap <D-3> <ESC>3gt
imap <D-4> <ESC>4gt
imap <D-5> <ESC>5gt
imap <D-6> <ESC>6gt
imap <D-7> <ESC>7gt
imap <D-8> <ESC>8gt
imap <D-9> <ESC>9gt

" NERDTree
imap <D-d> <ESC>:NERDTreeToggle<CR>i
map <D-d> :NERDTreeToggle<CR>
map <leader>d :NERDTreeToggle<CR>

" Tlist
map <D-l> :TlistToggle<CR>
map <leader>l :TlistToggle<CR>

map <leader>v :tabedit $MYVIMRC<CR>
let mapleader = '\'

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
set clipboard+=unnamed	" Share Windows clipboard
set showmatch " show matching brackets
set autoindent
set smartindent
set guifont=Monaco:h14 " change the font in the GUI
set guioptions-=T " Get rid of the annoying toolbar
set diffopt+=iwhite " Ignore whitespace when diffing

syntax on " syntax highlighting on

" Automatically fix a misspelling
ab teh the


" Source files
source $VIMRUNTIME/vimrc_example.vim

if filereadable($HOME . '/.host_vimrc')
  source $HOME/.host_vimrc " Keep computer-specific Vim commands here
endif

set nobackup " keep gVim from leaving ~ backup files
