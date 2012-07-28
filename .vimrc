colorscheme newdesert

imap kk <Esc>
imap <D-d> <ESC>:NERDTreeToggle<CR>i
map <Space> :
map <Down> <C-W>j
map <Up> <C-W>k
map <Right> <C-W>l
map <Left> <C-W>h
map <D-d> :NERDTreeToggle<CR>
map <leader>d :NERDTreeToggle<CR>
map <D-c> :TlistToggle<CR>
map <leader>c :TlistToggle<CR>
map <leader>v :tabedit $MYVIMRC<CR>

set softtabstop=2 shiftwidth=2 expandtab
set exrc " enable per-directory .vimrc files

let mapleader = '\'


filetype plugin indent on

call pathogen#infect()

" Source the vimrc file after saving it
if has("autocmd")
  autocmd BufWritePost .vimrc source $MYVIMRC
endif

if has("gui_running")
  set guioptions=egmrt
  colorscheme desert
endif

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
