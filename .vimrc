set nocompatible

" vim-plug
call plug#begin('~/.vim/plugged')
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'bling/vim-airline'
Plug 'tpope/vim-fugitive'
Plug 'scrooloose/syntastic'
Plug 'toyamarinyon/vim-swift'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-fireplace'
Plug 'vim-scripts/paredit.vim'
Plug 'anyakichi/vim-surround'
Plug 'venantius/vim-cljfmt'
Plug 'yegappan/grep'
call plug#end()

" basic tabs/spaces
set tabstop=4 " a tab is four spaces
set autoindent " enable auto indent
set shiftwidth=4 " four spaces for auto indent
set softtabstop=4
set smarttab
set expandtab
set showmatch " show matching parens
set ruler " show row/col num in bottom right
set backspace=2 " allow backspacing over everything in insert mode
set ignorecase " ignore case when searching
set smartcase " ignore case if search pattern is all lower-case
set hlsearch " highlight search terms
set incsearch " show searches as you type
set title " change the terminal's title
set undolevels=1000 " many undo
set history=1000 " remember more commands
set number " enable line numbers

" syntax highlighting and color theme
syntax enable 
set background=dark

" use TAB key for switching buffers
map <Tab> :bnext<CR>
map <S-Tab> :bprevious<CR>

" use control + direction for changing windows
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-h> <C-w>h
map <C-l> <C-w>l

" repaint (double backslash)
map <silent> <Bslash><Bslash> :nohlsearch<CR>

" prevent mouse selection of line numbers
set mouse+=a

" yank to system clipboard 
set clipboard=unnamed

" spell check
autocmd FileType latex,tex,md,markdown,gitcommit setlocal spell

" fzf custom maping (double space-bar for file search)
let mapleader = "\<space>"
nnoremap <silent> <leader><space> :FZF<CR>
nnoremap <silent> <leader>f :FZF ~<CR>

" syntastic settings
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" nerdtree directory mapping (space shift T)
map <silent> <S-T> :NERDTreeToggle<CR>

" lisp settings
let g:lisp_rainbow = 1
let g:paredit_electric_return = 0
