call plug#begin(stdpath('data') . '/plugged')
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'dracula/vim'
Plug 'ryanoasis/vim-devicons'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'scrooloose/nerdtree'
Plug 'preservim/nerdcommenter'
Plug 'mhinz/vim-startify'
Plug 'neoclide/coc.nvim', {'branch': 'release'}

Plug 'cocopon/iceberg.vim'

Plug 'Chiel92/vim-autoformat'

Plug 'neovim/nvim-lspconfig'
Plug 'ray-x/guihua.lua', {'do': 'cd lua/fzy && make'}
Plug 'ray-x/navigator.lua'

Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
"Plug 'nvim-treesitter/nvim-treesitter-refactor' "this provides "go to def" etc
call plug#end()

lua << EOF
require'navigator'.setup()
EOF

" Coc keybindings
map <Leader>ggd <Plug>(coc-definition)
map <Leader>ggi <Plug>(coc-implementation)
map <Leader>ggt <Plug>(coc-type-definition)
map <Leader>gh :call CocActionAsync('doHover')<cr>
map <Leader>gn <Plug>(coc-diagnostic-next)
map <Leader>gp <Plug>(coc-diagnostic-prev)
map <Leader>gr <Plug>(coc-references)

map <Leader>rn <Plug>(coc-rename)
map <Leader>rf <Plug>(coc-refactor)
map <Leader>qf <Plug>(coc-fix-current)

map <Leader>al <Plug>(coc-codeaction-line)
map <Leader>ac <Plug>(coc-codeaction-cursor)
map <Leader>ao <Plug>(coc-codelens-action)

nnoremap <Leader>kd :<C-u>CocList diagnostics<Cr>
nnoremap <Leader>kc :<C-u>CocList commands<Cr>
nnoremap <Leader>ko :<C-u>CocList outline<Cr>
nnoremap <Leader>kr :<C-u>CocListResume<Cr>

" Trigger autocompletion on <C-Space>
inoremap <silent><expr> <c-space> coc#refresh()
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<CR>"

autocmd CursorHold * silent call CocActionAsync('highlight')
autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')

" Use <Tab> and <S-Tab> to navigate the completion list
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Disable Coc for agda files
autocmd BufNewFile,BufRead *.agda execute 'CocDisable'

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline=%t%m%r%{coc#status()}%{get(b:,'coc_current_function','')}

set updatetime=200
set shada='1000,f1,<500
set relativenumber
set scrolloff=5
set autoindent
set ruler
set showcmd
set showmode
set list
set splitright
set splitbelow
set listchars=tab:>-
set showmatch
set ignorecase
set hlsearch
set incsearch
set tabstop=4
set softtabstop=4
set expandtab
set shiftwidth=4
set number
set wildmode=longest,list
set mouse=a
set cursorline
syntax on
filetype plugin indent on
filetype plugin on

" Set font
set guifont=Monospace:h10

" move line or visually selected block - alt+j/k
inoremap  <A-j> <Esc>:m .+1<CR>==gi
inoremap  <A-k> <Esc>:m .-2<CR>==gi
vnoremap  <A-j> :m '>+1<CR>gv=gv
vnoremap  <A-k> :m '<-2<CR>gv=gv

" move split panes to left/bottom/top/right
nnoremap <A-h> <C-W>H
nnoremap <A-j> <C-W>J
nnoremap <A-k> <C-W>K
nnoremap <A-l> <C-W>L

" move between panes to left/bottom/top/right
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
:tnoremap <C-h> <C-\><C-N><C-w>h
:tnoremap <C-j> <C-\><C-N><C-w>j
:tnoremap <C-k> <C-\><C-N><C-w>k
:tnoremap <C-l> <C-\><C-N><C-w>l
:inoremap <C-h> <C-\><C-N><C-w>h
:inoremap <C-j> <C-\><C-N><C-w>j
:inoremap <C-k> <C-\><C-N><C-w>k
:inoremap <C-l> <C-\><C-N><C-w>l

" open file in a text by placing text and gf
nnoremap gf :vert winc f<CR>

" copies filepath to clipboard by pressing yf
:nnoremap <silent> yf :let @+=expand('%:p')<CR>

" copies pwd to clipboard: command yd
:nnoremap <silent> yd :let @+=expand('%:p:h')<CR>

" open fzf file explorer
nnoremap <C-p> :GFiles<CR>
"nnoremap <C-P> :Files<CR>

" Esc in terminal mode
:tnoremap <Esc> <C-\><C-n>

" Ctrl+C and Ctrl+V
inoremap <C-c> <Esc>"+y<CR>i
inoremap <C-v> <Esc>"+p<CR>i
"nnoremap <C-c> "+y<CR>
"nnoremap <C-v> "+p<CR>
vnoremap <C-c> "+y<CR>
vnoremap <C-v> "+p<CR>
"tnoremap <C-c> <C-\><C-n>"+y<CR>i
tnoremap <C-v> <C-\><C-n>"+p<CR>i

" NERDTree
nnoremap <leader>n :NERDTreeFocus<CR>
nnoremap <C-n> :NERDTree<CR>
nnoremap <C-t> :NERDTreeToggle<CR>
nnoremap <C-f> :NERDTreeFind<CR>

" Format
nnoremap <C-I> :Autoformat<CR>

" Format on save
" au BufWrite * :Autoformat

" Vim jump to the last position when reopening a file
if has("autocmd")
    au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
                \| exe "normal! g'\"" | endif
endif

" Disable highlight until search
:noh

" Colors
colorscheme iceberg
hi Pmenu guibg=Black
