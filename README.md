# dtrt-indent

An Emacs minor mode that guesses the indentation offset originally used for creating source code files and transparently adjusts the corresponding settings in Emacs, making it more convenient to edit foreign files.

You can install dtrt-indent from MELPA.

To activate it, M-x customize-variable dtrt-indent-global-mode, and turn on
"Dtrt Indent Global Mode". See `dtrt-indent.el` for full documentation.

# Related work

[guess-style](https://nschum.de/src/emacs/guess-style/) is similar to dtrt-indent, but only supports `cc-mode`, and only guesses offsets of 2, 4 and 8.

[vim-sleuth](https://github.com/tpope/vim-sleuth) provides similar functionality for Vim.
