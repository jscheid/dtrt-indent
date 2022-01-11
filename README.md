# dtrt-indent

An Emacs minor mode that guesses the indentation offset originally used for creating source code files and transparently adjusts the corresponding settings in Emacs, making it more convenient to edit foreign files.

See `dtrt-indent.el` for full documentation, including installation instructions.

# Related work

[guess-style](https://nschum.de/src/emacs/guess-style/) is similar to dtrt-indent, but only guesses offsets of 2, 4 and 8, and lacks support for major modes other than `cc-mode`.

[vim-sleuth](https://github.com/tpope/vim-sleuth) provides similar functionality for Vim.
