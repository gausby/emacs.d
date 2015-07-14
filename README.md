My Emacs Configuration
======================

This time around I will set stuff up with Cask.

 * I have been looking at the [Emacs configuration](https://github.com/tonini/emacs.d/) by [Samuel Tonini](https://github.com/tonini), and some parts of his config has been lifted into this one.

 * Some bits has been taken from [Bozhidar Batsov](https://github.com/bbatsov)'s [Emacs Prelude](https://github.com/bbatsov/prelude) starter package.

Requirements
------------

  * Emacs 24+
  * [Cask](https://github.com/cask/cask) for handling dependencies


Installation
------------
### Fetch dependencies
Use Git to clone the repo into ~/.emacs.d and install the dependencies using `cask install` in the newly cloned folder.

### Elixir
The setup require emacs-elixir and alchemist.el to be cloned into the *~/Development/forks*-directory. I have some clones going there but the official repos might work just as well—or even better.

### Calendar and Diary
Calendar needs a diary file. Write `touch ~/.diary` in the terminal to generate one.

The calendar location is set to Copenhagen, Denmark.
