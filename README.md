My Emacs Configuration
======================

This time around I will set stuff up with Cask.

I have been looking at the [Emacs configuration](https://github.com/tonini/emacs.d/) by [Samuel Tonini](https://github.com/tonini), and some parts of his config has been lifted into this one.

Requirements
------------

  * Emacs 24+
  * [Cask](https://github.com/cask/cask) for handling dependencies


Installation
------------
### Fetch dependencies
Use Git to clone the repo into ~/.emacs.d and install the dependencies using `cask install` in the newly cloned folder.

### Elixir
The setup require emacs-elixir and alchemist.el to be cloned into the ~/Projects directory.

### Calendar and Diary
Calendar needs a diary file. Write `touch ~/.diary` in the terminal to generate one.

The calendar location is set to Copenhagen, Denmark.