# dotfiles

These are the dotfiles referenced in
[the darnedest thing](http://thedarnedestthing.com/colophon), and then
some.

## installation

<pre>git clone https://github.com/sdothum/dotfiles.git</pre>

The root folder name within dotfiles is the application the dotfiles
belong to. Below that is the actual home folder path (or subfolders) to the dotfile.

The dotfiles folder is the [GNU
stow](https://www.gnu.org/software/stow/) root folder, typically
installed in $HOME/dotfiles.

## usage

**Warning:** _Backup_ your configuration files before overwriting!

Create symlinks to application dotfiles with

<pre>cd ~/dotfiles
stow &lt;application&gt;</pre>

## caveats

Many of the configuration files contain custom keybind specifications for various
applications and window managers. Note that these are tailored for the
Colemake Shift-DH layout -- a custom Colemak variant -- described in the
reference url above and use vim mappings (customized) where possible. Redefine these keybinds as required.

## contributing

1. Fork it!
2. Create your feature branch: `git checkout -b my-new-feature`
3. Commit your changes: `git commit -am 'Add some feature'`
4. Push to the branch: `git push origin my-new-feature`
5. Submit a pull request :D

## license

This code is distributed under the terms and conditions of the MIT
license.
