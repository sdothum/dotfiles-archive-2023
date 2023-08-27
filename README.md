# dotfiles

These are the dotfiles referenced in
[the darnedest thing](http://thedarnedestthing.com/colophon), and then
some.

**!! NOTE !!**
Do to the rather large patch file size of this repo, it is now frozen. "dotfiles" is now continued in the "[stow](https://github.com/sdothum/stow)" repo.


## installation

<pre>git clone --depth 1 https://github.com/sdothum/dotfiles.git</pre>

**--depth 1** ensures you do not download the unnecessary patch history
which is extremely large. 

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

## beakl

There are several keyboard layouts for the Planck, Splitography, Chimera and
Corne keyboards. 

**[BEAKL Wi](http://thedarnedestthing.com/beakl%20wi)** is the current keyboard in daily use (hence, has the most
up-to-date libraries).

What this means is that,
while all layouts should compile and flash their appropriate
keyboards properly, older source (layouts) libraries -- in
particular, the *keycode_functions.c and tapdance.c* files -- will not incorporate
the latest tweaks and fixes to corner case behaviours caught over the
course of this site's BEAKL development. This does not mean
the older layouts do not work -- they do (with probably the majority of corner cases addressed
not likely to be noticed by the user).

See [keyboard
evaluation](http://thedarnedestthing.com/keyboard%20layout%20evaluation#beakl-weighting)
for a biased comparison of keyboard layouts!

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
