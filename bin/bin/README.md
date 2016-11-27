# dotfiles

These are the dotfiles referenced in
[the darnedest thing](http://thedarnedestthing.com/colophon), and then
some.

## installation

The bin directory is organized into a tree of subdirectories. The 
creation of the pseudo DSL functions for herbstluftwm was applied to the 
user bin scripts so actions such as "conky weather" or "dmenu music" 
could be incorporated into keybinds and calling scripts for better 
readability and code consolidation.

To enable the automatic construction of paths necessary to use the tree 
of bin scripts, "path.sh" which can be found in the 
build/root/etc/profile.d folder can be placed in /etc/profile.d or 
sourced manually in your shell profile.

## license

This code is distributed under the terms and conditions of the MIT
license.
