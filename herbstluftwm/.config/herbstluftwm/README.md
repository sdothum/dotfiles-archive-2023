# dotfiles

These are the dotfiles referenced in
[the darnedest thing](http://thedarnedestthing.com/colophon), and then
some.

## installation

The herbstluftwm directory is organized into a tree of subdirectories. 
The creation of the pseudo DSL functions for herbstluftwm was created to 
facilitate organizing the many script functions into a hierarchy with 
each node such as "draw", "focus", "switch", etc. sharing common 
environment variables and functions. This allowed invoking various 
functions, such as, "focus instance", "query theme", "draw panel", 
"toggle panel", etc. into keybinds and calling scripts for better 
readability and code consolidation.

To enable the automatic construction of paths necessary to use the tree 
of herbstluftwm scripts, "herbstluftwm.sh" which can be found in the 
build/root/etc/profile.d folder can be placed in /etc/profile.d or 
sourced manually in your .xinitrc script.

## license

This code is distributed under the terms and conditions of the MIT
license.
