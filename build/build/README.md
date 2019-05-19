# dotfiles

These are the dotfiles referenced in
[the darnedest thing](http://thedarnedestthing.com/colophon), and then
some.

## installation

These build files for various distros utilize the scripts in 
~/bin/functions/{package,log,query,sysadmin,test}.

bundle_install is the loader for the package bundle sets.

Debian is out of date, SID no longer being used.

Arch is the primary distro (Obarun is being looked at as it evolves).

The Void install (glibc) is close to the Arch build (save for the server 
components and gems). Void-musl is currently a no go with it's fifo 
buffer issues and herbstluftwm wm xorg issues.

Void, Arch..? Void has a few X11 (gvim) graphics niggles and its repos 
are still catching up to Arch's (most users will not notice). Runit is 
eloquent in its simplicity (though, in all honesty, i have never had any 
operational issues with systemd nor found any performance differences 
between the two distros). Void reminds me a lot of early Arch, hence, is 
a lot of fun for those who like exploring under the hood.

## license

This code is distributed under the terms and conditions of the MIT
license.
