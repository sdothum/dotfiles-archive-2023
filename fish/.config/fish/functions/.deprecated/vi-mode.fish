# vi-mode for fish
#
# To use this script, put it somewhere fish can find and add the following
# lines to your ~/.config/fish/config.fish:
#
# . /path/to/vi-mode.fish
#
# function fish_user_keybindings # Deprecated
#         vi_mode_insert
# end
# function fish_user_key_bindings
#         vi_mode_insert
# end
#
# function fish_prompt -d "Write out the prompt"
#         printf '%s@%s%s%s%s [%s]> ' (whoami) (hostname|cut -d . -f 1) (set_color $fish_color_cwd) (prompt_pwd) (set_color normal) $vi_mode
# end
#
#
#
# If your version of fish is too old the above may not work. Please ensure that
# you are running the latest beta fish built from:
# git://github.com/fish-shell/fish-shell.git
#
# If upgrading to that is not an option for you, you can try running:
# set -U fish_key_bindings vi_mode_insert
# But please note that as of this writing, these bindings REQUIRE a version of
# fish somewhat more recent than the latest official release, otherwise using
# directions will corrupt your commandline.
#
###############################################################################



# I'm thinking about changing these to separate out the colours from the text.
# I'm undecided on this though - when I support count arguments there may be
# some benefit from displaying the count here, but that would also make the
# size of these strings vary, which I'd rather not do...
set -l cn (set_color normal)
set -g vi_mode_normal  (set_color blue)'n'$cn
set -g vi_mode_replace (set_color red)'r'$cn
set -g vi_mode_REPLACE (set_color --background=red)'R'$cn
set -g vi_mode_insert  (set_color green)'i'$cn
set -g vi_mode_delete  (set_color red)'d'$cn
set -g vi_mode_change  (set_color yellow)'c'$cn
set -g vi_mode_g       (set_color blue)'g'$cn
set -g vi_mode_lower   (set_color blue)'u'$cn
set -g vi_mode_upper   (set_color blue)'U'$cn
set -g vi_mode_swapcase (set_color blue)'~'$cn

set -g __vi_mode_undo_cmdline ''
set -g __vi_mode_undo_cmdline_pos 0

function __vi_mode_direction_command
	# Embedded python... If you can do this in pure shell then more power to you :)

	# There may be some speedup to be gained by splitting this out into a
	# separate script, which python can compile once instead of every time

	set ret (python -c "

import sys
from functools import reduce

command = sys.argv[1]
direction = sys.argv[2]
new_pos = pos = int(sys.argv[3])
lineno = int(sys.argv[4]) - 1
cmdline_list = sys.argv[5:]
cmdline = '\n'.join(cmdline_list)

def start():
	return (0, 0)
def end():
	return (len(cmdline), -1)
class not_found(Exception): pass

def dir_0():
	return (reduce(lambda a,b: a + len(b) + 1, cmdline_list[:lineno], 0), 0)

def dir_eol(): # end of line
	before_len = reduce(lambda a,b: a + len(b) + 1, cmdline_list[:lineno], 0)
	line_len = len(cmdline_list[lineno]) or 1
	return (before_len + line_len, -1)

# These routines are all similar, they can probably be combined into one, but
# I'll make sure I get all working and understand the differences first
def dir_fnw(): # First Non-Whitespace
	import re
	len_before = reduce(lambda a,b: a + len(b) + 1, cmdline_list[:lineno], 0)
	match = re.search('^\s*[^\s]', cmdline_list[lineno])
	if not match:
		return start()
	return (len_before + match.end()-1, 0)
dir__ = dir_fnw # XXX: I always used _ as this, but turns out that might not be quite right

def _dir_w(regexp):
	import re

	searchpart = cmdline[pos:]
	match = re.search(regexp, searchpart)
	if not match:
		return end()
	return (pos + match.end()-1, 0)

def _dir_e(regexp):
	import re

	searchpart = cmdline[pos+1:]
	match = re.search(regexp, searchpart)
	if not match:
		return end()
	return (pos+2 + match.start(), -1)

def _dir_b(regexp):
	import re

	if pos == 0:
		return start()

	# Reverse the string instead of matching to right:
	searchpart = cmdline[pos-1::-1]
	match = re.search(regexp, searchpart)
	if not match:
		return start()
	return (len(searchpart) - (match.start()+1), 0)

def _dir_ge(regexp):
	import re

	if pos == 0:
		return start()

	# Reverse the string instead of matching to right:
	searchpart = cmdline[pos::-1]
	match = re.search(regexp, searchpart)
	if not match:
		return start()
	return (len(searchpart) - (match.end()), 0)

# Simple, but not inclusive enough:
# def dir_w(): return _dir_w(r'[^\w]\w')
# def dir_e(): return _dir_e(r'\w[^\w]')

# Slightly too inclusive, e.g. fi--sh matches both '-' characters, but should only match one:
_dir_w_regexp = r'[^\w][^\s]|\w[^\w\s]'
_dir_e_regexp = r'[^\s][^\w]|[^\w\s]\w'
_dir_W_regexp = r'\s[^\s]'
_dir_E_regexp = r'[^\s]\s'

def dir_w(): return _dir_w(_dir_w_regexp)
def dir_W(): return _dir_w(_dir_W_regexp)
def dir_e(): return _dir_e(_dir_e_regexp)
def dir_E(): return _dir_e(_dir_E_regexp)
def dir_b(): return _dir_b(_dir_e_regexp)
def dir_B(): return _dir_b(_dir_E_regexp)
def dir_ge(): return _dir_ge(_dir_w_regexp)
def dir_gE(): return _dir_ge(_dir_W_regexp)
def dir_cw(): return _dir_w(_dir_e_regexp)
def dir_cW(): return _dir_w(_dir_E_regexp)

def dir_h():
	if pos: return (pos-1, 0)
	return start()

def dir_l():
	return (pos+1, 0)

def dir_t(char):
	new_pos = cmdline.find(char, pos+1)
	if new_pos < 0:
		raise not_found
	return (new_pos, -1)

def dir_T(char):
	new_pos = cmdline.rfind(char, 0, pos)
	if new_pos < 0:
		raise not_found
	return (new_pos+1, 0)

def dir_f(char): return (dir_t(char)[0]+1, -1)
def dir_F(char): return (dir_T(char)[0]-1, 0)

def cmd_delete():
	dst_pos = dir(direction)
	if dst_pos >= pos:
		new_cmdline = cmdline[:pos] + cmdline[dst_pos:]
		return (new_cmdline, pos)
	new_cmdline = cmdline[:dst_pos] + cmdline[pos:]
	return (new_cmdline, dst_pos)

def cmd_change():
	# 'Special case: 'cw' and 'cW' are treated like 'ce' and 'cE' if the cursor
	# is on a non-blank.  This is because 'cw' is interpreted as change-word,
	# and a word does not include the following white space.'
	#
	# Note: Even with this special case the behaviour does not quite match what
	# vim actually does in practice - try with the cursor on punctuation, or on
	# the last character in a word. Specifically, the behaviour of cw differs
	# from ce when the cursor is already on the last character of a 'word', for
	# vim's definition of word.
	#
	# Because of this, we use a special direction to handle this case.
	global direction
	if direction in 'wW' and not cmdline[pos].isspace():
		direction = direction.replace('w', 'cw').replace('W', 'cW')
	return cmd_delete()

def cmd_o():
	above = '\n'.join(cmdline_list[:lineno + 1])
	below = '\n'.join(cmdline_list[lineno + 1:])
	return (above + '\n\n' + below, len(above)+1)

def cmd_O():
	above = '\n'.join(cmdline_list[:lineno])
	below = '\n'.join(cmdline_list[lineno:])
	return (above + '\n\n' + below, len(above)+1)

def _dir_cmd_func(func):
	(pos1, pos2) = (pos, dir(direction))
	if pos2 < pos:
		(pos1, pos2) = (pos2, pos1)
	new_cmdline = cmdline[:pos1] + func(cmdline[pos1:pos2]) + cmdline[pos2:]
	return (new_cmdline, pos1)

# XXX: automagic completion sometimes hides the results of changing the case in these commands:
def cmd_upper(): return _dir_cmd_func(str.upper)
def cmd_lower(): return _dir_cmd_func(str.lower)
def cmd_swapcase():  return _dir_cmd_func(str.swapcase)

def dir(d, cursor = False):
	def validate(pos):
		if pos < 0: return 0
		if pos > len(cmdline): return len(cmdline)
		return pos
	a = ()
	if ':' in d:
		(d, a) = d.split(':', 1)
	(new_pos, cursor_off) = globals()['dir_%s' % d](*a)
	if cursor:
		return validate(new_pos + cursor_off)
	return validate(new_pos)

def cmd(c): return globals()['cmd_%s' % c]()

def cmd_normal():
	return (None, dir(direction, True))

try:
	(cmdline, new_pos) = cmd(command)
	if cmdline is not None:
		print ( cmdline )
except not_found:
	new_pos = pos
print ( new_pos )

" $argv[1] $argv[2] (commandline -C) (commandline -L) (commandline)) # commandline should always be last

	set new_pos $ret[-1]
	set -e ret[-1] # Guessing that deleting last element is likely to be faster than deleting first
	if test (count $ret) -gt 0
		commandline -- $ret
	end
	commandline -C $new_pos
end

function __vi_mode_common_emacs -d "common key bindings for all vi-like modes that are identical to the emacs mode"
	# Feel free to expand this list for anything that makes sense, this is just
	# a couple of hand-picked ones that seemed useful and/or I use :)

	bind \e\[A up-or-search
	bind \e\[B down-or-search
	bind -k down down-or-search
	bind -k up up-or-search

	bind \e\[C forward-char
	bind \e\[D backward-char
	bind -k right forward-char
	bind -k left backward-char

	bind \e\[H beginning-of-line
	bind \e\[F end-of-line
	# OS X SnowLeopard doesn't have these keys. Don't show an annoying error message.
	bind -k home beginning-of-line 2> /dev/null
	bind -k end end-of-line 2> /dev/null

	bind \cl 'clear; commandline -f repaint'
	bind \cd delete-or-exit
end

function __vi_mode_common -d "common key bindings for all vi-like modes"
	__vi_mode_common_emacs

	bind \e __vi_mode_normal

	# Can we put commandline into history when pressing ^C?
	bind \cc '__vi_mode_save_cmdline; for i in (seq (count (commandline))); echo; end; commandline ""; vi_mode_insert'

	bind \n "commandline -f execute; vi_mode_insert"
end

function __vi_mode_common_insert -d "common key bindings for all insert vi-like modes"
	__vi_mode_common
	bind \e 'commandline -f backward-char; __vi_mode_normal'
	if functions -q vi_mode_user
		vi_mode_user insert
	end
end

function __vi_mode_bind_directions
	__vi_mode $argv[1]

	for direction in W w E e B b 0 _ h l
		bind $direction "$argv[3]; __vi_mode_direction_command '$argv[1]' $direction; $argv[2]"
	end
	bind \$ "$argv[3]; __vi_mode_direction_command '$argv[1]' eol; $argv[2]"
	bind \^ "$argv[3]; __vi_mode_direction_command '$argv[1]' fnw; $argv[2]"
	for direction in f F t T
		bind $direction "__vi_mode_bind_all '$argv[3]; __vi_mode_direction_command %q$argv[1]%q {$direction}:%k; $argv[2]'"
	end

	bind g "__vi_mode_bind_directions_g $argv"
end

function __vi_mode_bind_directions_g
	for direction in e E
		bind $direction "$argv[3]; __vi_mode_direction_command '$argv[1]' g$direction; $argv[2]"
	end
end

function __vi_mode_bind_all
	# There seems to be some magic that doesn't work properly without this:
	bind '' self-insert

	python -c "
command = '''$argv'''
for c in map(chr, range(0x20, 0x7f)):
	q = '\"' # Enclose command in these
	Q = '\'' # Other quote - for quotes inside command
	if c == '\"':
		l = r = r'\\%s' % c
		(q, Q) = (Q, q) # Swap quotes
	elif c in ['(', ')', '<', '>', ';', '|', '\'']:
		l = r = r'\%s' % c
	elif c == '\\\\':
		l = r'\\\\'
		r = r'\\\\\\\\'
	elif c == '\$':
		l = '\%s' % c
		r = r\"'\%s'\" % c
	else:
		l = r = \"'%s'\" % c
	print ( '''bind %s %s%s%s''' % (l, q, command.replace('%k', r).replace('%q', Q), q))
	" | .
end

function __vi_mode
	# Is there a way to do this without eval?
	# We really want something like a dictionary...
	eval set -g vi_mode \$vi_mode_{$argv}
	commandline -f repaint
end

function __vi_mode_replace
	__vi_mode replace
	bind --erase --all
	__vi_mode_common

	# backward-char should happen last, but only works if specified first
	# (guess I should dig through the C code and figure out what is going
	# on):
	# __vi_mode_bind_all "commandline -f delete-char; commandline -i %k; commandline -f backward-char; __vi_mode_normal"
	__vi_mode_bind_all "__vi_mode_save_cmdline; commandline -f backward-char delete-char; commandline -i %k; __vi_mode_normal"

	if functions -q vi_mode_user
		vi_mode_user replace
	end
end

function __vi_mode_overwrite
	__vi_mode REPLACE
	bind --erase --all
	__vi_mode_common_insert
	__vi_mode_save_cmdline

	__vi_mode_bind_all "commandline -f delete-char; commandline -i %k"
	if functions -q vi_mode_user
		vi_mode_user overwrite
	end
end

function __vi_mode_save_cmdline
	# Only vi style single level for now, patch to suppport vim style
	# multi-level undo history welcome
	set -g __vi_mode_undo_cmdline (commandline)
	set -g __vi_mode_undo_cmdline_pos (commandline -C)
end

function __vi_mode_undo
	set -l cmdline (commandline)
	set -l pos (commandline -C)
	commandline $__vi_mode_undo_cmdline
	commandline -C $__vi_mode_undo_cmdline_pos
	set -g __vi_mode_undo_cmdline $cmdline
	set -g __vi_mode_undo_cmdline_pos $pos
end

function __vi_mode_g -d "vi-like key bindings for fish (commands starting with g)"
	__vi_mode g
	bind --erase --all
	__vi_mode_bind_all '__vi_mode_normal'
	__vi_mode_common

	bind I '__vi_mode_save_cmdline; commandline -f beginning-of-line; vi_mode_insert'
	# XXX: automagic completion sometimes hides the results of changing the case in these commands:
	bind u '__vi_mode_bind_directions lower __vi_mode_normal __vi_mode_save_cmdline'
	bind U '__vi_mode_bind_directions upper __vi_mode_normal __vi_mode_save_cmdline'
	bind \~ '__vi_mode_bind_directions swapcase __vi_mode_normal __vi_mode_save_cmdline'
	# TODO: The rest of the g commands + directions.

	__vi_mode_bind_directions_g normal __vi_mode_normal ''

	if functions -q vi_mode_user
		vi_mode_user g
	end
end

function __vi_mode_normal -d "WIP vi-like key bindings for fish (normal mode)"
	__vi_mode normal

	bind --erase --all

	# NOTE: bind '' self-insert seems to be required to allow the
	# prompt to change, but we don't want unbound keys to be able to
	# self-insert, so set the default binding, but bind everything to
	# do nothing (which is wasteful, but seems to work):
	__vi_mode_bind_all ''

	__vi_mode_common

	# Fish recently gained support for this, redirect to /dev/null so it
	# doesn't fall over if running an old version of fish:
	commandline -f suppress-autosuggestion 2>/dev/null

	bind i '__vi_mode_save_cmdline; vi_mode_insert'
	bind I '__vi_mode_save_cmdline; __vi_mode_direction_command normal fnw; vi_mode_insert'
	bind a '__vi_mode_save_cmdline; commandline -f forward-char; vi_mode_insert'
	bind A '__vi_mode_save_cmdline; commandline -f end-of-line; vi_mode_insert'
	bind o '__vi_mode_save_cmdline; __vi_mode_direction_command o ""; vi_mode_insert'
	bind O '__vi_mode_save_cmdline; __vi_mode_direction_command O ""; vi_mode_insert'

	# Cool, these functions are pretty close to what I wanted:
	# FIXME: Cursor not placed in correct position, but moving it prevents further searching
	bind j down-or-search
	bind k up-or-search

	bind x '__vi_mode_save_cmdline; commandline -f delete-char'
	bind D '__vi_mode_save_cmdline; commandline -f kill-line'
	# bind Y 'commandline -f kill-whole-line yank'
	bind P '__vi_mode_save_cmdline; commandline -f yank'
	bind p '__vi_mode_save_cmdline; commandline -f yank forward-char' # Yes, this is reversed. Otherwise it does the wrong thing. Go figure.
	bind C '__vi_mode_save_cmdline; commandline -f kill-line; vi_mode_insert'
	bind S '__vi_mode_save_cmdline; commandline -f kill-whole-line; vi_mode_insert'
	bind s '__vi_mode_save_cmdline; commandline -f delete-char; vi_mode_insert'
	bind r __vi_mode_replace
	bind R __vi_mode_overwrite

	# XXX: The automagic completion sometimes displays the case from the
	# command it wants to complete instead of the case actually on the
	# commandline, so even though this works, it may not always appear to work.
	# I'm not sure if I can do anything about that, I'll need to look at the
	# code. Ideally I would turn off automagic completion whenever I'm not in
	# insert mode.
	bind \~ '__vi_mode_save_cmdline; commandline -f forward-char; __vi_mode_direction_command swapcase l'

	__vi_mode_bind_directions normal __vi_mode_normal ''
	bind d '__vi_mode_bind_directions delete __vi_mode_normal __vi_mode_save_cmdline'
	bind c '__vi_mode_bind_directions change vi_mode_insert __vi_mode_save_cmdline'

	# Override generic direction code for simple things that have a close
	# match in fish's builtin commands, which should be faster:
	bind h backward-char
	bind l forward-char
	bind 0 beginning-of-line
	# bind \$ end-of-line #FIXME: Cursor position
	# bind b backward-word # Note: built-in implementation only recently fixed. Also, before enabling this override, determine if this matches on the right characters

	bind g __vi_mode_g # MUST BE AFTER BIND_DIRECTIONS... I'm thinking about changing it so that this is all handled by bind_directions
	bind u __vi_mode_undo

	# NOT IMPLEMENTED:
	# bind 2 vi-arg-digit
	# bind y yank-direction
	# bind g magic :-P
	# bind ^a increment next number
	# bind ^x decrement next number
	# bind /?nN search (jk kind of does this)
	# registers (maybe try to make sensible integration into X, like an
	#   explicit yank with y goes to an X selection, while an implicit
	#   delete with x etc. doesn't. "* and "+ should natually go to the
	#   appropriate X selection if possible)
	# etc.

	if functions -q vi_mode_user
		vi_mode_user normal
	end
end

function vi_mode_insert -d "vi-like key bindings for fish (insert mode)"
	__vi_mode insert

	fish_default_key_bindings

	__vi_mode_common_insert
end

# vi:noexpandtab:sw=4:ts=4
