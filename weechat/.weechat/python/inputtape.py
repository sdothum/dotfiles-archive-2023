# -*- coding: utf-8 -*-

# Copyright (c) 2015 by sdothum <sdothum@gmail.com>
#
# Unfocused input bar indicator.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# 0.1:  initial release
# 0.2:  name change
# 0.3:  name change
# 0.4:  leader character change with iosevka font
#
# usage:
# add +inputtape to the end of weechat.bar.input.items

SCRIPT_NAME    = "inputtape"
SCRIPT_AUTHOR  = "sdothum <sdothum@gmail.com>"
SCRIPT_VERSION = "0.2"
SCRIPT_LICENSE = "GPL3"
SCRIPT_DESC    = "Unfocused input bar indicator. Add +inputtape to the end of input bar."

# inputtape_char = '⋅'
inputtape_char = '․'

try:
    import weechat as w

except Exception:
    print "This script must be run under WeeChat."
    print "Get WeeChat now at: http://www.weechat.org/"
    quit()

except ImportError as message:
    print('Missing package(s) for %s: %s' % (SCRIPT_NAME, message))
    import_ok = False

def unfocused_bar_item_update (data=None, signal=None, signal_data=None):
    '''Updates bar item'''
    w.bar_item_update('inputtape')
    return w.WEECHAT_RC_OK

def unfocused_bar_item (data, item, window):
    '''Item constructor'''
    # window empty? root bar!
    if not window:
        window = w.current_window()

    ptr_buffer = w.window_get_pointer(window, "buffer")
    if ptr_buffer == "" or ptr_buffer == w.current_buffer():
        return ""

    length = w.window_get_integer(window, 'win_width') - w.buffer_get_integer(ptr_buffer, 'input_length')
    s = length * inputtape_char
    return s

if __name__ == "__main__":
    if w.register(SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION, SCRIPT_LICENSE, SCRIPT_DESC, "", ""):
        # update status bar display
        unfocused_bar_item_update()
        w.hook_signal('buffer_switch', 'unfocused_bar_item_update', '')
        w.bar_item_new('inputtape', 'unfocused_bar_item', '')
