# -*- coding: utf-8 -*-

# Copyright (c) 2015 by sdothum <sdothum@gmail.com>
#
# In lieu of message timestamps, issue buffer timestamp clock per hourly interval
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
# 0.2:  center timestamp in message window, assumes balanced window widths
#       adjust buffers.look.name_size_max to correct window balancing
# 0.3:  name change
# 0.4:  leader character change with iosevka font
# 0.5:  persistent tape position, requires reload for window width changes
# 0.6:  dynamic tape position, using current buffer
# 0.7   dynamic tape position *per* visible window (only, bypasses hidden buffers)
#       a buffer in more than one window inherits the tape position of the first update
# 0.8   optional nickname prefix column offset
# 0.9   add timestamp to undisplayed buffers, minor code refactoring

import weechat
import time

SCRIPT_NAME    = 'buffertape'
SCRIPT_AUTHOR  = 'sdothum <sdothum@gmail.com>'
SCRIPT_VERSION = '0.9'
SCRIPT_LICENSE = 'GPL3'
SCRIPT_DESC    = 'Buffer timestamp'

# buffertape_char = '⋅⋅'
buffertape_char = '••'

settings = {
    'modulo_interval'         : '15',   # print a new timestamp every X minutes of the hour
    'center'                  : '1',    # (0) left justify (1) center
    'offset_prefix'           : '0',    # (0) include prefix (1) centre in message area only
}

# ⋅⋅ HH:MM ⋅⋅
def clock(current_time):
    timestamp = weechat.color('chat_delimiters')
    timestamp += buffertape_char + ' '
    timestamp += weechat.color('chat_time')
    timestamp += time.strftime('%H', time.localtime(current_time))
    timestamp += weechat.color('chat_time_delimiters')
    timestamp += ':'
    timestamp += weechat.color('chat_time')
    timestamp += time.strftime('%M', time.localtime(current_time))
    timestamp += weechat.color('chat_delimiters')
    timestamp += ' ' + buffertape_char
    return timestamp

def timer_cb(data, remaining_calls):
    global indent
    current_time = int(time.time())
    interval = int(weechat.config_get_plugin('modulo_interval')) * 60
    if (current_time % interval) == 0:
        timestamp = clock(current_time)
        clocked = []
        infolist = weechat.infolist_get('window', '', '')
        if infolist:
            prefix = int(weechat.string_eval_expression("${weechat.look.prefix_align_min}", {}, {}, {}))
            if weechat.config_get_plugin('offset_prefix') == '0':
                offset = (prefix + 3) / 2   # including prefix separator
            else:
                offset = 0
            while weechat.infolist_next(infolist):
                if weechat.config_get_plugin('center') == '0':
                    indent = 0
                else:
                    # centering = (window width - prefix width - prefix separator - buffertape date) / 2 - offset - rounding adjustment
                    window = weechat.infolist_pointer(infolist, 'pointer')
                    indent = (weechat.window_get_integer(window, 'win_width') - prefix - 2 - 11) / 2 - offset - 1
                buffer = weechat.window_get_pointer(window, 'buffer')
                if not buffer in clocked:
                    clocked.append(buffer)
                    weechat.prnt(buffer, ' ' * indent + timestamp)
        infolist = weechat.infolist_get('buffer', '', '')
        if infolist:
            while weechat.infolist_next(infolist):
                buffer = weechat.infolist_pointer(infolist, 'pointer')
                if not buffer in clocked:
                    clocked.append(buffer)
                    weechat.prnt(buffer, timestamp)
            weechat.infolist_free(infolist)
        del clocked
    return weechat.WEECHAT_RC_OK

if __name__ == '__main__':
    if weechat.register(SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION, SCRIPT_LICENSE,
                        SCRIPT_DESC, '', ''):
        # Set default settings
        for option, default_value in settings.iteritems():
            if not weechat.config_is_set_plugin(option):
                weechat.config_set_plugin(option, default_value)

	weechat.hook_timer(60000, 60, 0, 'timer_cb', '')
