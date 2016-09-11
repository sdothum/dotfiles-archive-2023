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

import weechat
import time

SCRIPT_NAME    = "timestamp"
SCRIPT_AUTHOR  = "sdothum <sdothum@gmail.com>"
SCRIPT_VERSION = "0.2"
SCRIPT_LICENSE = "GPL3"
SCRIPT_DESC    = "Buffer timestamp"

settings = {
    "modulo_interval"         : '15',   # print a new timestamp every X minutes of the hour
    "center"                  : '1',    # (0) left justify (1) center
}

def prnt_timestamp(buffer, timestamp, indent):
    weechat.prnt(buffer, '%s%s⋅⋅ %s%s%s:%s%s%s ⋅⋅' %
	(' ' * indent,
	 weechat.color("chat_delimiters"),
	 weechat.color("chat_time"),
	 time.strftime('%H', time.localtime(timestamp)),
	 weechat.color("chat_time_delimiters"),
	 weechat.color("chat_time"),
	 time.strftime('%M', time.localtime(timestamp)),
	 weechat.color("chat_delimiters")))

def timer_cb(data, remaining_calls):
    current_time = int(time.time())
    interval = int(weechat.config_get_plugin('modulo_interval')) * 60
    if (current_time % interval) == 0:
        infolist = weechat.infolist_get("buffer", "", "")
        if infolist:
            if weechat.config_get_plugin('center') == '0':
                indent = 0
            else:
                # centering = (window width - prefix width - (vertical separator + date)) / 2 - rounding adjustment
                indent = (weechat.window_get_integer(weechat.current_window (), "win_width") - int(weechat.string_eval_expression("${weechat.look.prefix_align_min}", {}, {}, {})) - 14) / 2 - 1
            while weechat.infolist_next(infolist):
                buffer = weechat.infolist_pointer(infolist, 'pointer')
                prnt_timestamp(buffer, current_time, indent)
            weechat.infolist_free(infolist)
    return weechat.WEECHAT_RC_OK

if __name__ == "__main__":
    if weechat.register(SCRIPT_NAME, SCRIPT_AUTHOR, SCRIPT_VERSION, SCRIPT_LICENSE,
                        SCRIPT_DESC, "", ""):
        # Set default settings
        for option, default_value in settings.iteritems():
            if not weechat.config_is_set_plugin(option):
                weechat.config_set_plugin(option, default_value)

	weechat.hook_timer(60000, 60, 0, 'timer_cb', '')
