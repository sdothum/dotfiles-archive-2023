# Search within your typed history as you type (like ctrl-R in bash)
# Usage:
# * First do: /bind ^R /history_search
# * Then type ctrl-R and type what you're searching for

# Copyright 2007  Wouter Coekaerts <coekie@irssi.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

use strict;
use Irssi 20070804;
use Irssi::TextUI;

use vars qw($VERSION %IRSSI);
$VERSION = '1.0';
%IRSSI = (
    authors     => 'Wouter Coekaerts',
    contact     => 'coekie@irssi.org',
    name        => 'history_search',
    description => 'Search within your typed history as you type (like ctrl-R in bash)',
    license     => 'GPLv2 or later',
    url         => 'http://wouter.coekaerts.be/irssi/',
    changed     => '04/08/07'
);

my $prev_typed;
my $prev_startpos;
my $enabled = 0;

Irssi::command_bind('history_search', sub {
	$enabled = ! $enabled;
	if ($enabled) {
		$prev_typed = '';
		$prev_startpos = 0;
	}
});

Irssi::signal_add_last 'gui key pressed' => sub {
	my ($key) = @_;
	
	if ($key == 10) { # enter
		$enabled = 0;
	}

	return unless $enabled;
	
	my $prompt = Irssi::parse_special('$L');
	my $pos = Irssi::gui_input_get_pos();
	
	if ($pos < $prev_startpos) {
		$enabled = 0;
		return;
	}
	
	my $typed = substr($prompt, $prev_startpos, ($pos-$prev_startpos));
	
	my $history = ($typed eq '') ? '' : Irssi::parse_special('$!' . $typed . '!');
	if ($history eq '') {
		$history = $typed;
	}
	
	my $startpos = index(lc($history), lc($typed));
		
	Irssi::gui_input_set($history);
	Irssi::gui_input_set_pos($startpos + length($typed));
	
	$prev_typed = $typed;
	$prev_startpos = $startpos;
};
