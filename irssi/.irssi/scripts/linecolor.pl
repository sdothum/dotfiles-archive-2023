# Line Color - Assign colours to lines from specific nicks, or matching specific patterns
# Adapted from "Nick Color" by Timo Sirainen, as modified by Ian Petersi

use strict;
use Irssi 20020101.0250 ();
use vars qw($VERSION %IRSSI); 
$VERSION = "1.2";
%IRSSI = (
    authors     => "Timo Sirainen, Ian Petersi, Imran Nazar",
    contact	=> "tss\@iki.fi", 
    name        => "Line Color",
    description => "assign colours to lines from specific nicks, or matching specific patterns",
    license	=> "Public Domain",
    url		=> "http://irssi.org/",
    changed	=> "2010-01-28T18:30+0000"
);

# hm.. i should make it possible to use the existing one..
Irssi::theme_register([
  'pubmsg_hilight', '{pubmsghinick $0 $3 $1}$2'
]);

my %saved_colors;
my %saved_regex_colors;
my %session_colors = {};
my @colors = qw/2 3 4 5 6 7 9 10 11 12 13/;

sub load_colors {
  open COLORS, "$ENV{HOME}/.irssi/saved_colors";

  while (<COLORS>) {
    # I don't know why this is necessary only inside of irssi
    my @lines = split "\n";
    foreach my $line (@lines) {
      my($type, $nick, $color) = split ":", $line;
      if ($type eq "NICK") {
        $saved_colors{$nick} = $color;
      } elsif ($type eq "REGEX") {
        $saved_regex_colors{$nick} = $color;
      }
    }
  }

  close COLORS;
}

sub save_colors {
  open COLORS, ">$ENV{HOME}/.irssi/saved_colors";

  foreach my $nick (keys %saved_colors) {
    print COLORS "NICK:$nick:$saved_colors{$nick}\n";
  }
  foreach my $regex (keys %saved_regex_colors) {
    print COLORS "REGEX:$regex:$saved_regex_colors{$regex}\n";
  }
  Irssi::print("Saved colors to $ENV{HOME}/.irssi/saved_colors");

  close COLORS;
}

# If someone we've colored (either through the saved colors, or the hash
# function) changes their nick, we'd like to keep the same color associated
# with them (but only in the session_colors, ie a temporary mapping).

sub sig_nick {
  my ($server, $newnick, $nick, $address) = @_;
  my $color;

  $newnick = substr ($newnick, 1) if ($newnick =~ /^:/);

  if ($color = $saved_colors{$nick}) {
    $session_colors{$newnick} = $color;
  } elsif ($color = $session_colors{$nick}) {
    $session_colors{$newnick} = $color;
  }
}

# This gave reasonable distribution values when run across
# /usr/share/dict/words

sub simple_hash {
  my ($string) = @_;
  chomp $string;
  my @chars = split //, $string;
  my $counter;

  foreach my $char (@chars) {
    $counter += ord $char;
  }

  $counter = $colors[$counter % 11];

  return $counter;
}

sub find_color {
  my ($server, $msg, $nick, $address, $target) = @_;
  my $chanrec = $server->channel_find($target);
  return if not $chanrec;
  my $nickrec = $chanrec->nick_find($nick);
  return if not $nickrec;
  my $nickmode = $nickrec->{op} ? "@" : $nickrec->{voice} ? "+" : "";

  # Has the user assigned this nick a color?
  my $color = $saved_colors{$nick};

  # Have -we- already assigned this nick a color?
  if (!$color) {
    $color = $session_colors{$nick};
  }

  # Does the message match any color regexen?
  if (!$color) {
    foreach my $r (keys %saved_regex_colors) {
      if ($msg =~ m/($r)/i) {
        $color = $saved_regex_colors{$r};
	last;
      }
    }
  }

  # Let's assign this nick a color, if none matched
  #if (!$color) {
  #  $color = simple_hash $nick;
  #  $session_colors{$nick} = $color;
  #  $color = 15;
  #}

  if (!$color) {
    $color = 0;
  }

  return $color;
}

# FIXME: breaks /HILIGHT etc.
sub sig_public {
  my ($server, $msg, $nick, $address, $target) = @_;
  my $color = find_color(@_);

  if($color) {
    $color = "0".$color if ($color < 10);
    $server->command('/^format pubmsg {pubmsgnick $2 {pubnick '.chr(3).$color.'$0'.chr(3).'15}}'.chr(3).$color.'$1');
  } else {
    $server->command('/^format pubmsg {pubmsgnick $2 {pubnick $0}}$1');
  }
}

sub sig_action {
  my ($server, $msg, $nick, $address, $target) = @_;
  my $color = find_color(@_);

  if($color) {
    $server->command('/^format action_public {pubaction '.chr(3).$color.'$0'.chr(3).'15}'.chr(3).$color.'$1');
  } else {
    $server->command('/^format action_public {pubaction $0}$1');
  }
}

sub cmd_color {
  my ($data, $server, $witem) = @_;
  my ($op, $nick, $color) = split " ", $data;

  $op = lc $op;

  if (!$op || $op eq "help") {
    Irssi::print ("Supported commands: 
    preview (list possible colors and their codes)
    list (show current entries in saved_colors)
    set <nick> <color> (associate a color to a nick)
    rset <regex> <color> (colorize messages matching a regex)
    clear <nick> (delete color associated to nick)
    rclear <regex> (delete color associated to regex)
    save (save colorsettings to saved_colors file)");
  } elsif ($op eq "save") {
    save_colors;
  } elsif ($op eq "set") {
    if (!$nick) {
      Irssi::print ("Nick not given");
    } elsif (!$color) {
      Irssi::print ("Color not given");
    } elsif ($color < 2 || $color > 14) {
      Irssi::print ("Color must be between 2 and 14 inclusive");
    } else {
      $saved_colors{$nick} = $color;
    }
    Irssi::print ("Added ".chr (3) . "$saved_colors{$nick}$nick" .
                           chr (3) . "1 ($saved_colors{$nick})");
  } elsif ($op eq "rset") {
    if (!$nick) {
      Irssi::print ("Regex not given");
    } elsif (!$color) {
      Irssi::print ("Color not given");
    } elsif ($color < 2 || $color > 14) {
      Irssi::print ("Color must be between 2 and 14 inclusive");
    } else {
      $saved_regex_colors{$nick} = $color;
    }
    Irssi::print ("Added ".chr (3) . "$saved_regex_colors{$nick}$nick" .
                           chr (3) . "1 ($saved_regex_colors{$nick})");
  } elsif ($op eq "clear") {
    if (!$nick) {
      Irssi::print ("Nick not given");
    } else {
      delete ($saved_colors{$nick});
    }
    Irssi::print ("Cleared ".$nick);
  } elsif ($op eq "rclear") {
    if (!$nick) {
      Irssi::print ("Regex not given");
    } else {
      delete ($saved_regex_colors{$nick});
    }
    Irssi::print ("Cleared ".$nick);
  } elsif ($op eq "list") {
    Irssi::print ("\nSaved colors:");
    foreach my $nick (keys %saved_colors) {
      Irssi::print ("Nick: ".chr (3) . "$saved_colors{$nick}$nick" .
		             chr (3) . "1 ($saved_colors{$nick})");
    }
    foreach my $r (keys %saved_regex_colors) {
      Irssi::print ("Regex: ".chr (3) . "$saved_regex_colors{$r}$r" .
                              chr (3) . "1 ($saved_regex_colors{$r})");
    }
  } elsif ($op eq "preview") {
    Irssi::print ("\nAvailable colors:");
    foreach my $i (2..14) {
      Irssi::print (chr (3) . "$i" . "Color #$i");
    }
  }
}

load_colors;

Irssi::command_bind('color', 'cmd_color');

Irssi::signal_add('message public', 'sig_public');
Irssi::signal_add('message irc action', 'sig_action');
Irssi::signal_add('event nick', 'sig_nick');
