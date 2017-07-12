use Irssi;
use Irssi::TextUI;
use strict;
use vars qw($VERSION %IRSSI);

#Setting variables:

#first_away_message - The first /away messsage
#second_away_message - The second /away message
#first_away_timeout - Number of seconds to activate the first away state
#second_away_timeout - Number of seconds to activate the second away state
#away_servers - list of servertags seperated by spaces where auto_away will work. 
#               If empty (/set -clear away_servers, it will work on every network
#
# CHANGELOG:
# 21 DEC 2004:
# the timer is only being reset when pressing enter. and the away timer starts counting after the last time you pressed enter.
# this is less CPU consuming :-D

$VERSION = '0.2';
%IRSSI = (
    authors     => 'Tijmen Ruizendaal',
    contact     => 'tijmen@fokdat.nl',
    name        => 'auto_away.pl',
    description => 'server specific autoaway with two different away states at different intervals',
    license     => 'GPLv2',
    url         => 'http://the-timing.nl/stuff/irssi-bitlbee',
    changed     => '2004-12-21',
);


my $timer;

Irssi::settings_add_str('misc', 'first_away_message', undef);
Irssi::settings_add_str('misc', 'second_away_message', undef);
Irssi::settings_add_int('misc', 'first_away_timeout', undef);
Irssi::settings_add_int('misc', 'second_away_timeout', undef);
Irssi::settings_add_str('misc', 'away_servers', undef);

sub reset_timer{
  my $key=shift;
  if($key == 10){
    my (@servers) = Irssi::servers();
    foreach my $server (@servers) {
      if($server->{usermode_away} == 1){
        $server->command("AWAY -one");
      }
    }
    Irssi::timeout_remove($timer); 
    my $timeout = Irssi::settings_get_int('first_away_timeout');
    if ($timeout){
      $timer = Irssi::timeout_add_once($timeout*1000, 'first_away', undef); ## activate first away state
    }
  }
}
sub first_away{
  away(1);
  my $timeout = Irssi::settings_get_int('second_away_timeout');
  if ($timeout){
    Irssi::timeout_remove($timer);
    $timer = Irssi::timeout_add_once($timeout*1000, 'away', 2); ## activate second away state
  }
}
sub away{
  my $state = shift;
  my $server_string = Irssi::settings_get_str('away_servers');
  my (@away_servers) = split (/ +/, $server_string);
  my (@servers) = Irssi::servers();
  my $message;

  if($state == 1){
    $message = Irssi::settings_get_str('first_away_message');
  }elsif($state == 2){
    $message = Irssi::settings_get_str('second_away_message');
  }
  if($server_string){
    foreach my $away_server (@away_servers) {
    #print "|$away_server|"; 
      foreach my $server (@servers) {
        if ($away_server eq $server->{tag} && ($server->{usermode_away} == 0 || $state == 2) ){
          $server->command("AWAY -one $message");
        }
      }
    }
  }else{
    my $server=$servers[0];
    $server->command("AWAY -all $message");
  }
}
Irssi::signal_add_last('gui key pressed', 'reset_timer');
