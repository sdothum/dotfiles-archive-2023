# Wifish

Wifish (from wifi.sh) is meant to do very little. Design Goals are

1. List Available Wifi APs
2. Connect to an AP

That is all.

## Requirements

1. `wpa_supplicant` - Tested with 2.4.
2. `gawk` - Sorry other awks, Multidimensional arrays make this much cleaner.
3. `dialog` - *Optional* For ncurses menu and password prompt

## Usage

### Get it
clone this repo
```
% git clone git://github.com/bougyman/wifish
```

### Configure it

*Optional* 

A few environment variables can modify the behavior of wifish

* `SCAN_SECONDS` - How long an AP scan waits for scanned AP results.  Default
  is 0 seconds, as a running `wpa_supplicant` will generally have a viable
  scan\_result already. If you have a problem not finding APs, try 

```
SCAN_SECONDS=5 wifish
```

* `WIFISH_DEFAULT` - Sets what Command runs when wifish is called with no arguments. Defaults to `menu` or `list`, depending on if `dialog` is available. 

### Use it

#### Make sure `wpa_supplicant` is running and you have access

```
% wpa_cli status
```

If this errors, FIX IT BEFORE GOING ANY FURTHER, NOTHING ELSE WILL WORK

Common fixes:

*UPDATE* You may use the included `sv/wpa_supplicant` service which should get `wpa_cli status` to 
work for you. See <a href="#supervising-wpa_supplicant">Supervising `wpa_supplicant`</a>

1. Start `wpa_supplicant` Example: `wpa_supplicant -B -c /etc/wpa_supplicant/wpa_spplicant.conf -i wlan0`
   You may need to modify your conf file location as well as the -i interface, and this must be done as root or with sudo
2. Make sure you have rights to the `wpa_supplicant` control socket, take a look a the first few lines of `wpa_supplicant.conf`
   and it should be clear what group you have to be in. Of course you can always just run `wifish` as root (But don't)

   ```
   ctrl_interface=/run/wpa_supplicant
   ctrl_interface_group=wheel
   update_config=1
   ``` 

   In this case you would have to be a member of the 'wheel' group. Make sure `update_config=1` is set in the configuration file, as well.
3. Read some docs
4. See Support, below

Now run wifish

```
% cd wifish
% ./wifish
```

This should list all available APs.

Without arguments, wifish will automatically present a menu of APs if you have `dialog` installed, otherwise it will show a list of APs. This default
can be configured (see Configuration).


* No `dialog` installed, `wifish` with no args calls

    ```
    % wifish list
    ```

* `dialog` installed, `wifish` with no args calls

    ```
    % wifish menu
    ```

## Commands

All commands can be shortened to uniqueness

`wifish m` and `wifish menu` both run the menu command

#### Currently implemented

* `list` - Lists all available APs (from iw scan)
* `menu` - Menu of APs to connect to (requires `dialog`)
* `connect <ssid>` - Connects to an AP
  `wifish c MySSID`

Currently works when connecting to Open, WPA-PSK, and WPA2-PSK (and mixed-mode WPA) with or without TKIP

#### In Testing

* `connect` - Without args, tries to find something to connect to.

  Are you feeling lucky? Maybe use `wifish menu` instead

## Installation

#### Void Linux

`% xbps-install -S wifish`

#### Other Linux

```
% sudo ./install.sh
```

Now you can use 'wifish' from anywhere without the fully qualified path

## Supervising `wpa_supplicant`

In order to keep `wpa_supplicant` running all the time, you can background it (in Fixes, above) or
properly supervise it. The `sv/wpa_supplicant` directory is a service directory which will work
with `runit`, `daemontools`, `s6`, and many others. It will install to `/etc/sv/wpa_supplicant` 
when running `install.sh`. To enable the service, first edit `/etc/sv/wpa_supplicant/conf` to match
your wifi interface and optionally `wpa_supplicant` control group. Then symlink the service directory
to your supervision directory.

Examples:

```
% ln -s /etc/sv/wpa_supplicant /service # Daemontools
% ln -s /etc/sv/wpa_supplicant /var/service # Runit on VoidLinux
% ln -s /etc/sv/wpa_supplicant /etc/service # Runit on Debian/Ubuntu
% sv-enable wpa_supplicant # Anything with sv-helper on it
```

Once linked, `wpa_supplicant` will run forever, on every boot, always logging to `/var/log/wpa_supplicant/current`

* note: the logger for the `wpa_supplicant` service (`log/run`) uses rsvlog, part of the runit suite. This file can be modified
        to use another logger, simply edit `/etc/sv/wpa_supplicant/log/run` (`logger -t` works for most syslogs)

## Uninstall

#### Void LInux

`% xbps-remove wifish`

#### Other Linux

```
% rm -rf /usr/local/share/wifish /usr/local/bin/wifish /etc/sv/wpa_supplicant
```

## Support

* <a href="FAQ.md">F.A.Q.</a>
* file a gh issue or catch me on #voidlinux on the Freenode IRC Network
