#ifndef CONFIG_USER_H
#define CONFIG_USER_H

#include "../../config.h"

// prevent holdback of 1-3 key TxBolt chords
#define PREVENT_STUCK_MODIFIERS
// fix dual function timing
#define PERMISSIVE_HOLD
// enable for center column
#define TAPPING_TOGGLE 1
// tap dance key press termination interval
#define HASKELL_TERM 200

// smooth mouse motion
// #define MOUSEKEY_INTERVAL    20
// #define MOUSEKEY_DELAY       0
// #define MOUSEKEY_TIME_TO_MAX 60
// #define MOUSEKEY_MAX_SPEED   7
// #define MOUSEKEY_WHEEL_DELAY 0

// compile time macro strings, see functions/hardware qmk script
#define PRIVATE_STRING ""
#define PUBLIC_STRING ""

// language tap dance operators
#define HASKELL
#define UNIX
// #define EMOJI

// thumb capslock toggle on and off (default on only to preseve bspc auto-repeat)
#define THUMB_CAPS
// left home shift I -> space
#define LEFT_SPACE

// sync app with window manager keybind hook defined in plover_keybind.h
#define PLOVER_KEYBIND

// rolling home row modifiers (replaces LSFT_T, LGUI_T, LCTL_T, LALT_T qmk macros)
// apply rolling shift to opposite hand (0) for all keys (1) opposite shift key only
#define ROLLOVER 1

#ifdef ROLLOVER
#define TAPPING_TERM 250
#else
#define IGNORE_MOD_TAP_INTERRUPT
#define TAPPING_TERM 200
#endif

// left handed space/enter
#define LEFT_SPC_ENT

// double tap "=="
#define EQLEQL "=~"

// double tap ",," (number layer)
// #define COMMACOMMA ", "
#define COMMACOMMA ";"

// upper case hex (idefault lower case abcdef)
// #define UPPER_HEX

#define SPLITOGRAPHY

#endif
