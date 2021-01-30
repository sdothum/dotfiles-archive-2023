// sdothum - 2016 (c) wtfpl

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

// rolling home row modifiers (replaces LSFT_T, LGUI_T, LCTL_T, LALT_T qmk macros)
// apply rolling shift to opposite hand (0) for all keys (1) opposite shift key only
#define ROLLOVER 1

#ifdef ROLLOVER
#define TAPPING_TERM 200
#else
#define IGNORE_MOD_TAP_INTERRUPT
#define TAPPING_TERM 200
#endif

// left handed space/enter
#define LEFT_SPC_ENT

// double tap "==" (regex layer) default: "=~"
// #define EQLEQL

// double tap ",," (number layer) default: ";"
// #define COMMASPACE

// smart vim G'oto and 0x on hexpad
#define SMART_DELIM

// initial hex case (0) lower case abcdef (1) upper case ABCDEF
#define HEXADECIMAL_CASE 0

// initial pinkie stagger position (0) beakl wi home row (1) wi-v stagger (2) wi-x stagger
#define PINKIE_STAGGER 0

#define PLANCK

#endif
