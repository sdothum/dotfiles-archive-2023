#ifndef CONFIG_USER_H
#define CONFIG_USER_H

#include "../../config.h"

// prevent holdback of 1-3 key TxBolt chords
#define PREVENT_STUCK_MODIFIERS
// fix dual function timing
#define PERMISSIVE_HOLD
#define IGNORE_MOD_TAP_INTERRUPT
// enable for center column
#define TAPPING_TOGGLE 1
// tap dance key press termination interval
#define TAPPING_TERM 250
#define HASKELL_TERM 200

// smooth mouse motion
// #define MOUSEKEY_INTERVAL    20
// #define MOUSEKEY_DELAY       0
// #define MOUSEKEY_TIME_TO_MAX 60
// #define MOUSEKEY_MAX_SPEED   7
// #define MOUSEKEY_WHEEL_DELAY 0

// compile time macro strings, see functions/hardware qmk script
#define PRIVATE_STRING ""
#define PUBLIC1_STRING ""
#define PUBLIC2_STRING ""

// language tap dance operators
#define HASKELL
#define UNIX
#define EMOJI

// test all keyswitches
// #define TEST

// rolling home row modifiers (replaces LGUI_T, LCTL_T, LALT_T qmk macros)
#define ROLLOVER

// left handed space/enter
// #define LEFT_SPC_ENT

// double tap "=="
#define EQLEQL "!="

#endif
