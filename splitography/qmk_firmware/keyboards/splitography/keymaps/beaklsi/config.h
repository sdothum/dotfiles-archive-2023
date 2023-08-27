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

// compile time macro string, see functions/hardware splitography script
#define PRIVATE_STRING ""

// compile time macro string
#define PUBLIC_STRING ""

// number layer 0 position KEYPAD_0, THUMB_0
#define THUMB_0
// Haskell tap dance operator chords
#define HASKELL
// thumb capslock toggle on and off (default on only to preseve bspc auto-repeat)
#define THUMB_CAPS
// left home shift I -> space
#define LEFT_SPACE

// sync app with window manager keybind hook defined in plover_keybind.h
#define PLOVER_KEYBIND

#endif
