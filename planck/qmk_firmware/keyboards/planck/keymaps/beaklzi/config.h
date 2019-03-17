#ifndef CONFIG_USER_H
#define CONFIG_USER_H

#include "../../config.h"

// required because lower/raise modifiers are redefined by colemak-dh
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

// compile time macro string, see functions/hardware planck script (undefine otherwise)
#define PRIVATE_STRING

// compile time macro string, must be in quotes
#define PUBLIC_STRING ":%s/arch=(.*)/arch=('any')\n"

// thumb key tap-shift() double tap: one shot shift (0) off (1) on
#define DT_SHIFT 1
// number layer 0 position KEYPAD_0, THUMB_0
#define THUMB_0
// Haskell tap dance operator chords
#define HASKELL
// equal key toggle layer (differs from splitography)
#define EQL_LT _ADJUST
// sync app with window manager keybind hook defined in plover_keybind.h
#define PLOVER_KEYBIND
// thumb capslock toggle on and off (default on only to preseve bspc auto-repeat)
#define THUMB_CAPS
// left home shift I -> space
#define LEFT_SPACE

// layout ADNW*, BEAKL*, COLEKA*, QWERTY (default)
#define BEAKLTI

// keyboard hardware
#define PLANCK
#endif
