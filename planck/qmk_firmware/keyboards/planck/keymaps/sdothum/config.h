#ifndef CONFIG_USER_H
#define CONFIG_USER_H

#include "../../config.h"

// required because lower/raise modifiers are redefined by colemak-dh
#define PREVENT_STUCK_MODIFIERS

// tap dance key press termination interval
#define TAPPING_TERM 250

// fix dual function timing
#define PERMISSIVE_HOLD

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

// Layout ADNW*, BEAKL*, COLEKA*, QWERTY (default)
#define BEAKL8P

// sync app on steno layer
#define LAUNCH_PLOVER

#endif
