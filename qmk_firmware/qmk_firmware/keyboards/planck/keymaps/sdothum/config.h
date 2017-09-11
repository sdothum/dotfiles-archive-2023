#ifndef CONFIG_USER_H
#define CONFIG_USER_H

#include "../../config.h"

// required because lower/raise modifiers are redefined by colemak-dh
#define PREVENT_STUCK_MODIFIERS

// tap dance key press termination interval
#define TAPPING_TERM   250

// compile time macro string, see functions/hardware planck script (undefine otherwise)
#define PRIVATE_STRING

// compile time macro string, must be in quotes
#define PUBLIC_STRING ":%s/arch=(.*)/arch=('any')\n"

// thumb key tap-shift() double tap: one shot shift (0) off (1) on
#define DT_SHIFT 1

// Layout COLEKAX, MOD_DH (Colemak), QWERTY (default)
#define COLEMAX

#endif
