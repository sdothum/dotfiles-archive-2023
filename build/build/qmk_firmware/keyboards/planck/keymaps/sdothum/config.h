#ifndef CONFIG_USER_H
#define CONFIG_USER_H

#include "../../config.h"

// required because lower/raise modifiers are redefined by colemak-dh
#define PREVENT_STUCK_MODIFIERS

// tap dance key press termination interval
#define TAPPING_TERM   250

// reserve dynamic macro buffer size
#define DYNAMIC_MACRO_SIZE 24

// compile time macro string
#define COMPILE_STRING

#endif
