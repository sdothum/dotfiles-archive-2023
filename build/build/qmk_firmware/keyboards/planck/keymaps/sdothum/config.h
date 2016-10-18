#ifndef CONFIG_USER_H
#define CONFIG_USER_H

#include "../../config.h"

// required because lower/raise modifiers are redefined by colemak-dh
#define PREVENT_STUCK_MODIFIERS

// tap dance key press termination interval
#define TAPPING_TERM   225

// tap modifier actions

// #define SHIFT_TOGGLE

// #define LEADER
#ifdef LEADER
#define LEADER_TIMEOUT 250
#endif

#ifndef LEADER
#define ONE_SHOT
#endif

#endif
