// sdothum - 2016 (c) wtfpl

// ....................................................................... Corne
 
#ifdef CORNE
#define KEYMAP LAYOUT
#include QMK_KEYBOARD_H
#ifdef RGBLIGHT_ENABLE
//Following line allows macro to read current RGB settings
extern rgblight_config_t rgblight_config;
#endif
extern uint8_t is_master;
#endif

// ............................................................. Chimera Ergo 42

#ifdef CHIMERA
// #include "config.h"
#include "chimera_ergo_42.h"
// #include "action_layer.h"
// #include "eeconfig.h"
// extern keymap_config_t keymap_config;
#endif

// ...................................................................... Planck

#ifdef PLANCK
#include "config.h"
#include "planck.h"
#include "action_layer.h"
#ifdef STENO_ENABLE
#include "keymap_steno.h"
#endif
#ifdef AUDIO_ENABLE
#include "audio.h"
#include "sounds.h"
#endif
#include "eeconfig.h"
#endif

// ................................................................ Splitography

#ifdef SPLITOGRAPHY
#include "config.h"
#include "splitography.h"
#include "action_layer.h"
#ifdef STENO_ENABLE
#include "keymap_steno.h"
#endif
#ifdef AUDIO_ENABLE
#include "audio.h"
#endif
#include "eeconfig.h"
#endif
