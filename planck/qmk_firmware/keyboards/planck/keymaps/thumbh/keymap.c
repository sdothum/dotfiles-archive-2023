// This is the canonical layout file for the Quantum project. If you want to add another keyboard,
// this is the style you want to emulate.
//
// To flash planck firmware
// ════════════════════════
//   Reset keyboard or press hw reset button on base (hole)
//
//   cd qmk_firmware/keyboards/planck
//   sudo make KEYMAP=sdothum dfu
//
//   sudo make clean          (good practice before flashing)
//   sudo make KEYMAP=sdothum (to compile check)
//
// Package requirements (for arch linux)
// ═════════════════════════════════════
//   avr-gcc-atmel
//   avr-libc-atmel
//   dfu-programmer
//
// Notes
// ═════
//   ** E R G O   W I D E   S P L I T ** Layout
//
//   Autocompletion tap dance key pairs (),[],{} are available from the
//   number/symbol layer, as well as, numerous (un)shift key values
//
//   The navigation pad provides a single hand right thumb activated cluster
//   with left hand modifiers
//
//   #define PRIVATE_STRING includes private_string.h, a user defined code
//   block for the PRIV tap dance e.g. SEND_STRING("secret messape"),
//   see function private()
//
// Modifier clusters
// ═════════════════
//   The num and sym keys together access the navigation pad layer
//
//   ,-----------------------------------------------------------------------------------.
//   | Ctrl |  GUI |  Alt |  Esc | Space|  Tab | Bksp |  Ent | Left | Down |  Up  | Right|
//   `-----------------------------------------------------------------------------------'
//
// Hint
// ════
//   For sculpted keycaps such as Cherry or OEM profile, reverse the Alt, Num,
//   Shift, Shift, Nav, Sym keycaps for more ergonomic thumb orientation and
//   actuation
//
// Code
// ════
//   This source is shamelessly based on the "default" planck layout
//
//   #ifdef/#endif block structures are not indented, as syntax highlighting
//   in vim is sufficient for identification
//
//   c++ commenting style is used throughout
//
// Change history
// ══════════════
//   See http://thedarnedestthing.com/colophon



//                === N O T E ===
//
// sudo CPATH=<keymap.c directory>/common make ...


#include "config.h"
#include "planck.h"
#include "action_layer.h"
#ifdef STENO_ENABLE
#include "keymap_steno.h"
#endif
#ifdef AUDIO_ENABLE
#include "audio.h"
#endif
#include "eeconfig.h"

extern keymap_config_t keymap_config;

enum planck_layers {
  _BASE = 0
 ,_SHIFT
 ,_LSHIFT
 ,_RSHIFT
 ,_LSYMBOL
 ,_RSYMBOL
 ,_PLOVER
 ,_NUMBER
 ,_FNCKEY
 ,_MOUSE
 ,_EDIT
 ,_ADJUST
#ifdef CENTER_TT
 ,_TTNUMBER
 ,_TTREGEX
 ,_TTFNCKEY
 ,_TTCURSOR
 ,_TTMOUSE
#endif
 ,_END_LAYERS
};

enum planck_keycodes {
  BASE = SAFE_RANGE
 ,BASE1
 ,BASE2
 ,PLOVER
 ,SM_G      // pseudo MT   (MOD_LALT | MOD_LSFT, S(KC_G)) for shifted key-codes, see process_record_user()
 ,SA_PERC   // pseudo ALT_T(S(KC_5))                      for shifted key-codes, see process_record_user()
 ,SG_TILD   // pseudo GUI_T(S(KC_GRV))                    for shifted key-codes, see process_record_user()
 ,SL_DEL    // pseudo LT   (_MOUSE, KC_DEL)               for shifted key-codes, see process_record_user()
 ,SL_TAB    // pseudo LT   (_MOUSE, S(KC_TAB))
#ifdef CENTER_TT
 ,TT_ESC
#endif
#ifdef STENO_ENABLE
 ,PS_STNA = STN_A
 ,PS_STNO = STN_O
 ,PS_STNE = STN_E
 ,PS_STNU = STN_U
#else
 ,LT_C    = LT (_LSYMBOL, KC_C)
 ,LT_V    = LT (_NUMBER,  KC_V)
 ,LT_N    = LT (_FNCKEY,  KC_N)
 ,LT_M    = LT (_RSYMBOL, KC_M)
#endif
};

// modifier keys
#define AT_B    ALT_T(KC_B)
#define AT_DOWN ALT_T(KC_DOWN)
#define CT_RGHT CTL_T(KC_RGHT)
#define CT_C    CTL_T(KC_C)
#define GT_UP   GUI_T(KC_UP)
#define MT_E    MT   (MOD_LCTL | MOD_LALT, KC_E)
#define ST_A    SFT_T(KC_A)
#ifdef HOME_MODS
#define HOME_Q  GUI_T(KC_Q)
#define HOME_I  CTL_T(KC_I)
#define HOME_E  ALT_T(KC_E)
#define HOME_A  SFT_T(KC_A)
#define HOME_T  SFT_T(KC_T)
#define HOME_R  ALT_T(KC_R)
#define HOME_S  CTL_T(KC_S)
#define HOME_W  GUI_T(KC_W)
#else
#define HOME_Q  KC_Q
#define HOME_I  KC_I
#define HOME_E  KC_E
#define HOME_A  KC_A
#define HOME_T  KC_T
#define HOME_R  KC_R
#define HOME_S  KC_S
#define HOME_W  KC_W
#endif

#define S_DOWN  S    (KC_DOWN)
#define S_LEFT  S    (KC_LEFT)
#define S_RGHT  S    (KC_RGHT)
#define S_UP    S    (KC_UP)

#include "tapdance.h"

// keycodes
#define ___x___ KC_TRNS
#define ___fn__ KC_TRNS
#ifdef _______
#undef _______
#endif
#define _______ KC_NO

#ifdef HASKELL
#define HS_COLN TD_COLN
#define HS_LT   TD_LT
#define HS_GT   TD_GT
#define HS_EQL  TD_EQL
#else
#define HS_COLN KC_COLN
#define HS_LT   KC_LT
#define HS_GT   KC_GT
#define HS_EQL  KC_EQL
#endif

#define COPY    LCTL(KC_C)
#define CUT     LCTL(KC_X)
#define EOT     LCTL(KC_D)
#define NAK     LCTL(KC_U)
#define PASTE   LCTL(KC_V)
#define UNDO    LCTL(KC_Z)
#define TMCOPY  LALT(LCTL(KC_C))
#define TMPASTE LALT(LCTL(KC_V))
#define LT_BSPC LT  (_RSYMBOL, KC_BSPC)     // see process_record_user() for extended handling
#define LT_DEL  LT  (_ADJUST, KC_DEL)
#define LT_ESC  LT  (_LSYMBOL, KC_ESC)
#define LT_H    LT  (_LSHIFT, KC_H)
#define LT_INS  LT  (_NUMBER, KC_INS)
#define LT_LEFT LT  (_EDIT,   KC_LEFT)
#define OS_ALT  OSM (MOD_LALT)
#define OS_CTL  OSM (MOD_LCTL)
#define OS_GUI  OSM (MOD_LGUI)
#define OS_SFT  OSM (MOD_LSFT)

#ifdef CENTER_TT
#define CNTR_TL TT  (_TTFNCKEY)
#define CNTR_TR     KC_CAPS
#define CNTR_HL TT  (_TTCURSOR)
#define CNTR_HR TT  (_TTMOUSE)
#define CNTR_BL TT  (_TTNUMBER)
#define CNTR_BR TT  (_TTREGEX)
#else
#define CNTR_TL OSM (MOD_LALT | MOD_LCTL)
#define CNTR_TR OSM (MOD_LGUI | MOD_LCTL)
#define CNTR_HL OSM (MOD_LALT | MOD_LSFT)
#define CNTR_HR OSM (MOD_LGUI | MOD_LSFT)
#define CNTR_BL TD  (_CAPS)
#define CNTR_BR OSM (MOD_LSFT | MOD_LCTL)
#endif

// ........................................................ Default Alpha Layout

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

#include "base_layout.h"
#include "steno_layout.h"

// ...................................................... Number / Function Keys

#include "number_fkey_layout.h"

// ......................................................... Symbol / Navigation

#include "symbol_guifn_layout.h"

// ............................................................... Toggle Layers

#ifdef CENTER_TT
#include "toggle_layout.h"
#endif

// ......................................................... Short Cuts / Adjust

#include "chord_layout.h"

};

// ...................................................................... Sounds

#include "sounds.h"

// ........................................................... User Keycode Trap

#include "keycode_functions.c"

#define BASE_1  1
#define BASE_2  2
#define BASE_12 3
static uint8_t base_n    = 0;
static uint8_t down_rule = 0;               // (1) substitute keycode (2) keycode+shift, see tap_lt()

bool process_record_user(uint16_t keycode, keyrecord_t *record)
{
  switch (keycode) {
  case BASE1:
    if (record->event.pressed) {
      base_n = base_n | BASE_1;
      if (base_n == BASE_12) { base_layer(); }
    } else { base_n = base_n & ~BASE_1; }
    return false;
  case BASE2:
    if (record->event.pressed) {
      base_n = base_n | BASE_2;
      if (base_n == BASE_12) { base_layer(); }
    } else { base_n = base_n & ~BASE_2; }
    return false;
#ifdef HOME_MODS
  case HOME_Q:
  case HOME_W:
    tap_mods(record, KC_LGUI);
    break;
  case HOME_I:
  case HOME_S:
    tap_mods(record, KC_LCTL);
    break;
  case HOME_E:
  case HOME_R:
    tap_mods(record, KC_LALT);
    break;
  case HOME_A:
    tap_mods(record, KC_LSFT);
    break;
  case HOME_T:
    tap_mods(record, KC_RSFT);              // note: SFT_T actually uses KC_LSFT
    break;
  // special shift layer mappings
  case KC_DOT:
    if (record->event.pressed) { down_rule = 2; } // dot+space/enter+shift shortcut, see tap_lt()
    else                       { down_rule = 0; }
    if (map_shift(record, KC_RSFT, SHIFT, KC_GRV)) { return false; }
    break;
  case KC_QUES:
    down_rule = 0;                          // trap layer switching timimg issue between . and ?
    break;
  case KC_COMM:
    if (map_shift(record, KC_RSFT, SHIFT, KC_1)) { return false; }
    break;
#endif
  case AT_DOWN:
    tap_mods(record, KC_LALT);
    break;
  case CT_RGHT:
    tap_mods(record, KC_LGUI);
    break;
  case GT_UP:
    tap_mods(record, KC_LCTL);
    break;
#ifdef CENTER_TT
  case TT_ESC:
    tt_clear();                             // exit TT layer
    return false;
#endif
  case OS_ALT:
    tap_mods(record, KC_LALT);
    break;
  case OS_CTL:
    tap_mods(record, KC_LCTL);
    break;
  case OS_GUI:
    tap_mods(record, KC_LGUI);
    break;
  case SM_G:
    mt_shift(record, KC_LALT, KC_LSFT, KC_G);
    break;
  case SA_PERC:
    mt_shift(record, KC_LALT, 0, KC_5);
    break;
  case SG_TILD:
    mt_shift(record, KC_LGUI, 0, KC_GRV);
    break;
  case LT_H:
    if (map_shift(record, KC_LSFT, NOSHIFT, KC_SPC)) { return false; }
    tap_layer(record, _LSHIFT);
    break;
  case TD_ENT:
    if (record->event.pressed) { tap_rule = down_rule; } // down_rule persistance for tap_lt()
    break;
  case TD_SPC:
    if (record->event.pressed) { tap_rule = down_rule; } // down_rule persistance for tap_lt()
    // trap potential repeating enter caused by tap dance definition
    if (map_shift(record, KC_RSFT, NOSHIFT, KC_ENT)) { unregister_code(KC_ENT); return false; }
    tap_layer(record, _RSHIFT);
    break;
  case LT_ESC:
    if (map_shift(record, KC_LSFT, NOSHIFT, KC_TAB)) { return false; }
#ifdef CENTER_TT
    if (tt_keycode != 0) {
      tt_clear();                           // exit TT layer
      return false;
    }
#endif
    tap_layer(record, _LSYMBOL);
    thumb_roll(record, LEFT, 0, 0, _LSYMBOL, _RSYMBOL);
    break;
  case KC_TAB:
    if (record->event.pressed) { down_rule = 1; } // tab+enter thumb roll, see tap_lt()
    else                       { down_rule = 0; }
    break;
  case SL_TAB:
    thumb_roll(record, LEFT, SHIFT, KC_TAB, _MOUSE, _RSYMBOL);
    break;
  case SL_DEL:
    thumb_roll(record, RIGHT, NOSHIFT, KC_DEL, _MOUSE, _LSYMBOL);
    break;
  case LT_BSPC:
    if (map_shift(record, KC_RSFT, NOSHIFT, KC_DEL)) { return false; }
    tap_layer(record, _RSYMBOL);
    thumb_roll(record, RIGHT, 0, 0, _RSYMBOL, _LSYMBOL);
    break;
#ifdef CENTER_TT
  case CNTR_TL:
  case CNTR_TR:
  case CNTR_HL:
  case CNTR_HR:
  case CNTR_BL:
  case CNTR_BR:
    // return to base layer first if different TT layer selected
    if (tt_keycode != keycode && tt_keycode != 0) { tt_clear(); }
    tt_escape(record, keycode);
    break;
#endif
// #ifdef STENO_ENABLE
//   case PS_STNA:
//     stn_layer(record, STN_A, _LSYMBOL);
//     break;
//   case PS_STNO:
//     stn_layer(record, STN_O, _NUMBER);
//     break;
//   case PS_STNE:
//     stn_layer(record, STN_E, _FNCKEY);
//     break;
//   case PS_STNU:
//     stn_layer(record, STN_U, _RSYMBOL);
//     break;
// #endif
  case PLOVER:
    steno(record);
    return false;
  default:
    key_timer = 0;                          // regular keycode, clear timer in keycode_functions.h
  }
  return true;
}

#include "init.c"
