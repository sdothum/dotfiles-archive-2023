// This is the canonical layout file for the Quantum project. If you want to add another keyboard,
// this is the style you want to emulate.
//
// To flash splitography / planck firmware
// ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
//   Reset keyboard or press hw reset button on base
//
//   cd qmk_firmware/keyboards/<keyboard>
//   sudo make KEYMAP=<keymap> dfu
//
//   sudo make clean           (good practice before flashing)
//   sudo make KEYMAP=<keymap> (to compile check)
//
// Package requirements (for arch linux)
// ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
//   avr-gcc-atmel
//   avr-libc-atmel
//   dfu-programmer
//
// Notes
// ▔▔▔▔▔
//   ** E R G O   W I D E   S P L I T ** Layout
//
//   Autocompletion tap dance key pairs (),[],{} are available from the
//   number/symbol layer, as well as, numerous (un)shift key values
//
//   #define PRIVATE_STRING includes private_string.h, a user defined code
//   block for the PRIV tap dance e.g. SEND_STRING("secret messape"),
//   see function private()
//
// Code
// ▔▔▔▔
//   This source is shamelessly based on the "default" planck layout
//
//   #ifdef/#endif block structures are not indented, as syntax highlighting
//   in vim is sufficient for identification
//
//   c++ commenting style is used throughout
//
// Change history
// ▔▔▔▔▔▔▔▔▔▔▔▔▔▔
//   See http://thedarnedestthing.com/colophon


//                === N O T E ===
//
// sudo CPATH=<keymap.c directory>/common make ...


#ifndef PLANCK
#ifndef SPLITOGRAPHY
#define SPLITOGRAPHY
#endif
#endif

#include "config.h"
#ifdef SPLITOGRAPHY
#include "splitography.h"
#else
#include "planck.h"
#endif
#include "action_layer.h"
#ifdef STENO_ENABLE
#include "keymap_steno.h"
#endif
#ifdef AUDIO_ENABLE
#include "audio.h"
#endif
#include "eeconfig.h"

extern keymap_config_t keymap_config;

enum keyboard_layers {
  _BASE = 0
 ,_SHIFT
 ,_GUIFN
 ,_SYMBOL
 ,_MOUSE
 ,_NUMBER
 ,_FNCKEY
 ,_PLOVER
 ,_EDIT
#ifdef PLANCK
 ,_ADJUST
#endif
 ,_TTFNCKEY
 ,_TTCAPS
 ,_TTCURSOR
 ,_TTMOUSE
 ,_TTNUMBER
 ,_TTREGEX
 ,_END_LAYERS
};

enum keyboard_keycodes {
  BASE = SAFE_RANGE
 ,BASE1
 ,BASE2
 ,PLOVER
 ,LT_I      // pseudo LT   (_SYMBOL, KC_I)                for shifted key-codes, see process_record_user()
 ,SG_TILD   // pseudo GUI_T(S(KC_GRV))                    for shifted key-codes, see process_record_user()
 ,SM_G      // pseudo MT   (MOD_LALT | MOD_LSFT, S(KC_G)) for shifted key-codes, see process_record_user()
 ,SS_A      // pseudo SFT_T(S(KC_A))                      for shifted key-codes, see process_record_user()
 ,SS_T      // pseudo SFT_T(S(KC_T))                      for shifted key-codes, see process_record_user()
 ,TT_ESC
 ,ML_BSLS
 ,ML_EQL
#ifdef STENO_ENABLE
 ,PS_STNA = STN_A
 ,PS_STNO = STN_O
 ,PS_STNE = STN_E
 ,PS_STNU = STN_U
#else
 ,LT_C    = LT (_SYMBOL, KC_C)
 ,LT_V    = LT (_NUMBER, KC_V)
 ,LT_N    = LT (_FNCKEY, KC_N)
 ,LT_M    = LT (_GUIFN, KC_M)
#endif
};

// modifier keys
#ifdef PLANCK
#define CT_RGHT CTL_T(KC_RGHT)
#define AT_DOWN ALT_T(KC_DOWN)
#define GT_UP   GUI_T(KC_UP)
#endif
#define AT_B    ALT_T(KC_B)
#define CT_C    CTL_T(KC_C)
#define MT_E    MT   (MOD_LCTL | MOD_LALT, KC_E)
#define ST_A    SFT_T(KC_A)

#define HOME_Q  GUI_T(KC_Q)
#define HOME_H  CTL_T(KC_H)
#define HOME_E  ALT_T(KC_E)
#define HOME_A  SFT_T(KC_A)
#define HOME_T  SFT_T(KC_T)
#define HOME_R  ALT_T(KC_R)
#define HOME_S  CTL_T(KC_S)
#define HOME_W  GUI_T(KC_W)

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
#else
#define HS_COLN KC_COLN
#define HS_LT   KC_LT
#define HS_GT   KC_GT
#endif

#define COPY    LCTL(KC_C)
#define CUT     LCTL(KC_X)
#define EOT     LCTL(KC_D)
#define NAK     LCTL(KC_U)
#define PASTE   TD_PASTE
#define UNDO    LCTL(KC_Z)
#define XCOPY   LCTL(LSFT(KC_C))
#define XPASTE  TD_XPASTE

#define TT_SPC  LT  (_TTCURSOR, KC_SPC)
#ifdef PLANCK
#define LT_DEL  LT  (_ADJUST, KC_DEL)
#define LT_INS  LT  (_FNCKEY, KC_INS)
#define LT_LEFT LT  (_EDIT, KC_LEFT)
#endif
#define LT_ESC  LT  (_NUMBER, KC_ESC)
#define OS_ALT  OSM (MOD_LALT)
#define OS_CTL  OSM (MOD_LCTL)
#define OS_GUI  OSM (MOD_LGUI)
#define OS_SFT  OSM (MOD_LSFT)

#define CNTR_TL TT  (_TTFNCKEY)
#define CNTR_TR TT  (_TTCAPS)               // pseudo capslock to avoid TT key_timer conflicts
#define CNTR_HL TT  (_TTCURSOR)
#define CNTR_HR TT  (_TTMOUSE)
#define CNTR_BL TT  (_TTNUMBER)
#define CNTR_BR TT  (_TTREGEX)

// ........................................................ Default Alpha Layout

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

#include "base_layout.h"
#include "steno_layout.h"

  // ...................................................... Number / Function Keys

#include "number_fkey_layout.h"

  // ......................................................... Symbol / Navigation

#include "symbol_guifn_layout.h"

  // ............................................................... Toggle Layers

#include "toggle_layout.h"

  // ......................................................... Short Cuts / Adjust

#include "chord_layout.h"

};

// ...................................................................... Sounds

#include "sounds.h"


// User Keycode Trap
// ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔

#include "keycode_functions.c"

#define BASE_1  1
#define BASE_2  2
#define BASE_12 3
static uint8_t base_n    = 0;

static uint8_t down_punc = 0;               // substitute (0) keycode (1) leader + one shot shift, see cap_lt()

bool process_record_user(uint16_t keycode, keyrecord_t *record)
{
  // ........................................................ Home Row Modifiers

  switch (keycode) {
  case HOME_Q:
  case HOME_W:
    tap_mods(record, KC_LGUI);
    break;
  case HOME_H:
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

  case OS_ALT:
    tap_mods(record, KC_LALT);
    break;
  case OS_CTL:
    tap_mods(record, KC_LCTL);
    break;
  case OS_GUI:
    tap_mods(record, KC_LGUI);
    break;

  // ...................................................... Center Toggle Layers

  case CNTR_TL:
  case CNTR_TR:
  case CNTR_HL:
  case CNTR_HR:
  case CNTR_BL:
  case CNTR_BR:
    // return to base layer first if different TT layer selected
    if (tt_keycode != keycode && tt_keycode) { tt_clear(); }
    tt_escape(record, keycode);
    break;

  case TT_ESC:
    if (map_shift(record, KC_LSFT, SHIFT, KC_TAB))   { return false; }
    if (map_shift(record, KC_RSFT, NOSHIFT, KC_TAB)) { return false; }
    tt_clear();                             // exit TT layer
    return false;

  // ................................................................ Thumb Keys

  case LT_ESC:
    if (raise_layer(record, _FNCKEY, LEFT, ONOFF))   { return false; }
    if (map_shift(record, KC_LSFT, SHIFT, KC_TAB))   { return false; }
    if (map_shift(record, KC_RSFT, NOSHIFT, KC_TAB)) { return false; }
    if (tt_keycode)                                  { tt_clear(); return false; }
    tap_layer(record, _NUMBER);
    break;

  case ML_EQL:
    tap_layer    (record, _MOUSE);
    rolling_layer(record, LEFT, NOSHIFT, KC_EQL, _MOUSE, _GUIFN);
    break;
  case LT_I:
    if (raise_layer(record, _FNCKEY, RIGHT, ONOFF)) { return false; }
    lt_shift     (record, shift_mod(KC_RSFT) ? SHIFT : NOSHIFT, KC_I, _SYMBOL); // maintain repeating tap case
    tap_layer    (record, _SYMBOL);
    rolling_layer(record, LEFT, 0, 0, _SYMBOL, _GUIFN);
    break;

  case TT_SPC:
    if (map_shift(record, KC_LSFT, NOSHIFT, KC_ENT)) { return false; }
    if (map_shift(record, KC_RSFT, NOSHIFT, KC_ENT)) { return false; }
    break;
  case TD_SPC:
    if (raise_layer(record, _TTCAPS, LEFT, TOGGLE))  { return false; }
    if (record->event.pressed)                       { auto_cap = down_punc; } // down_punc persistance for cap_lt()
    if (map_shift(record, KC_LSFT, NOSHIFT, KC_ENT)) { return false; }
    if (map_shift(record, KC_RSFT, NOSHIFT, KC_ENT)) { return false; }
    rolling_layer(record, RIGHT, 0, 0, _GUIFN, _SYMBOL);
    tap_layer    (record, _GUIFN);
    break;

  case KC_BSPC:
    if (map_shift(record, KC_LSFT, NOSHIFT, KC_DEL)) { return false; }
    break;
  case TD_BSPC:
    if (raise_layer(record, _TTCAPS, RIGHT, TOGGLE)) { return false; }
    if (map_shift(record, KC_LSFT, NOSHIFT, KC_DEL)) { return false; }
    if (record->event.pressed)                       { auto_cap = down_punc; } // down_punc persistance for cap_lt()
    tap_layer(record, _EDIT);
    break;

  // .............................................................. Special Keys

  case ML_BSLS:
    tap_layer    (record, _MOUSE);
    rolling_layer(record, RIGHT, NOSHIFT, KC_BSLS, _MOUSE, _SYMBOL);
    break;
  case KC_COLN:
  case TD_EMOJ:
    if (map_shift(record, KC_RSFT, NOSHIFT, KC_COLN)) { return false; }
    break;
  case KC_COMM:
    if (tt_keycode && map_shift(record, KC_RSFT, SHIFT, KC_1)) { return false; }
    if (map_shift(record, KC_RSFT, NOSHIFT, KC_GRV))           { return false; }
    break;
  // smart capitalization chords (on down)
  case TD_TILD:
    if (shift_mod(KC_RSFT)) { unregister_code(KC_LSFT); } // un-shift before tap dance processing to register unshifted keycodes, see tilde()
  case KC_EXLM:
  case KC_QUES:
    down_punc = (record->event.pressed) ? 1 : 0;          // dot/ques/exlm + space/enter + shift shortcut, see cap_lt()
    break;
  case KC_DOT:
    if (map_shift(record, KC_RSFT, SHIFT, KC_SLSH)) { return false; }
    if (map_shift(record, KC_LSFT, SHIFT, KC_SLSH)) { return false; }
    break;
  case SM_G:
    mt_shift(record, KC_LALT, KC_LSFT, KC_G);
    break;
  case SS_A:
    tap_mods(record, KC_LSFT);
    mt_shift(record, KC_LSFT, 0, KC_A);
    break;
  case SS_T:
    tap_mods(record, KC_RSFT);
    mt_shift(record, KC_LSFT, 0, KC_T);
    break;
  case SG_TILD:
    mt_shift(record, KC_LGUI, 0, KC_GRV);
    break;

  // ............................................................ Thumb Row Keys

#ifdef PLANCK
  case AT_DOWN:
    tap_mods(record, KC_LALT);
    break;
  case CT_RGHT:
    tap_mods(record, KC_LGUI);
    break;
  case GT_UP:
    tap_mods(record, KC_LCTL);
    break;
#endif

  // ................................................................ Steno Keys

#define SET_BASE if (base_n == BASE_12) { base_layer(); }

  case PLOVER:
    steno(record);
    return false;
  case BASE1:
    if (record->event.pressed) { base_n = base_n | BASE_1; SET_BASE }
    else                       { base_n = base_n & ~BASE_1; }
    return false;
  case BASE2:
    if (record->event.pressed) { base_n = base_n | BASE_2; SET_BASE }
    else                       { base_n = base_n & ~BASE_2; }
    return false;

  // ................................................................ Other Keys

  default:
    key_timer = 0;                          // regular keycode, clear timer in keycode_functions.h
  }

  return true;
}

#include "init.c"
