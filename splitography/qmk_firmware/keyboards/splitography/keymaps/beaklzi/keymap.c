// This is the canonical layout file for the Quantum project. If you want to add another keyboard,
// this is the style you want to emulate.
//
// To flash splitography / planck firmware
// ═══════════════════════════════════════
//   Reset keyboard or press hw reset button on base
//
//   cd qmk_firmware/keyboards/<keyboard>
//   sudo make KEYMAP=<keymap> dfu
//
//   sudo make clean           (good practice before flashing)
//   sudo make KEYMAP=<keymap> (to compile check)
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
//   #define PRIVATE_STRING includes private_string.h, a user defined code
//   block for the PRIV tap dance e.g. SEND_STRING("secret messape"),
//   see function private()
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
 ,_TTCAPS
 ,_SYMGUI
 ,_REGEX
 ,_MOUSE
 ,_NUMBER
 ,_FNCKEY
 ,_EDIT
 ,_TTFNCKEY
 ,_TTCURSOR
 ,_TTMOUSE
 ,_TTNUMBER
 ,_TTREGEX
 ,_PLOVER
#ifdef PLANCK
 ,_ADJUST
#endif
 ,_END_LAYERS
};

enum keyboard_keycodes {
  BASE = SAFE_RANGE
 ,BASE1
 ,BASE2
 ,HOME_A  // pseudo SFT_T(KC_A) disables auto repeat for shift
 ,HOME_T  // pseudo SFT_T(KC_T) disables auto repeat for shift
#ifndef HASKELL
 ,HS_LT   // pseudo CTL_T(S(KC_COMM))
 ,HS_GT   // pseudo SFT_T(S(KC_DOT))
#endif
 ,LT_SPC  // pseudo LT(_SYMGUI, KC_SPC)
 ,ML_BSLS
 ,PLOVER
 ,AST_G   // pseudo MT   (MOD_LALT | MOD_LSFT, S(KC_G))
 ,SST_A   // pseudo SFT_T(S(KC_A))
 ,SST_T   // pseudo SFT_T(S(KC_T))
 ,TT_I    // pseudo LT(_REGEX, S(KC_I))
 ,TT_SPC  // pseudo LT(_SYMGUI, KC_SPC)
};

// modifier keys
#ifdef PLANCK
#define AT_DOWN ALT_T(KC_DOWN)
#define CT_RGHT CTL_T(KC_RGHT)
#define GT_UP   GUI_T(KC_UP)
#endif
#define ACT_E   MT   (MOD_LALT | MOD_LCTL, KC_E)
#define AT_B    ALT_T(KC_B)
#define CT_C    CTL_T(KC_C)
#define ST_A    SFT_T(KC_A)

#define HOME_Q  GUI_T(KC_Q)
#define HOME_H  CTL_T(KC_H)
#define HOME_E  ALT_T(KC_E)
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
#define HS_LT   TD_LT
#define HS_GT   TD_GT
#else
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

#define LT_BSPC LT  (_EDIT, KC_BSPC)
#define LT_I    LT  (_REGEX, KC_I)
#ifdef PLANCK
#define LT_0    LT  (_ADJUST, KC_0)
#define LT_EQL  LT  (_ADJUST, KC_EQL)
#define LT_INS  LT  (_FNCKEY, KC_INS)
#define LT_LEFT LT  (_EDIT, KC_LEFT)
#define MO_ADJ  MO  (_ADJUST)
#endif
#define LT_ESC  LT  (_NUMBER, KC_ESC)
#define OS_ALT  OSM (MOD_LALT)
#define OS_CTL  OSM (MOD_LCTL)
#define OS_GUI  OSM (MOD_LGUI)
#define OS_SFT  OSM (MOD_LSFT)

#define TGL_TL  TT  (_TTFNCKEY)
#define TGL_TR  TT  (_TTCAPS)  // pseudo capslock to avoid TT key_timer conflicts
#define TGL_HL  TT  (_TTCURSOR)
#define TGL_HR  TT  (_TTMOUSE)
#define TGL_BL  TT  (_TTNUMBER)
#define TGL_BR  TT  (_TTREGEX)
#define TT_ESC  MO  (_NUMBER)

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
// ═════════════════════════════════════════════════════════════════════════════

#include "keycode_functions.c"

static uint8_t down_punc = 0;  // substitute (0) keycode (1) leader + one shot shift, see cap_lt()
static uint8_t dual_down = 0;  // dual keys down (2 -> 1 -> 0) reset on last up stroke, see TGL_TL, TGL_TR

bool process_record_user(uint16_t keycode, keyrecord_t *record)
{
  if (reshifted && !mod_down(KC_LSFT)) { unregister_code(KC_LSFT); reshifted = 0; }  // see map_shift()

  // ........................................................ Home Row Modifiers

  switch (keycode) {

  case HOME_Q:
  case HOME_W:
  case OS_GUI:
    tap_mods(record, KC_LGUI);
    break;
  case HOME_H:
  case HOME_S:
  case OS_CTL:
    tap_mods(record, KC_LCTL);
    break;
  case HOME_E:
  case HOME_R:
  case OS_ALT:
    tap_mods(record, KC_LALT);
    break;
  case HS_GT:                         // for rolling cursor to enter, del
  case OS_SFT:
    tap_mods(record, KC_LSFT);
    break;
  case HOME_A:
    tap_mods(record, KC_LSFT);
    mod_t   (record, KC_LSFT, KC_A);  // SFT_T replacement to circumvent auto repeat latency side effect
    break;
  case HOME_T:
    tap_mods(record, KC_RSFT);
    mod_t   (record, KC_RSFT, KC_T);  // SFT_T replacement to circumvent auto repeat latency side effect
    break;

  // ...................................................... Center Toggle Layers

  case TGL_TL:
    if (raise_layer(record, 0, LEFT, TOGGLE))  { dual_down = 2; return false; }  // defer reset!
    if (dual_down)                             { dual_down--; base_layer(dual_down); return false; }
    tt_escape(record, keycode);
    break;
  case TGL_TR:
    if (raise_layer(record, 0, RIGHT, TOGGLE)) { dual_down = 2; return false; }  // defer reset!
    if (dual_down)                             { dual_down--; base_layer(dual_down); return false; }
    tt_escape(record, keycode);
    break;
  case TGL_HL:
  case TGL_HR:
  case TGL_BL:
  case TGL_BR:
    tt_escape(record, keycode);
    break;

  // ...................................................... Outer Left Thumb Key

  case TT_ESC:
    if (map_shift  (record, KC_LSFT, SHIFT, KC_TAB))   { return false; }
    if (map_shift  (record, KC_RSFT, NOSHIFT, KC_TAB)) { return false; }
    if (key_press  (record))                           { base_layer(0); return false; }  // exit TT layer
    break;
  case LT_ESC:
    if (raise_layer(record, _FNCKEY, LEFT, ONDOWN))    { return false; }
    if (map_shift  (record, KC_LSFT, SHIFT, KC_TAB))   { return false; }
    if (map_shift  (record, KC_RSFT, NOSHIFT, KC_TAB)) { return false; }
    if (tt_keycode)                                    { base_layer(0); return false; }
    tap_layer(record, _NUMBER);
    break;

  // ...................................................... Inner Left Thumb Key

  case LT_I:
    if (raise_layer(record, _FNCKEY, RIGHT, ONDOWN))   { return false; }
#ifdef LEFT_SPACE
    if (map_shift  (record, KC_LSFT, NOSHIFT, KC_SPC)) { return false; }
#endif
    tap_layer      (record, _REGEX);
    rolling_layer  (record, LEFT, 0, 0, _REGEX, _SYMGUI);
    break;
  case TT_I:
    tap_layer      (record, _REGEX);
    lt             (record, _REGEX, SHIFT, KC_I);
    break;

  case TD_EQL:
    if (tt_keycode) { break; }  // no thumb mouse layer on toggle layer
    tap_layer      (record, _MOUSE);
    rolling_layer  (record, LEFT, 0, 0, _MOUSE, _SYMGUI);
    break;

  // ..................................................... Inner Right Thumb Key

  case ML_BSLS:
    tap_layer      (record, _MOUSE);
    rolling_layer  (record, RIGHT, NOSHIFT, KC_BSLS, _MOUSE, _REGEX);
    break;

  case LT_SPC:
#ifdef THUMB_CAPS
    if (raise_layer(record, _TTCAPS, LEFT, TOGGLE))      { return false; }
#endif
    if (leader_cap (record, _SYMGUI, down_punc, KC_SPC)) { return false; }                      // see KC_SPC for multi-tap
    if (mapc_shift (record, KC_LSFT, NOSHIFT, KC_ENT))   { layer_off(_SYMGUI); return false; }  // rolling cursor to enter
    if (map_shift  (record, KC_RSFT, NOSHIFT, KC_ENT))   { return false; }
    tap_layer      (record, _SYMGUI);
    lt             (record, _SYMGUI, NOSHIFT, KC_SPC);  // because LT() issues <spc> before <enter> on mapc_shift()
    rolling_layer  (record, RIGHT, 0, 0, _SYMGUI, _REGEX);
    break;
  case KC_SPC:
    if (leader_cap (record, 0, down_punc, KC_SPC))       { return false; }  // KC_SPC from LT_SPC -> (space)* space shift
    break;
  case TT_SPC:
#ifdef THUMB_CAPS
    if (raise_layer(record, _TTCAPS, LEFT, TOGGLE))      { return false; }
#endif
    if (mapc_shift (record, KC_LSFT, NOSHIFT, KC_ENT))   { layer_off(_SYMGUI); return false; }  // rolling cursor to enter
    if (map_shift  (record, KC_RSFT, NOSHIFT, KC_ENT))   { return false; }
    tap_layer      (record, _SYMGUI);
    lt             (record, _SYMGUI, NOSHIFT, KC_SPC);  // because LT() issues <spc> before <enter> on mapc_shift()
    break;

  // ..................................................... Outer Right Thumb Key

  case LT_BSPC:
#ifdef THUMB_CAPS
    if (raise_layer(record, _TTCAPS, RIGHT, TOGGLE))   { return false; }
#endif
    if (map_shift  (record, KC_LSFT, NOSHIFT, KC_DEL)) { layer_off(_SYMGUI); return false; }  // rolling cursor to del
    if (map_shift  (record, KC_RSFT, NOSHIFT, KC_DEL)) { return false; }
    if (leader_cap (record, _EDIT, down_punc, KC_ENT)) { return false; }                      // see KC_BSPC for multi-tap
    tap_layer      (record, _EDIT);
    break;
  case KC_BSPC:
#ifdef THUMB_CAPS
    if (raise_layer(record, _TTCAPS, RIGHT, TOGGLE))   { return false; }
#endif
    if (map_shift  (record, KC_LSFT, NOSHIFT, KC_DEL)) { return false; }
    if (map_shift  (record, KC_RSFT, NOSHIFT, KC_DEL)) { return false; }
    if (leader_cap (record, 0, down_punc, KC_ENT))     { return false; }  // KC_BSPC from LT_BSPC -> (enter)* enter shift
#ifdef THUMB_CAPS
    if (record->event.pressed)                         { key_timer = timer_read(); }
    else if (timer_elapsed(key_timer) < TAPPING_TERM)  { tap_key(KC_BSPC); }
    return false;  // capslock toggling trap, use shift bspc -> del for auto repeat
#else
    break;
#endif

  // ............................................................. Modifier Keys

  case AST_G:
    tap_mods(record, KC_LALT);
    mt_shift(record, KC_LALT, KC_LSFT, KC_G);
    break;
  case SST_A:
    tap_mods(record, KC_LSFT);
    mt_shift(record, KC_LSFT, 0, KC_A);
    break;
  case SST_T:
    tap_mods(record, KC_RSFT);
    mt_shift(record, KC_RSFT, 0, KC_T);
    break;
#ifndef HASKELL
  case HS_LT:
    tap_mods(record, KC_LCTL);
    mt_shift(record, KC_LCTL, 0, KC_COMM);
    break;
  case HS_GT:
    tap_mods(record, KC_LSFT);
    mt_shift(record, KC_LSFT, 0, KC_DOT);
    break;
#endif


  // ......................................................... Shift Mapped Keys

  case KC_COLN:
    if (map_shift(record, KC_RSFT, NOSHIFT, KC_COLN)) { return false; }
    break;
  case TD_COLN:
    if (mod_down(KC_RSFT))                            { unregister_code(KC_RSFT); }  // *must* un-shift before tap dance processing to register unshifted keycodes
    break;

  case KC_COMM:
    down_punc = (record->event.pressed) ? 1 : 0;  // dot/ques/exlm + space/enter + shift shortcut, see cap_lt()
    if (map_shift(record, KC_RSFT, NOSHIFT, KC_GRV))  { return false; }
    break;
  case KC_DOT:
    down_punc = (record->event.pressed) ? 1 : 0;  // dot/ques/exlm + space/enter + shift shortcut, see cap_lt()
    if (map_shift(record, KC_RSFT, SHIFT, KC_GRV))    { return false; }
    break;

  // ..................................................... Leader Capitalization
  
  case TD_TILD:
    if (mod_down(KC_RSFT)) { unregister_code(KC_RSFT); }  // *must* un-shift before tap dance processing to register unshifted keycodes
  case KC_EXLM:
  case KC_QUES:
    down_punc = (record->event.pressed) ? 1 : 0;          // dot/ques/exlm + space/enter + shift shortcut, see cap_lt()
    break;

  // ..................................................... Thumb Row Cursor Keys

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

  case PLOVER:
    steno(record);
    return false;
  case BASE1:
    if (raise_layer(record, 0, LEFT, TOGGLE))  { base_layer(0); return false; }
    return false;
  case BASE2:
    if (raise_layer(record, 0, RIGHT, TOGGLE)) { base_layer(0); return false; }
    return false;

  // ................................................................ Other Keys

  default:
    key_timer = 0;  // regular keycode, clear timer in keycode_functions.h
  }
  return true;
}

#include "init.c"
