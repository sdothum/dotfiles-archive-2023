// This is the canonical layout file for the Quantum project. If you want to add another keyboard,
// this is the style you want to emulate.
//
// To flash chimera firmware
// ═════════════════════════
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


#define CHIMERA

// #include "config.h"
#include "chimera_ergo_42.h"
// #include "action_layer.h"
// #include "eeconfig.h"

// extern keymap_config_t keymap_config;

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
#ifdef TEST
 ,_TEST
#endif
 ,_END_LAYERS
};

enum keyboard_keycodes {
  BASE = SAFE_RANGE
#ifdef ROLLOVER
 ,HOME_Q  // pseudo GUI_T(KC_A)
 ,HOME_H  // pseudo CTL_T(KC_H)
 ,HOME_E  // pseudo ALT_T(KC_E)
 ,HOME_A  // pseudo SFT_T(KC_A)
 ,HOME_T  // pseudo SFT_T(KC_T)
 ,HOME_R  // pseudo ALT_T(KC_R)
 ,HOME_S  // pseudo CTL_T(KC_S)
 ,HOME_W  // pseudo GUI_T(KC_W)
#endif
#ifndef HASKELL
 ,HS_LT   // pseudo CTL_T(S(KC_COMM))
 ,HS_GT   // pseudo SFT_T(S(KC_DOT))
#endif
 ,AST_G   // pseudo MT   (MOD_LALT | MOD_LSFT, S(KC_G))
 ,SST_A   // pseudo SFT_T(S(KC_A))
 ,SST_T   // pseudo SFT_T(S(KC_T))
 ,TT_ESC
 ,TT_I    // pseudo LT(_REGEX, S(KC_I))
 ,TT_SPC  // pseudo LT(_SYMGUI, KC_SPC)
};

// modifier keys
#define ACT_E   MT   (MOD_LALT | MOD_LCTL, KC_E)
#define AT_B    ALT_T(KC_B)
#define CT_C    CTL_T(KC_C)
#define ST_A    SFT_T(KC_A)

#ifndef ROLLOVER
#define HOME_Q  GUI_T(KC_Q)
#define HOME_H  CTL_T(KC_H)
#define HOME_E  ALT_T(KC_E)
#define HOME_A  SFT_T(KC_A)
#define HOME_T  SFT_T(KC_T)
#define HOME_R  ALT_T(KC_R)
#define HOME_S  CTL_T(KC_S)
#define HOME_W  GUI_T(KC_W)
#endif

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

#define LT_BSPC LT  (_MOUSE, KC_BSPC)
#define LT_ENT  LT  (_EDIT, KC_ENT)
#define LT_ESC  LT  (_FNCKEY, KC_ESC)
#define LT_I    LT  (_REGEX, KC_I)
#define LT_SPC  LT  (_SYMGUI, KC_SPC)
#define LT_TAB  LT  (_NUMBER, KC_TAB)
#define TT_TAB  LT  (_NUMBER, KC_TAB)
#define OS_ALT  OSM (MOD_LALT)
#define OS_CTL  OSM (MOD_LCTL)
#define OS_GUI  OSM (MOD_LGUI)
#define OS_SFT  OSM (MOD_LSFT)

#define TGL_TL  TT  (_TTFNCKEY)
#define TGL_HL  TT  (_TTCAPS)
#define TGL_BL  TT  (_TTMOUSE)
#define TGL_TR  TT  (_TTREGEX)
#define TGL_HR  TT  (_TTNUMBER)
#define TGL_BR  TT  (_TTCURSOR)

#ifdef TEST
#define DEBUG   TG  (_TEST)
#else
#define DEBUG   KC_NO
#endif


// ........................................................ Default Alpha Layout

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

#include "base_layout.h"

// ...................................................... Number / Function Keys

#include "number_fkey_layout.h"

// ......................................................... Symbol / Navigation

#include "symbol_guifn_layout.h"

// ............................................................... Toggle Layers

#include "toggle_layout.h"

// .............................................................. Mouse / Chords

#include "mouse_chord_layout.h"

};


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
#ifdef ROLLOVER
  case HOME_Q:
    mod_roll(record, LEFT, NOSHIFT, KC_LGUI, KC_Q, 0);  break;
  case HOME_H:
    mod_roll(record, LEFT, NOSHIFT, KC_LCTL, KC_H, 1);  break;
  case HOME_E:
    mod_roll(record, LEFT, NOSHIFT, KC_LALT, KC_E, 2);  break;
  case HOME_A:
    down_punc = (record->event.pressed) ? 1 : 0;  // space/enter + shift shortcut, see cap_lt()
    mod_roll(record, LEFT, SHIFT, KC_LSFT, KC_A, 3);    break;

  case HOME_T:
    mod_roll(record, RIGHT, SHIFT, KC_RSFT, KC_T, 6);   break;
  case HOME_R:
    mod_roll(record, RIGHT, NOSHIFT, KC_RALT, KC_R, 7); break;
  case HOME_S:
    mod_roll(record, RIGHT, NOSHIFT, KC_RCTL, KC_S, 8); break;
  case HOME_W:
    mod_roll(record, RIGHT, NOSHIFT, KC_RGUI, KC_W, 9); break;
#else
  case HOME_A:
    down_punc = (record->event.pressed) ? 1 : 0;  // space/enter + shift shortcut, see cap_lt()
    mod_bits(record, KC_LSFT);                          break;
  case HOME_T:
    mod_bits(record, KC_RSFT);                          break;
#endif

  // ............................................................. Toggle Layers

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

  // ........................................................... Left Thumb Keys

  case TT_ESC:
    base_layer(0);  // exit TT layer
    return false;
  case LT_ESC:
    if (tt_keycode)                                  { base_layer(0); return false; }
    break;

  case LT_I:
    if (map_shift(record, KC_LSFT, NOSHIFT, KC_SPC)) { return false; }
    break;
  case TT_I:
    lt(record, _REGEX, SHIFT, KC_I);
    break;
  case S(KC_I):
    if (map_shift(record, KC_LSFT, NOSHIFT, KC_SPC)) { return false; }
    if (!record->event.pressed)                      { clear_oneshot_layer_state(ONESHOT_PRESSED); }  // see leader_cap()
    break;

  case LT_TAB:
    if (map_shift(record, KC_RSFT, SHIFT, KC_TAB))   { return false; }
    if (map_shift(record, KC_LSFT, SHIFT, KC_ENT))   { return false; }
    break;

  // .......................................................... Right Thumb Keys

  case LT_ENT:
    if (leader_cap(record, _EDIT, down_punc, KC_ENT))   { return false; }  // KC_ENT -> enter shift
    break;
  case KC_ENT:
    if (leader_cap(record, 0, down_punc, KC_ENT))       { return false; }  // KC_ENT from LT_ENT -> enter enter* shift
    break;

  case LT_SPC:
    if (leader_cap(record, _SYMGUI, down_punc, KC_SPC)) { return false; }  // KC_SPC -> space shift
    break;
  case TT_SPC:
    lt(record, _SYMGUI, NOSHIFT, KC_SPC);
    break;
  case KC_SPC:
    if (!record->event.pressed)                         { clear_oneshot_layer_state(ONESHOT_PRESSED); }  // see leader_cap()
    break;

  case LT_BSPC:
  case KC_BSPC:
    if (!record->event.pressed)                         { clear_oneshot_layer_state(ONESHOT_PRESSED); }  // see leader_cap()
    if (map_shift(record, KC_LSFT, NOSHIFT, KC_DEL))    { layer_off(_SYMGUI); return false; }  // rolling cursor to del
    if (map_shift(record, KC_RSFT, NOSHIFT, KC_DEL))    { return false; }
    break;

  // ............................................................. Modifier Keys

  case AST_G:
    mt_shift(record, KC_LALT, KC_LSFT, KC_G); break;
  case SST_A:
    mt_shift(record, KC_LSFT, 0, KC_A);       break;
  case SST_T:
    mt_shift(record, KC_RSFT, 0, KC_T);       break;
#ifndef HASKELL
  case HS_LT:
    mt_shift(record, KC_LCTL, 0, KC_COMM);    break;
  case HS_GT:
    mt_shift(record, KC_LSFT, 0, KC_DOT);     break;
#endif

  // ......................................................... Shift Mapped Keys

  case KC_COLN:
    down_punc = (record->event.pressed) ? 1 : 0;  // semi/coln + space/enter + shift shortcut, see cap_lt()
    if (map_shift(record, KC_RSFT, NOSHIFT, KC_COLN)) { return false; }
    break;
  case TD_COLN:
    if (mod_down(KC_RSFT))                            { unregister_code(KC_RSFT); }  // *must* un-shift before tap dance processing to register unshifted keycodes
    down_punc = (record->event.pressed) ? 1 : 0;  // semi/coln + space/enter + shift shortcut, see cap_lt()
    break;

  case KC_COMM:
    down_punc = (record->event.pressed) ? 1 : 0;  // comm + space/enter + shift shortcut, see cap_lt()
    if (map_shift(record, KC_RSFT, NOSHIFT, KC_GRV))  { return false; }
    break;
  case KC_DOT:
    down_punc = (record->event.pressed) ? 1 : 0;  // dot + space/enter + shift shortcut, see cap_lt()
    if (map_shift(record, KC_RSFT, SHIFT, KC_GRV))    { return false; }
    break;

  // ..................................................... Leader Capitalization

  case TD_TILD:
    if (mod_down(KC_RSFT)) { unregister_code(KC_RSFT); }  // *must* un-shift before tap dance processing to register unshifted keycodes
  case KC_EXLM:
  case KC_QUES:
    down_punc = (record->event.pressed) ? 1 : 0;          // dot/ques/exlm + space/enter + shift shortcut, see cap_lt()
    break;

  // .............................................................. Top Row Keys

#ifdef ROLLOVER
  case KC_Y:
    mod_roll(record, LEFT, NOSHIFT, 0, KC_Y, 1);    return false;
  case KC_O:
    mod_roll(record, LEFT, NOSHIFT, 0, KC_O, 2);    return false;
  case KC_U:
    mod_roll(record, LEFT, NOSHIFT, 0, KC_U, 3);    return false;

  case KC_G:
    mod_roll(record, RIGHT, NOSHIFT, 0, KC_G, 5);   return false;
  case KC_D:
    mod_roll(record, RIGHT, NOSHIFT, 0, KC_D, 6);   return false;
  case KC_N:
    mod_roll(record, RIGHT, NOSHIFT, 0, KC_N, 7);   return false;
  case KC_M:
    mod_roll(record, RIGHT, NOSHIFT, 0, KC_M, 8);   return false;

  // ........................................................... Middle Row Keys 

  case KC_C:
    mod_roll(record, RIGHT, NOSHIFT, 0, KC_C, 5);   return false;

  // ........................................................... Bottom Row Keys

  case KC_MINS:
    mod_roll(record, LEFT, NOSHIFT, 0, KC_MINS, 1); return false;
  case KC_QUOT:
    mod_roll(record, LEFT, NOSHIFT, 0, KC_QUOT, 2); return false;
  case KC_K:
    mod_roll(record, LEFT, NOSHIFT, 0, KC_K, 3);    return false;

  case KC_B:
    mod_roll(record, RIGHT, NOSHIFT, 0, KC_B, 5);   return false;
  case KC_P:
    mod_roll(record, RIGHT, NOSHIFT, 0, KC_P, 6);   return false;
  case KC_L:
    mod_roll(record, RIGHT, NOSHIFT, 0, KC_L, 7);   return false;
  case KC_F:
    mod_roll(record, RIGHT, NOSHIFT, 0, KC_F, 8);   return false;
#endif

  // ................................................................ Other Keys

  default:
    if (!record->event.pressed) { clear_oneshot_layer_state(ONESHOT_PRESSED); }  // see leader_cap()
    key_timer  = 0;  // regular keycode, clear timer in keycode_functions.h
  }
  return true;
}


// Initialization
// ═════════════════════════════════════════════════════════════════════════════

void matrix_init_user(void)
{
  base_layer(0);
}


// Layer States
// ═════════════════════════════════════════════════════════════════════════════

void matrix_scan_user(void) {
  uint8_t layer = biton32(layer_state);
  
  switch (layer) {
  case _BASE:
    set_led_blue;    break;
  case _SHIFT:
  case _TTCAPS:
    set_led_cyan;    break;
  case _NUMBER:
  case _TTNUMBER:
    set_led_green;   break;
  case _REGEX:
  case _SYMGUI:
  case _TTREGEX:
    set_led_red;     break;
  case _MOUSE:
  case _TTCURSOR:
  case _TTMOUSE:
    set_led_magenta; break;
  case _FNCKEY:
  case _TTFNCKEY:
    set_led_green;   break;
  case _EDIT:
  default:
    set_led_yellow;  break;
  }
}

