// sdothum - 2016 (c) wtfpl

// This is the canonical layout file for the Quantum project. If you want to add another keyboard,
// this is the style you want to emulate.
//
// To flash corne / chimera / planck firmware
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
//
//                === N O T E ===
//
// sudo CPATH=<keymap.c directory>/common make ...

// Hardware
// ═════════════════════════════════════════════════════════════════════════════

#ifdef CORNE
#define KEYMAP LAYOUT
#include QMK_KEYBOARD_H
#ifdef RGBLIGHT_ENABLE
//Following line allows macro to read current RGB settings
extern rgblight_config_t rgblight_config;
#endif
extern uint8_t is_master;
#endif

#ifdef CHIMERA
// #include "config.h"
#include "chimera_ergo_42.h"
// #include "action_layer.h"
// #include "eeconfig.h"
// extern keymap_config_t keymap_config;
#endif

#ifdef PLANCK
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
#endif

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

// Keymaps
// ═════════════════════════════════════════════════════════════════════════════

extern keymap_config_t keymap_config;

// ...................................................................... Layers

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
 ,_TTBASEL
 ,_TTBASER
 ,_TTFNCKEY
 ,_TTCURSOR
 ,_TTMOUSE
 ,_TTNUMBER
 ,_TTREGEX
#ifdef STENO_ENABLE
 ,_PLOVER
#endif
#ifdef PLANCK
 ,_ADJUST
#endif
#ifdef TEST
 ,_TEST
#endif
 ,_END_LAYERS
};

// .................................................................... Keycodes

#include "keycodes.h"

// .............................................................. Tapdance Codes

#include "tapcodes.h"

// Layouts
// ═════════════════════════════════════════════════════════════════════════════

// ........................................................ Default Alpha Layout

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

#include "base_layout.h"
#ifdef STENO_ENABLE
#include "steno_layout.h"
#endif

// ...................................................... Number / Function Keys

#include "number_fkey_layout.h"

// ......................................................... Symbol / Navigation

#include "symbol_guifn_layout.h"

// ............................................................... Toggle Layers

#include "toggle_layout.h"

// .............................................................. Mouse / Chords

#include "mouse_chord_layout.h"

};

#ifdef PLANCK
// ...................................................................... Sounds

#include "sounds.h"
#endif

// User Keycode Trap
// ═════════════════════════════════════════════════════════════════════════════

#include "keycode_functions.c"
#include "tapdance.c"

// ..................................................... Dynamic Pinkie Stagger!

static uint16_t pinkies[][3] = { {KC_X, KC_V, KC_Z},    // ZVX beakl wi (row 3 -> 1)
                                 {KC_V, KC_X, KC_Z},    // ZXV beakl wi-v
                                 {KC_V, KC_Z, KC_X} };  // XZV beakl wi-x
static uint8_t  stagger      = PINKIE_STAGGER;          // pinkie on (0) home row (1,2) bottom row stagger variant, see case STAGGER

#define PINKIE(r) pinkies[stagger][r - 1]

// ............................................................... Keycode Cycle

#define LEADERCAP         leadercap = KEY_DOWN ? 1 : 0
#define MOD_ROLL(m, k, c) mod_roll(record, m, 0, 0, k, c)

bool process_record_user(uint16_t keycode, keyrecord_t *record)
{
#ifdef CORNE
  if (record->event.pressed) {
#ifdef SSD1306OLED
    set_keylog(keycode, record);
#endif
    // set_timelog();
  }
#endif

  if (reshifted && !MOD_DOWN(KC_LSFT)) { unregister_code(KC_LSFT); reshifted = 0; }  // see map_shift()

// ...................................................... Smart Keypad Delimiter

static uint16_t postfix    = KC_SPC;  // see case DELIM
static bool     numerating = 0;       // see case LT_TAB
static bool     smart      = 1;       // see case SMART

#ifdef SMART_DELIM
  if (numerating && smart) {
    switch (keycode) {
    case KC_0:  LEADERCAP;  // DELIM -> 0x
    case KC_1:
    case KC_2:
    case KC_3:
    case KC_4:
    case KC_5:
    case KC_6:
    case KC_7:
    case KC_8:
    case KC_9:
      postfix = KC_G;       // Vim ..G'oto
      break;
    case DELIM:
      break;                // apply context rule
    default:
      postfix = KC_SPC;
    }
  } else { postfix = KC_SPC; }
#endif

// Home Row
// ═════════════════════════════════════════════════════════════════════════════

// .......................................................... Home Row Modifiers

#define HOME_ROLL(m, k, c) MOD_ROLL(m, k, c); break

  switch (keycode) {
#ifdef ROLLOVER
  case HOME_Q:  HOME_ROLL(KC_LGUI, KC_Q,      0);
  case HOME_H:  HOME_ROLL(KC_LCTL, KC_H,      1);
  case HOME_E:  HOME_ROLL(KC_LALT, KC_E,      2);
  case HOME_A:
    LEADERCAP;  HOME_ROLL(KC_LSFT, KC_A,      3);          // space/enter + shift shortcut, see leader_cap()

  case HOME_T:  HOME_ROLL(KC_RSFT, KC_T,      6);
  case HOME_R:  HOME_ROLL(KC_RALT, KC_R,      7);
  case HOME_S:  HOME_ROLL(KC_RCTL, KC_S,      8);
  case PINKY2:  HOME_ROLL(KC_RGUI, PINKIE(2), 9);
#else
  case HOME_A:
    LEADERCAP;  MOD_BITS(KC_LSFT);                  break  // space/enter + shift shortcut, see leader_cap()
  case HOME_T:  MOD_BITS(KC_RSFT);                  break;
  case PINKY2:  toggle(record, KC_RGUI, PINKIE(2)); break;
#endif

// Thumb Keys
// ═════════════════════════════════════════════════════════════════════════════

#ifdef SPLITOGRAPHY
#include "steno_thumbs_keymap.c"
#else

// ............................................................. Left Thumb Keys

  case TT_ESC:  base_layer(0); return false;     // exit TT layer
  case LT_ESC:  if (tt_keycode) { base_layer(0); return false; }; break;

  case LT_I:
#ifdef LEFT_SPC_ENT
    if (map_shifted(record, KC_LSFT, LOWER, KC_SPC, _REGEX)) { return false; }  // non-autorepeating
#endif
#ifdef ROLLOVER
    if (MOD_ROLL(0, KC_I, 4)) { return false; }  // MO(_REGEX) -> LT(_REGEX, KC_I)
#endif
    break;
  case TT_I:  layer_toggle(record, _REGEX, UPPER, KC_I); break;

  case LT_TAB:
    numerating = KEY_DOWN ? 1 : 0;
#ifdef LEFT_SPC_ENT
    if (map_shift(record, KC_LSFT, LOWER, KC_ENT)) { return false; }
#endif
    if (map_shift(record, KC_RSFT, UPPER, KC_TAB)) { return false; }
    break;

// ............................................................ Right Thumb Keys

#ifdef ROLLOVER
  case LT_ENT:
    leaderlayer = _EDIT;                            // see mod_roll()
    if (MOD_ROLL(0, KC_ENT, 10)) { return false; }  // KC_ENT -> enter shift
    break;
  case KC_ENT:
    if (MOD_ROLL(0, KC_ENT, 10)) { return false; }  // KC_ENT from LT_ENT -> enter enter* shift
    break;

  case LT_SPC:
    leaderlayer = _SYMGUI;                          // see mod_roll()
    if (MOD_ROLL(0, KC_SPC, 11)) { return false; }  // KC_SPC -> space shift
    break;
#else
  case LT_ENT:
    if (leader_cap(record, _EDIT, KC_ENT)) { return false; }  // KC_ENT -> enter shift
    break;
  case KC_ENT:
    if (leader_cap(record, 0, KC_ENT))     { return false; }  // KC_ENT from LT_ENT -> enter enter* shift
    break;

  case LT_SPC:
    if (leader_cap(record, _SYMGUI, KC_SPC)) { return false; }  // KC_SPC -> space shift
    break;
#endif
  case TT_SPC:
    layer_toggle(record, _SYMGUI, LOWER, KC_SPC);
    break;
  case KC_SPC:
    if (!KEY_DOWN) { CLR_1SHOT; }  // see leader_cap()
    break;

  case LT_BSPC:
  case KC_BSPC:
    if (!KEY_DOWN) { CLR_1SHOT; }  // see leader_cap()
    if (map_shift(record, KC_LSFT, LOWER, KC_DEL)) { layer_off(_SYMGUI); return false; }  // rolling cursor to del
    if (map_shift(record, KC_RSFT, LOWER, KC_DEL)) { return false; }
    break;
#endif

// Key Pad
// ═════════════════════════════════════════════════════════════════════════════

// .................................................................... HEX Keys

static bool hexcase = HEXADECIMAL_CASE;  // hex case (0) lower case abcdef (1) upper case ABCDEF, see case HEXCASE

#ifdef ROLLOVER
#define HEX(m, m2, k, c) mod_roll(record, m, m2, hexcase, k, c); break

  case HEX_A:  HEX(0,       0,       KC_A, 1);
  case HEX_B:  HEX(KC_LALT, KC_LCTL, KC_B, 2);
  case HEX_C:  HEX(0,       0,       KC_C, 3);
  case HEX_D:  HEX(KC_LCTL, 0,       KC_D, 1);
  case HEX_E:  HEX(KC_LALT, 0,       KC_E, 2);
  case HEX_F:  HEX(KC_LSFT, 0,       KC_F, 3);
#else
#define HEX(m, m2, k) mod_tap(record, m, m2, hexcase, k)

  case HEX_A:  HEX(0,       0,       KC_A);
  case HEX_B:  HEX(KC_LALT, KC_LCTL, KC_B);
  case HEX_C:  HEX(0,       0,       KC_C);
  case HEX_D:  HEX(KC_LCTL, 0,       KC_D);
  case HEX_E:  HEX(KC_LALT, 0,       KC_E);
  case HEX_F:  HEX(KC_LSFT, 0,       KC_F);
#endif

// ......................................................... Numpad Bracket Keys

static uint16_t brkts[][3] = { {LOWER, KC_LBRC, KC_RBRC},    // [] (side 1 -> 2)
                               {UPPER, KC_9,    KC_0},       // ()
                               {UPPER, KC_LCBR, KC_RCBR} };  // {}
static uint8_t  brktype    = 0;                              // default (0) [], see case BRKTYPE

#ifdef ROLLOVER
#define BRACKET(m, m2, s, c) mod_roll(record, m, m2, brkts[brktype][0], brkts[brktype][s], c); break

  case L_BRKT:  BRACKET(0,       0,       LEFT,  1);
  case R_BRKT:  BRACKET(KC_LALT, KC_LSFT, RIGHT, 2);
#else
#define BRACKET(m, m2, s) mod_tap(record, m, m2, brkts[brktype][0], brkts[brktype][s]); break

  case L_BRKT:  BRACKET(0,       0,       LEFT);
  case R_BRKT:  BRACKET(KC_LALT, KC_LSFT, RIGHT);
#endif

// ............................................................. Smart Delimiter

#define POSTCASE postfix == KC_G ? UPPER : LOWER

#ifdef ROLLOVER
  case DELIM:
    if (leadercap) { mod_roll(record, 0, 0, LOWER,    KC_X,    3); }  // 0x
    else           { mod_roll(record, 0, 0, POSTCASE, postfix, 3); }  // smart vim goto
    break;
#else
  case DELIM:
    if (leadercap) { mod_tap(record, 0, 0, LOWER,    KC_X); }         // 0x
    else           { mod_tap(record, 0, 0, POSTCASE, postfix); }      // smart vim goto
    break;
#endif

// Symbols
// ═════════════════════════════════════════════════════════════════════════════

// ........................................................... Shift Mapped Keys

#ifndef HASKELL
  case HS_GT:  mod_tap(record, KC_LSFT, 0, UPPER, KC_DOT);  break;
  case HS_LT:  mod_tap(record, KC_LCTL, 0, UPPER, KC_COMM); break;
#endif

// ......................................................... Shift Mapped Leader

#ifdef ROLLOVER
  case KC_COLN:
    LEADERCAP;  // semi/colon + space/enter + shift shortcut, see leader_cap()
    if (map_leader(record, KC_RSFT, LOWER, KC_COLN, 4)) { return false; }
    break;
#ifdef HASKELL
  case TD_COLN:
    if (MOD_DOWN(KC_RSFT)) { unregister_code(KC_RSFT); }  // *must* un-shift before tap dance processing to register unshifted keycodes
    LEADERCAP;  // semi/colon + space/enter + shift shortcut, see leader_cap()
    set_leader(record, KC_COLN, 4);
    break;
#endif

  case KC_COMM:
    LEADERCAP;  // comma + space/enter + shift shortcut, see leader_cap()
    if (map_leader(record, KC_RSFT, LOWER, KC_GRV, 4)) { return false; }
    break;
#else
  case KC_COLN:
    LEADERCAP;  // semi/colon + space/enter + shift shortcut, see leader_cap()
    if (map_shift(record, KC_RSFT, LOWER, KC_COLN)) { return false; }
    break;
#ifdef HASKELL
  case TD_COLN:
    if (MOD_DOWN(KC_RSFT)) { unregister_code(KC_RSFT); }  // *must* un-shift before tap dance processing to register unshifted keycodes
    LEADERCAP;  // semi/colon + space/enter + shift shortcut, see leader_cap()
    break;
#endif

  case KC_COMM:
    LEADERCAP;  // comma + space/enter + shift shortcut, see leader_cap()
    if (map_shift(record, KC_RSFT, LOWER, KC_GRV)) { return false; }
    break;
#endif

// ...................................................... Shift Mapped Tap Dance

#ifdef UNIX
static uint16_t td_timer = 0;  // pseudo tapdance timer

#define TAPDANCE if (KEY_DOWN) { td_timer = KEY_TAPPED(td_timer) ? 0 : timer_read(); }
#endif
#ifdef ROLLOVER
  case KC_DOT:
    LEADERCAP;  // dot + space/enter + shift shortcut, see leader_cap()
#ifdef UNIX
    TAPDANCE; if (map_leader(record, KC_RSFT, td_timer ? UPPER : LOWER, td_timer ? KC_GRV : KC_SLSH, 4)) { return false; }  // pseudo tapdance ~ -> ~/
#else
    if (map_leader(record, KC_RSFT, UPPER, KC_GRV, 4)) { return false; }
#endif
    break;
#else
  case KC_DOT:
    LEADERCAP;  // dot + space/enter + shift shortcut, see leader_cap()
#ifdef UNIX
    TAPDANCE; if (map_shift(record, KC_RSFT, td_timer ? UPPER : LOWER, td_timer ? KC_GRV : KC_SLSH)) { return false; }  // pseudo tapdance ~ -> ~/
#else
    if (map_shift(record, KC_RSFT, UPPER, KC_GRV)) { return false; }
#endif
    break;
#endif

// ....................................................... Leader Capitalization

  case KC_EXLM:
  case KC_QUES:
    LEADERCAP;  // exclamation/question + space/enter + shift shortcut, see leader_cap()
#ifdef ROLLOVER
    if (map_leader(record, 0, LOWER, keycode, 4)) { return false; }
#endif
    break;

// Alpha Keys
// ═════════════════════════════════════════════════════════════════════════════

// ............................................................... Modifier Keys

  case TT_A:  layer_toggle(record, _TTBASEL, UPPER, KC_A);  break;
  case TT_T:  layer_toggle(record, _TTBASER, UPPER, KC_T);  break;

// ..................................................... Remaining Rollover Keys

#ifdef ROLLOVER
#define CASE_ROLL(k, c) case k: MOD_ROLL(0, k, c); return false

  CASE_ROLL(KC_Y,    1);  // top row 3
  CASE_ROLL(KC_O,    2);
  CASE_ROLL(KC_U,    3);
  CASE_ROLL(KC_MINS, 4);

  CASE_ROLL(KC_G,    5);
  CASE_ROLL(KC_D,    6);
  CASE_ROLL(KC_N,    7);
  CASE_ROLL(KC_M,    8);
  case PINKY3:
    MOD_ROLL(0, PINKIE(3), 9); return false;

  CASE_ROLL(KC_W,    4);  // middle row 2
  CASE_ROLL(KC_C,    5);

  CASE_ROLL(KC_J,    0);  // bottom row 1
  CASE_ROLL(KC_K,    3);
  CASE_ROLL(KC_QUOT, 4);

  CASE_ROLL(KC_B,    5);
  CASE_ROLL(KC_P,    6);
  CASE_ROLL(KC_L,    7);
  CASE_ROLL(KC_F,    8);
  case PINKY1:
    MOD_ROLL(0, PINKIE(1), 9); return false;
#endif

// Layers
// ═════════════════════════════════════════════════════════════════════════════

// .................................................... Toggle Layer Pinkie Keys

#define TYPE_LOWER(r) type(record, LOWER, PINKIE(r)); break
#define TYPE_UPPER(r) type(record, UPPER, PINKIE(r)); break

#ifndef ROLLOVER
  case PINKY3:
#endif
  case KEY3:    TYPE_LOWER(3);
  case KEY2:    TYPE_LOWER(2);
#ifndef ROLLOVER
  case PINKY1:
#endif
  case KEY1:    TYPE_LOWER(1);
  case SHIFT3:  TYPE_UPPER(3);
  case SHIFT2:  TYPE_UPPER(2);
  case SHIFT1:  TYPE_UPPER(1);

// ............................................................... Toggle Layers

static uint8_t dual_down = 0;  // dual keys down (2 -> 1 -> 0) reset on last up stroke, see case TGL_TL, TGL_TR

#define DEFAULTS brktype = 0;                \
                 hexcase = HEXADECIMAL_CASE; \
                 postfix = KC_SPC;           \
                 smart   = 1;                \
                 stagger = PINKIE_STAGGER

#define RESET(s) if (raise_layer(record, 0, s, INVERT)) { dual_down = 2; return false; }                                \
                 if (dual_down)                         { dual_down--; base_layer(dual_down); DEFAULTS; return false; } \
                 tt_escape(record, keycode); break

  case TGL_TL:  RESET(LEFT);
  case TGL_TR:  RESET(RIGHT);
  case TGL_HL:
  case TGL_HR:
  case TGL_BL:
  case TGL_BR:  tt_escape(record, keycode); break;

// .................................................................. Steno Keys

#define BASE(s) if (raise_layer(record, 0, s, INVERT)) { base_layer(0); }; return false

#ifdef STENO_ENABLE
  case PLOVER:  steno(record); return false;
  case BASE1:   BASE(LEFT);
  case BASE2:   BASE(RIGHT);
#endif

// Special Keys
// ═════════════════════════════════════════════════════════════════════════════

// .................................................................. Other Keys

#define CYCLE(n)  if (KEY_DOWN) { n = (n == 0) ? 1 : ((n == 1) ? 2 : 0); }; break
#define TOGGLE(b) if (KEY_DOWN) { b = !b; }; break

  case BRKTYPE:  CYCLE(brktype);  // see BRACKET()
  case HEXCASE:  TOGGLE(hexcase);
  case SMART:    TOGGLE(smart);
  case STAGGER:  CYCLE(stagger);  // see PINKIE()
  }
  
  CLR_1SHOT;                     // see leader_cap()
  return true;
}

// Initialization
// ═════════════════════════════════════════════════════════════════════════════

#include "initialize.c"
