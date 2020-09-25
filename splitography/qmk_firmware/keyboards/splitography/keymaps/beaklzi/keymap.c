// This is the canonical layout file for the Quantum project. If you want to add another keyboard,
// this is the style you want to emulate.
//
// To flash splitography firmware
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
//
//                === N O T E ===
//
// sudo CPATH=<keymap.c directory>/common make ...
 
// Hardware
// ═════════════════════════════════════════════════════════════════════════════

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

// Keymaps
// ═════════════════════════════════════════════════════════════════════════════

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
 ,_END_LAYERS
};

enum keyboard_keycodes {
  BASE = SAFE_RANGE
 ,BASE1
 ,BASE2
 ,PLOVER
#ifdef ROLLOVER
 ,HOME_Q  // pseudo GUI_T(KC_Q)
 ,HOME_H  // pseudo CTL_T(KC_H)
 ,HOME_E  // pseudo ALT_T(KC_E)
 ,HOME_A  // pseudo SFT_T(KC_A)
 ,HOME_T  // pseudo SFT_T(KC_T)
 ,HOME_R  // pseudo ALT_T(KC_R)
 ,HOME_S  // pseudo CTL_T(KC_S)
 ,HOME_W  // pseudo GUI_T(KC_W)
#endif
#ifdef HASKELL
 ,HS_GT   // pseudo SFT_T(S(KC_DOT))
 ,HS_LT   // pseudo CTL_T(S(KC_COMM))
#endif
 ,AST_G   // pseudo MT   (MOD_LALT | MOD_LSFT, S(KC_G))
#ifdef UPPER_HEX
 ,ACT_E   // pseudo MT   (MOD_LALT | MOD_LCTL, S(KC_E))
 ,AT_B    // pseudo ALT_T(S(KC_B))
 ,CT_C    // pseudo CTL_T(S(KC_C))
 ,ST_A    // pseudo SFT_T(S(KC_A))
#endif
 ,LT_SPC  // pseudo LT(_SYMGUI, KC_SPC)
 ,ML_BSLS
 ,ST_T    // pseudo SFT_T(S(KC_T))
 ,TT_I    // pseudo LT(_REGEX, S(KC_I))
 ,TT_SPC  // pseudo LT(_SYMGUI, KC_SPC)
};

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
#ifndef UPPER_HEX
#define ACT_E   MT   (MOD_LALT | MOD_LCTL, KC_E)
#define AT_B    ALT_T(KC_B)
#define CT_C    CTL_T(KC_C)
#define ST_A    SFT_T(KC_A)
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
#define HS_GT   TD_GT
#define HS_LT   TD_LT
#else
#define HS_GT   KC_GT
#define HS_LT   KC_LT
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
#define LT_ESC  LT  (_NUMBER, KC_ESC)
#define OS_ALT  OSM (MOD_LALT)
#define OS_CTL  OSM (MOD_LCTL)
#define OS_GUI  OSM (MOD_LGUI)
#define OS_SFT  OSM (MOD_LSFT)

#define TGL_TL  TT  (_TTFNCKEY)
#define TGL_HL  TT  (_TTCAPS)  // pseudo capslock to avoid TT key_timer conflicts
#define TGL_BL  TT  (_TTMOUSE)
#define TGL_TR  TT  (_TTREGEX)
#define TGL_HR  TT  (_TTNUMBER)
#define TGL_BR  TT  (_TTCURSOR)
#define TT_ESC  MO  (_NUMBER)

// Layers
// ═════════════════════════════════════════════════════════════════════════════

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

// .............................................................. Mouse / Chords

#include "mouse_chord_layout.h"

};

// ...................................................................... Sounds

#include "sounds.h"

// User Keycode Trap
// ═════════════════════════════════════════════════════════════════════════════

#include "keycode_functions.c"
#include "tapdance.c"

static uint8_t dual_down = 0;  // dual keys down (2 -> 1 -> 0) reset on last up stroke, see TGL_TL, TGL_TR
#ifdef UNIX
static uint16_t td_timer = 0;  // pseudo tapdance timer

#define TAPDANCE if (KEY_DOWN) { td_timer = timer_elapsed(td_timer) < TAPPING_TERM ? 0 : timer_read(); }
#endif

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
    leadercap = KEY_DOWN ? 1 : 0;  // space/enter + shift shortcut, see leader_cap()
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
    leadercap = KEY_DOWN ? 1 : 0;  // space/enter + shift shortcut, see leader_cap()
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
    if (map_shift  (record, KC_LSFT, SHIFT, KC_TAB))   { return false; }
    if (map_shift  (record, KC_RSFT, NOSHIFT, KC_TAB)) { return false; }
    if (key_press  (record))                           { base_layer(0); return false; }  // exit TT layer
    break;
  case LT_ESC:
    if (raise_layer(record, _FNCKEY, LEFT, ONDOWN))    { return false; }
    if (map_shift  (record, KC_LSFT, SHIFT, KC_TAB))   { return false; }
    if (map_shift  (record, KC_RSFT, NOSHIFT, KC_TAB)) { return false; }
    if (tt_keycode)                                    { base_layer(0); return false; }
    break;

  case LT_I:
    if (raise_layer(record, _FNCKEY, RIGHT, ONDOWN))   { return false; }
#ifdef LEFT_SPACE
    if (map_shift  (record, KC_LSFT, NOSHIFT, KC_SPC)) { return false; }
#endif
#ifdef ROLLOVER
    if (mod_roll(record, LEFT, NOSHIFT, 0, KC_I, 4)) { return false; }  // MO(_REGEX) -> LT(_REGEX, KC_I)
#endif
    rolling_layer  (record, LEFT, 0, 0, _REGEX, _SYMGUI);
    break;
  case TT_I:
    lt             (record, _REGEX, SHIFT, KC_I);
    break;
  case S(KC_I):
#ifdef LEFT_SPC_ENT
    if (map_shift(record, KC_LSFT, NOSHIFT, KC_SPC)) { return false; }
#endif
    if (!KEY_DOWN)                                   { CLR_1SHOT; }     // see leader_cap()
    break;

  case TD_EQL:
    if (tt_keycode) { break; }  // no thumb mouse layer on toggle layer
    rolling_layer  (record, LEFT, 0, 0, _MOUSE, _SYMGUI);
    break;

  // .......................................................... Right Thumb Keys

  case ML_BSLS:
    rolling_layer  (record, RIGHT, NOSHIFT, KC_BSLS, _MOUSE, _REGEX);
    break;

  case LT_SPC:
#ifdef THUMB_CAPS
    if (raise_layer(record, _TTCAPS, LEFT, TOGGLE))      { return false; }
#endif
    if (mapc_shift (record, KC_LSFT, NOSHIFT, KC_ENT))   { layer_off(_SYMGUI); return false; }  // rolling cursor to enter
    if (map_shift  (record, KC_RSFT, NOSHIFT, KC_ENT))   { return false; }
#ifdef ROLLOVER
    leaderlayer = _SYMGUI;                                                  // see mod_roll()
    if (mod_roll(record, RIGHT, NOSHIFT, 0, KC_SPC, 11)) { return false; }  // KC_SPC -> space shift
#else
    if (leader_cap (record, _SYMGUI, KC_SPC))            { return false; }  // KC_SPC -> space shift
#endif
    rolling_layer  (record, RIGHT, 0, 0, _SYMGUI, _REGEX);
    break;
  case TT_SPC:
#ifdef THUMB_CAPS
    if (raise_layer(record, _TTCAPS, LEFT, TOGGLE))      { return false; }
#endif
    if (mapc_shift (record, KC_LSFT, NOSHIFT, KC_ENT))   { layer_off(_SYMGUI); return false; }  // rolling cursor to enter
    if (map_shift  (record, KC_RSFT, NOSHIFT, KC_ENT))   { return false; }
    lt             (record, _SYMGUI, NOSHIFT, KC_SPC);  // because LT() issues <spc> before <enter> on mapc_shift()
    break;
  case KC_SPC:
    if (!KEY_DOWN)                                       { CLR_1SHOT; }  // see leader_cap()
    break;

  case LT_BSPC:
    if (!KEY_DOWN)                                       { CLR_1SHOT; }  // see leader_cap()
#ifdef THUMB_CAPS
    if (raise_layer(record, _TTCAPS, RIGHT, TOGGLE))     { return false; }
#endif
    if (map_shift  (record, KC_LSFT, NOSHIFT, KC_DEL))   { layer_off(_SYMGUI); return false; }  // rolling cursor to del
    if (map_shift  (record, KC_RSFT, NOSHIFT, KC_DEL))   { return false; }
    if (leader_cap (record, _EDIT, KC_ENT))              { return false; }  // see KC_BSPC for multi-tap
    break;
  case KC_BSPC:
    if (!KEY_DOWN)                                       { CLR_1SHOT; }     // see leader_cap()
#ifdef THUMB_CAPS
    if (raise_layer(record, _TTCAPS, RIGHT, TOGGLE))     { return false; }
#endif
    if (map_shift  (record, KC_LSFT, NOSHIFT, KC_DEL))   { layer_off(_SYMGUI); return false; }  // rolling cursor to del
    if (map_shift  (record, KC_RSFT, NOSHIFT, KC_DEL))   { return false; }
    if (leader_cap (record, 0, KC_ENT))                  { return false; }  // KC_BSPC from LT_BSPC -> (enter)* enter shift
#ifdef THUMB_CAPS
    if (KEY_DOWN)                                        { key_timer = timer_read(); }
    else if (timer_elapsed(key_timer) < TAPPING_TERM)    { tap_key(KC_BSPC); }
    return false;  // capslock toggling trap, use shift bspc -> del for auto repeat
#else
    break;
#endif

  // ............................................................. Modifier Keys

  case AST_G:
    mt_shift(record, KC_LALT, KC_LSFT, KC_G); break;
#ifdef UPPER_HEX
  case ACT_E:
    mt_shift(record, KC_LALT, KC_LCTL, KC_E); break;
  case AT_B:
    mt_shift(record, KC_LALT, 0, KC_B);       break;
  case CT_C:
    mt_shift(record, KC_LCTL, 0, KC_C);       break;
  case ST_A:
    mt_shift(record, KC_LSFT, 0, KC_A);       break;
#endif
  case ST_T:
    mt_shift(record, KC_RSFT, 0, KC_T);       break;
#ifdef HASKELL
  case HS_GT:
    mt_shift(record, KC_LSFT, 0, KC_DOT);     break;
  case HS_LT:
    mt_shift(record, KC_LCTL, 0, KC_COMM);    break;
#endif

  // ......................................................... Shift Mapped Keys

#ifdef ROLLOVER
  case KC_COLN:
    leadercap = KEY_DOWN ? 1 : 0;  // semi/colon + space/enter + shift shortcut, see leader_cap()
    if (map_leader(record, LEFT, KC_RSFT, NOSHIFT, KC_COLN, 4)) { return false; }
    break;
  case TD_COLN:
    if (mod_down(KC_RSFT))                                      { unregister_code(KC_RSFT); }  // *must* un-shift before tap dance processing to register unshifted keycodes
    leadercap = KEY_DOWN ? 1 : 0;  // semi/colon + space/enter + shift shortcut, see leader_cap()
    set_leader(record, LEFT, KC_RSFT, NOSHIFT, KC_COLN, 4);
    break;

  case KC_COMM:
    leadercap = KEY_DOWN ? 1 : 0;  // comma + space/enter + shift shortcut, see leader_cap()
    if (map_leader(record, LEFT, KC_RSFT, NOSHIFT, KC_GRV, 4))  { return false; }
    break;
  case KC_DOT:
    leadercap = KEY_DOWN ? 1 : 0;  // dot + space/enter + shift shortcut, see leader_cap()
#ifdef UNIX
    TAPDANCE; if (map_leader(record, LEFT, KC_RSFT, td_timer ? SHIFT : NOSHIFT, td_timer ? KC_GRV : KC_SLSH, 4)) { return false; }  // pseudo tapdance ~ -> ~/
#else
    if (map_leader(record, LEFT, KC_RSFT, SHIFT, KC_GRV, 4))    { return false; }
#endif
    break;
#else
  case KC_COLN:
    leadercap = KEY_DOWN ? 1 : 0;  // semi/colon + space/enter + shift shortcut, see leader_cap()
    if (map_shift(record, KC_RSFT, NOSHIFT, KC_COLN))           { return false; }
    break;
  case TD_COLN:
    if (mod_down(KC_RSFT))                                      { unregister_code(KC_RSFT); }  // *must* un-shift before tap dance processing to register unshifted keycodes
    leadercap = KEY_DOWN ? 1 : 0;  // semi/colon + space/enter + shift shortcut, see leader_cap()
    break;

  case KC_COMM:
    leadercap = KEY_DOWN ? 1 : 0;  // comma + space/enter + shift shortcut, see leader_cap()
    if (map_shift(record, KC_RSFT, NOSHIFT, KC_GRV))            { return false; }
    break;
  case KC_DOT:
    leadercap = KEY_DOWN ? 1 : 0;  // dot + space/enter + shift shortcut, see leader_cap()
#ifdef UNIX
    TAPDANCE; if (map_shift(record, KC_RSFT, td_timer ? SHIFT : NOSHIFT, td_timer ? KC_GRV : KC_SLSH)) { return false; }  // pseudo tapdance ~ -> ~/
#else
    if (map_shift(record, KC_RSFT, SHIFT, KC_GRV))              { return false; }
#endif
    break;
#endif

  // ..................................................... Leader Capitalization

  case KC_EXLM:
  case KC_QUES:
    leadercap = KEY_DOWN ? 1 : 0;  // exclamation/question + space/enter + shift shortcut, see leader_cap()
#ifdef ROLLOVER
    if (map_leader(record, LEFT, 0, NOSHIFT, keycode, 4))  { return false; }
#endif
    break;

  // .............................................................. Top Row Keys

#ifdef ROLLOVER
#define CASE_ROLL(s, k, c) case k: \
                           mod_roll(record, s, NOSHIFT, 0, k, c); \
                           return false

#define CASE_LKEY(k, c)    CASE_ROLL(LEFT,  k, c)
#define CASE_RKEY(k, c)    CASE_ROLL(RIGHT, k, c)

  CASE_LKEY(KC_Z,    0);
  CASE_LKEY(KC_Y,    1);
  CASE_LKEY(KC_O,    2);
  CASE_LKEY(KC_U,    3);

  CASE_RKEY(KC_G,    5);
  CASE_RKEY(KC_D,    6);
  CASE_RKEY(KC_N,    7);
  CASE_RKEY(KC_M,    8);
  CASE_RKEY(KC_X,    9);

  // ........................................................... Middle Row Keys

  CASE_RKEY(KC_C,    5);

  // ........................................................... Bottom Row Keys

  CASE_LKEY(KC_J,    0);
  CASE_LKEY(KC_MINS, 1);
  CASE_LKEY(KC_QUOT, 2);
  CASE_LKEY(KC_K,    3);

  CASE_RKEY(KC_B,    5);
  CASE_RKEY(KC_P,    6);
  CASE_RKEY(KC_L,    7);
  CASE_RKEY(KC_F,    8);
  CASE_RKEY(KC_V,    9);
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
  
  CLR_1SHOT;        // see leader_cap()
  return true;
}

// Initialization
// ═════════════════════════════════════════════════════════════════════════════

void matrix_init_user(void)
{
  clear_events();
#ifdef STENO_ENABLE
  steno_set_mode(STENO_MODE_BOLT);  // or STENO_MODE_GEMINI
#endif
}
