// This is the canonical layout file for the Quantum project. If you want to add another keyboard,
// this is the style you want to emulate.
//
// To flash planck firmware
// ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
//   Reset keyboard or press hw reset button on base (hole)
//
//   cd qmk_firmware/keyboards/planck
//   sudo make KEYMAP=sdothum dfu
//
//   sudo make clean          (good practice before flashing)
//   sudo make KEYMAP=sdothum (to compile check)
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
//   The navigation pad provides a single hand right thumb activated cluster
//   with left hand modifiers
//
//   #define PRIVATE_STRING includes private_string.h, a user defined code
//   block for the PRIV tap dance e.g. SEND_STRING("secret messape"),
//   see function private()
//
// Modifier clusters
// ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
//   The num and sym keys together access the navigation pad layer
//
//   ,-----------------------------------------------------------------------------------.
//   | Ctrl |  GUI |  Alt |  Esc | Space|  Tab | Bksp |  Ent | Left | Down |  Up  | Right|
//   `-----------------------------------------------------------------------------------'
//
// Hint
// ▔▔▔▔
//   For sculpted keycaps such as Cherry or OEM profile, reverse the Alt, Num,
//   Shift, Shift, Nav, Sym keycaps for more ergonomic thumb orientation and
//   actuation
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
//   See http://thedarnedestthing.com/planck%20constant
//   See http://thedarnedestthing.com/planck%20done

#include "config.h"
#include "planck.h"
#include "action_layer.h"
#ifdef AUDIO_ENABLE
#include "audio.h"
#endif
#include "eeconfig.h"

extern keymap_config_t keymap_config;

// appears to be 0..15 layer limit
enum planck_layers {
  _BASE = 0
 ,_SHIFT
 ,_LSHIFT
 ,_RSHIFT
 ,_PLOVER
 ,_NUMBER
 ,_NUMSYM
 ,_SYMBOL
 ,_SYMREG
 ,_MOUSE
 ,_FNCKEY
 ,_EDIT
 ,_ADJUST
};

enum planck_keycodes {
  BASE = SAFE_RANGE
 ,PLOVER
 ,PLOVEX
 ,PS_CIRC   // pseudo GUI_T(S(KC_6))            for modified key-codes, see process_record_user()
 ,PS_DLR    // pseudo SFT_T(S(KC_4))            for modified key-codes, see process_record_user()
 ,PS_PERC   // pseudo ALT_T(S(KC_5))            for modified key-codes, see process_record_user()
 ,PS_LPRN   // pseudo CTL_T(S(KC_9))            for modified key-codes, see process_record_user()
 ,PS_LEFT   // pseudo LT   (_MOUSE, S(KC_LEFT)) for modified key-codes, see process_record_user()
 ,PS_PIPE   // pseudo LT   (_MOUSE, S(KC_BSLS)) for modified key-codes, see process_record_user()
 ,PS_TAB    // pseudo LT   (_FNCKEY, S(KC_TAB)) for modified key-codes, see process_record_user()
#ifdef THUMB_0
 ,LT_EQL  = LT (_ADJUST, KC_EQL)
#else
 ,LT_0    = LT (_ADJUST, KC_0)
#endif
 ,LT_A    = LT (_NUMSYM, KC_A)
 ,LT_BSLS = LT (_ADJUST, KC_BSLS)
 ,LT_BSPC = LT (_EDIT,   KC_BSPC)
 ,LT_ESC  = LT (_NUMBER, KC_ESC)
 ,LT_LEFT = LT (_SYMBOL, KC_LEFT)           // see process_record_user() for extended handling
 ,LT_LFTX = LT (_SYMREG, KC_LEFT)
 ,LT_TAB  = LT (_FNCKEY, KC_TAB)
 ,OS_ALT  = OSM(MOD_LALT)
 ,OS_CALT = OSM(MOD_LALT | MOD_LCTL)
 ,OS_CGUI = OSM(MOD_LGUI | MOD_LCTL)
 ,OS_CSFT = OSM(MOD_LSFT | MOD_LCTL)
 ,OS_CTL  = OSM(MOD_LCTL)
 ,OS_GUI  = OSM(MOD_LGUI)
 ,OS_SALT = OSM(MOD_LALT | MOD_LSFT)
 ,OS_SFT  = OSM(MOD_LSFT)
 ,OS_SGUI = OSM(MOD_LGUI | MOD_LSFT)
};

// modifier keys
#define AT_B    ALT_T(KC_B)
#define AT_DOWN ALT_T(KC_DOWN)
#define CT_RGHT CTL_T(KC_RGHT)
#define GT_C    GUI_T(KC_C)
#define GT_UP   GUI_T(KC_UP)
#define MT_E    MT   (MOD_LCTL | MOD_LALT, KC_E)
#define MT_X    MT   (MOD_LALT | MOD_LSFT, KC_X)

#define S_DOWN  S    (KC_DOWN)
#define S_RGHT  S    (KC_RGHT)
#define S_UP    S    (KC_UP)

// tap dance keys
enum tap_dance {
  _CAPS = 0
 ,_COLN
 ,_COMM
 ,_DQOT
 ,_ENT
 ,_FBKT
 ,_GRV
 ,_GT
 ,_LBRC
 ,_LCBR
 ,_LPRN
 ,_LT
 ,_PRIV
 ,_QUOT
 ,_RBRC
 ,_RCBR
 ,_RPRN
 ,_SEND
 ,_SHEX
 ,_SPC
 ,_TILD
};

#define TD_CAPS TD(_CAPS)
#define TD_COLN TD(_COLN)
#define TD_COMM TD(_COMM)
#define TD_PERC TD(_PERC)
#define TD_DQOT TD(_DQOT)
#define TD_ENT  TD(_ENT)
#define TD_GRV  TD(_GRV)
#define TD_GT   TD(_GT)
#define TD_LBRC TD(_LBRC)
#define TD_LCBR TD(_LCBR)
#define TD_LPRN TD(_LPRN)
#define TD_LT   TD(_LT)
#define TD_PRIV TD(_PRIV)                   // compile time macro string, provided in private_string.h
#define TD_QUOT TD(_QUOT)
#define TD_RBRC TD(_RBRC)
#define TD_RCBR TD(_RCBR)
#define TD_RPRN TD(_RPRN)
#define TD_SEND TD(_SEND)                   // config.h defined macro string
#define TD_SPC  TD(_SPC)                    // see process_record_user() for extended handling of Spc
#define TD_TILD TD(_TILD)

// keycodes
#define ___x___ KC_TRNS
#define ___fn__ ___x___
#ifdef _______
#undef _______
#endif
#define _______ KC_NO
#define COPY    LCTL(KC_C)
#define CUT     LCTL(KC_X)
#define EOT     LCTL(KC_D)
#define NAK     LCTL(KC_U)
#define PASTE   LCTL(KC_V)
#define UNDO    LCTL(KC_Z)
#define TMCOPY  LALT(LCTL(KC_C))
#define TMPASTE LALT(LCTL(KC_V))

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

// ..................................................................... ColemaX
#ifdef COLEMAX
// http://www.keyboard-layout-editor.com/#/gists/9ecfcbee4c332b5141bfeab966759fde

  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   W  |   C  |   G  |   Z  | ^Alt | ^GUI |   J  |   L  |   U  |   Y  |   ;  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   A  |   R  |   S  |   T  |   B  | ↑Alt | ↑GUI |   K  |   N  |   E  |   I  |   O  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   X  |   V  |   F  |   D  |   P  | Caps |^Shift|   M  |   H  |   ,  |   .  |   "  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt |  Esc | Space|  Tab | Bksp |  Ent | Left | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'

  [_BASE] = {
    {KC_Q,    KC_W,    KC_C,    KC_G,    KC_Z,    OS_CALT, OS_CGUI,  KC_J,   KC_L,    KC_U,    KC_Y,    KC_SCLN},
    {KC_A,    KC_R,    KC_S,    KC_T,    KC_B,    OS_SALT, OS_SGUI,  KC_K,   KC_N,    KC_E,    KC_I,    KC_O   },
    {KC_X,    KC_V,    KC_F,    KC_D,    KC_P,    TD_CAPS, OS_CSFT,  KC_M,   KC_H,    KC_COMM, KC_DOT,  TD_QUOT},
    {OS_CTL,  OS_GUI,  OS_ALT,  LT_ESC,  TD_SPC,  LT_TAB,  LT_BSPC,  TD_ENT, LT_LEFT, AT_DOWN, GT_UP,   CT_RGHT},
  },

  [_SHIFT] = {
    {S(KC_Q), S(KC_W), S(KC_C), S(KC_G), S(KC_Z), OS_CALT, OS_CGUI, S(KC_J), S(KC_L), S(KC_U), S(KC_Y), KC_SCLN},
    {S(KC_A), S(KC_R), S(KC_S), S(KC_T), S(KC_B), OS_SALT, OS_SGUI, S(KC_K), S(KC_N), S(KC_E), S(KC_I), S(KC_O)},
    {S(KC_X), S(KC_V), S(KC_F), S(KC_D), S(KC_P), TD_CAPS, OS_CSFT, S(KC_M), S(KC_H), KC_COMM, KC_DOT,  TD_QUOT},
    {OS_CTL,  OS_GUI,  OS_ALT,  LT_ESC,  ___fn__, LT_TAB,  KC_DEL,  KC_MINS, PS_LEFT, S_DOWN,  S_UP,    S_RGHT },
  },

// http://www.keyboard-layout-editor.com/#/gists/8fa64e71484881b496d326f162959823

  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   W  |   C  |   G  |   Z  | ^Alt | ^GUI |   J  |   L  |   U  |   Y  |   :  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   A  |   R  |   S  |   T  |   B  | ↑Alt | ↑GUI |   K  |   N  |   E  |   I  |   O  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   X  |   V  |   F  |   D  |   P  | Caps |^Shift|   M  |   H  |   /  |   ?  |   "  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt |  Esc |  f() |  Tab |  Del |   -  | Left | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'

  [_LSHIFT] = {
    {S(KC_Q), S(KC_W), S(KC_C), S(KC_G), S(KC_Z), OS_CALT, OS_CGUI, S(KC_J), S(KC_L), S(KC_U), S(KC_Y), TD_COLN},
    {S(KC_A), S(KC_R), S(KC_S), S(KC_T), S(KC_B), OS_SALT, OS_SGUI, S(KC_K), S(KC_N), S(KC_E), S(KC_I), S(KC_O)},
    {S(KC_X), S(KC_V), S(KC_F), S(KC_D), S(KC_P), TD_CAPS, OS_CSFT, S(KC_M), S(KC_H), KC_SLSH, KC_QUES, TD_DQOT},
    {OS_CTL,  OS_GUI,  OS_ALT,  LT_ESC,  ___fn__, LT_TAB,  KC_DEL,  KC_MINS, PS_LEFT, S_DOWN,  S_UP,    S_RGHT },
  },

  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   W  |   C  |   G  |   Z  | ^Alt | ^GUI |   J  |   L  |   U  |   Y  |   :  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   A  |   R  |   S  |   T  |   B  | ↑Alt | ↑GUI |   K  |   N  |   E  |   I  |   O  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   X  |   V  |   F  |   D  |   P  | Caps |^Shift|   M  |   H  |   ~  |   `  |   "  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt | Caps |   _  | ↑Tab | Bksp |  f() | Left | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'

  [_RSHIFT] = {
    {S(KC_Q), S(KC_W), S(KC_C), S(KC_G), S(KC_Z), OS_CALT, OS_CGUI, S(KC_J), S(KC_L), S(KC_U), S(KC_Y), TD_COLN},
    {S(KC_A), S(KC_R), S(KC_S), S(KC_T), S(KC_B), OS_SALT, OS_SGUI, S(KC_K), S(KC_N), S(KC_E), S(KC_I), S(KC_O)},
    {S(KC_X), S(KC_V), S(KC_F), S(KC_D), S(KC_P), TD_CAPS, OS_CSFT, S(KC_M), S(KC_H), TD_TILD, TD_GRV,  TD_DQOT},
    {OS_CTL,  OS_GUI,  OS_ALT,  KC_CAPS, KC_UNDS, PS_TAB,  LT_BSPC, ___fn__, PS_LEFT, S_DOWN,  S_UP,    S_RGHT },
  },
#endif

// .............................................................. Colemak Mod-DH
#ifdef COLEMAK
  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   W  |   F  |   P  |   B  | ^Alt | ^GUI |   J  |   L  |   U  |   Y  |   ;  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   A  |   R  |   S  |   T  |   G  | ↑Alt | ↑GUI |   M  |   N  |   E  |   I  |   O  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   Z  |   X  |   C  |   D  |   V  | Caps |^Shift|   K  |   H  |   ,  |   .  |   "  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt |  Esc | Space|  Tab | Bksp |  Ent | Left | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'

  [_BASE] = {
    {KC_Q,    KC_W,    KC_F,    KC_P,    KC_B,    OS_CALT, OS_CGUI,  KC_J,   KC_L,    KC_U,    KC_Y,    KC_SCLN},
    {KC_A,    KC_R,    KC_S,    KC_T,    KC_G,    OS_SALT, OS_SGUI,  KC_M,   KC_N,    KC_E,    KC_I,    KC_O   },
    {KC_Z,    KC_X,    KC_C,    KC_D,    KC_V,    TD_CAPS, OS_CSFT,  KC_K,   KC_H,    KC_COMM, KC_DOT,  TD_QUOT},
    {OS_CTL,  OS_GUI,  OS_ALT,  LT_ESC,  TD_SPC,  LT_TAB,  LT_BSPC,  TD_ENT, LT_LEFT, AT_DOWN, GT_UP,   CT_RGHT},
  },

  [_SHIFT] = {
    {S(KC_Q), S(KC_W), S(KC_F), S(KC_P), S(KC_B), OS_CALT, OS_CGUI, S(KC_J), S(KC_L), S(KC_U), S(KC_Y), KC_SCLN},
    {S(KC_A), S(KC_R), S(KC_S), S(KC_T), S(KC_G), OS_SALT, OS_SGUI, S(KC_M), S(KC_N), S(KC_E), S(KC_I), S(KC_O)},
    {S(KC_Z), S(KC_X), S(KC_C), S(KC_D), S(KC_V), TD_CAPS, OS_CSFT, S(KC_K), S(KC_H), KC_COMM, KC_DOT,  TD_QUOT},
    {OS_CTL,  OS_GUI,  OS_ALT,  LT_ESC,  ___fn__, LT_TAB,  KC_DEL,  KC_MINS, PS_LEFT, S_DOWN,  S_UP,    S_RGHT },
  },

  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   W  |   F  |   P  |   B  | ^Alt | ^GUI |   J  |   L  |   U  |   Y  |   :  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   A  |   R  |   S  |   T  |   G  | ↑Alt | ↑GUI |   M  |   N  |   E  |   I  |   O  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   Z  |   X  |   C  |   D  |   V  | Caps |^Shift|   K  |   H  |   /  |   ?  |   "  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt |  Esc |  f() |  Tab |  Del |   -  | Left | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'

  [_LSHIFT] = {
    {S(KC_Q), S(KC_W), S(KC_F), S(KC_P), S(KC_B), OS_CALT, OS_CGUI, S(KC_J), S(KC_L), S(KC_U), S(KC_Y), TD_COLN},
    {S(KC_A), S(KC_R), S(KC_S), S(KC_T), S(KC_G), OS_SALT, OS_SGUI, S(KC_M), S(KC_N), S(KC_E), S(KC_I), S(KC_O)},
    {S(KC_Z), S(KC_X), S(KC_C), S(KC_D), S(KC_V), TD_CAPS, OS_CSFT, S(KC_K), S(KC_H), KC_SLSH, KC_QUES, TD_DQOT},
    {OS_CTL,  OS_GUI,  OS_ALT,  LT_ESC,  ___fn__, LT_TAB,  KC_DEL,  KC_MINS, PS_LEFT, S_DOWN,  S_UP,    S_RGHT },
  },

  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   W  |   F  |   P  |   B  | ^Alt | ^GUI |   J  |   L  |   U  |   Y  |   :  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   A  |   R  |   S  |   T  |   G  | ↑Alt | ↑GUI |   M  |   N  |   E  |   I  |   O  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   Z  |   X  |   C  |   D  |   V  | Caps |^Shift|   K  |   H  |   ~  |   `  |   "  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt | Caps |   _  | ↑Tab | Bksp |  f() | Left | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'

  [_RSHIFT] = {
    {S(KC_Q), S(KC_W), S(KC_F), S(KC_P), S(KC_B), OS_CALT, OS_CGUI, S(KC_J), S(KC_L), S(KC_U), S(KC_Y), TD_COLN},
    {S(KC_A), S(KC_R), S(KC_S), S(KC_T), S(KC_G), OS_SALT, OS_SGUI, S(KC_M), S(KC_N), S(KC_E), S(KC_I), S(KC_O)},
    {S(KC_Z), S(KC_X), S(KC_C), S(KC_D), S(KC_V), TD_CAPS, OS_CSFT, S(KC_K), S(KC_H), TD_TILD, TD_GRV,  TD_DQOT},
    {OS_CTL,  OS_GUI,  OS_ALT,  KC_CAPS, KC_UNDS, PS_TAB,  LT_BSPC, ___fn__, PS_LEFT, S_DOWN,  S_UP,    S_RGHT },
  },
#endif

// .................................................................... BEAKL EZ
#ifdef BEAKLEZ

  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   Y  |   O  |   U  |   K  | ^Alt | ^GUI |   F  |   G  |   R  |   C  |   V  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   H  |   I  |   E  |   A  |   "  | ↑Alt | ↑GUI |   L  |   S  |   T  |   N  |   W  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   ;  |   ,  |   J  |   .  |   X  | Caps |^Shift|   B  |   D  |   M  |   P  |   Z  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt |  Esc | Space|  Tab | Bksp |  Ent | Left | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'

  [_BASE] = {
    {KC_Q,    KC_Y,    KC_O,    KC_U,    KC_K,    OS_CALT, OS_CGUI,  KC_F,   KC_G,    KC_R,    KC_C,    KC_V   },
    {KC_H,    KC_I,    KC_E,    KC_A,    TD_QUOT, OS_SALT, OS_SGUI,  KC_L,   KC_S,    KC_T,    KC_N,    KC_W   },
    {KC_SCLN, KC_COMM, KC_J,    KC_DOT,  KC_X,    TD_CAPS, OS_CSFT,  KC_B,   KC_D,    KC_M,    KC_P,    KC_Z   },
    {OS_CTL,  OS_GUI,  OS_ALT,  LT_ESC,  TD_SPC,  LT_TAB,  LT_BSPC,  TD_ENT, LT_LEFT, AT_DOWN, GT_UP,   CT_RGHT},
  },

  [_SHIFT] = {
    {S(KC_Q), S(KC_Y), S(KC_O), S(KC_U), S(KC_K), OS_CALT, OS_CGUI, S(KC_F), S(KC_G), S(KC_R), S(KC_C), S(KC_V)},
    {S(KC_H), S(KC_I), S(KC_E), S(KC_A), TD_QUOT, OS_SALT, OS_SGUI, S(KC_L), S(KC_S), S(KC_T), S(KC_N), S(KC_W)},
    {KC_SCLN, KC_COMM, S(KC_J), KC_DOT,  S(KC_X), TD_CAPS, OS_CSFT, S(KC_B), S(KC_D), S(KC_M), S(KC_P), S(KC_Z)},
    {OS_CTL,  OS_GUI,  OS_ALT,  LT_ESC,  ___fn__, LT_TAB,  KC_DEL,  KC_UNDS, PS_LEFT, S_DOWN,  S_UP,    S_RGHT },
  },

// http://www.keyboard-layout-editor.com/#/gists/8bd8208cd4792a10ccbff660372750e9

  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   Y  |   O  |   U  |   K  | ^Alt | ^GUI |   F  |   G  |   R  |   C  |   V  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   H  |   I  |   E  |   A  |   "  | ↑Alt | ↑GUI |   L  |   S  |   T  |   N  |   W  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   :  |   ~  |   J  |   `  |   X  | Caps |^Shift|   B  |   D  |   M  |   P  |   Z  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt |  Esc |  f() |  Tab |  Del |   _  | Left | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'

  [_LSHIFT] = {
    {S(KC_Q), S(KC_Y), S(KC_O), S(KC_U), S(KC_K), OS_CALT, OS_CGUI, S(KC_F), S(KC_G), S(KC_R), S(KC_C), S(KC_V)},
    {S(KC_H), S(KC_I), S(KC_E), S(KC_A), TD_DQOT, OS_SALT, OS_SGUI, S(KC_L), S(KC_S), S(KC_T), S(KC_N), S(KC_W)},
    {TD_COLN, TD_TILD, S(KC_J), TD_GRV,  S(KC_X), TD_CAPS, OS_CSFT, S(KC_B), S(KC_D), S(KC_M), S(KC_P), S(KC_Z)},
    {OS_CTL,  OS_GUI,  OS_ALT,  LT_ESC,  ___fn__, LT_TAB,  KC_DEL,  KC_UNDS, PS_LEFT, S_DOWN,  S_UP,    S_RGHT },
  },

  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   Y  |   O  |   U  |   K  | ^Alt | ^GUI |   F  |   G  |   R  |   C  |   V  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   H  |   I  |   E  |   A  |   "  | ↑Alt | ↑GUI |   L  |   S  |   T  |   N  |   W  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   :  |   /  |   J  |   ?  |   X  | Caps |^Shift|   B  |   D  |   M  |   P  |   Z  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt | Caps |   -  | ↑Tab | Bksp |  f() | Left | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'

  [_RSHIFT] = {
    {S(KC_Q), S(KC_Y), S(KC_O), S(KC_U), S(KC_K), OS_CALT, OS_CGUI, S(KC_F), S(KC_G), S(KC_R), S(KC_C), S(KC_V)},
    {S(KC_H), S(KC_I), S(KC_E), S(KC_A), TD_DQOT, OS_SALT, OS_SGUI, S(KC_L), S(KC_S), S(KC_T), S(KC_N), S(KC_W)},
    {TD_COLN, KC_SLSH, S(KC_J), KC_QUES, S(KC_X), TD_CAPS, OS_CSFT, S(KC_B), S(KC_D), S(KC_M), S(KC_P), S(KC_Z)},
    {OS_CTL,  OS_GUI,  OS_ALT,  KC_CAPS, KC_MINS, PS_TAB,  LT_BSPC, ___fn__, PS_LEFT, S_DOWN,  S_UP,    S_RGHT },
  },
#endif

// ..................................................................... BEAKL 8
#ifdef BEAKL8
// http://www.keyboard-layout-editor.com/#/gists/249312bcc3b6ab02be1fe0ace57f2405

  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   Y  |   O  |   U  |   X  | ^Alt | ^GUI |   G  |   C  |   R  |   F  |   Z  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   K  |   H  |   E  |   A  |   .  | ↑Alt | ↑GUI |   D  |   S  |   T  |   N  |   B  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   J  |   ;  |   "  |   I  |   ,  | Caps |^Shift|   W  |   M  |   L  |   P  |   V  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt |  Esc | Space|  Tab | Bksp |  Ent | Left | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'

  [_BASE] = {
    {KC_Q,    KC_Y,    KC_O,    KC_U,    KC_X,    OS_CALT, OS_CGUI,  KC_G,   KC_C,    KC_R,    KC_F,    KC_Z   },
    {KC_K,    KC_H,    KC_E,    KC_A,    KC_DOT,  OS_SALT, OS_SGUI,  KC_D,   KC_S,    KC_T,    KC_N,    KC_B   },
    {KC_J,    KC_SCLN, TD_QUOT, KC_I,    KC_COMM, TD_CAPS, OS_CSFT,  KC_W,   KC_M,    KC_L,    KC_P,    KC_V   },
    {OS_CTL,  OS_GUI,  OS_ALT,  LT_ESC,  TD_SPC,  LT_TAB,  LT_BSPC,  TD_ENT, LT_LEFT, AT_DOWN, GT_UP,   CT_RGHT},
  },

  [_SHIFT] = {
    {S(KC_Q), S(KC_Y), S(KC_O), S(KC_U), S(KC_X), OS_CALT, OS_CGUI, S(KC_G), S(KC_C), S(KC_R), S(KC_F), S(KC_Z)},
    {S(KC_K), S(KC_H), S(KC_E), S(KC_A), KC_DOT,  OS_SALT, OS_SGUI, S(KC_D), S(KC_S), S(KC_T), S(KC_N), S(KC_B)},
    {S(KC_J), KC_SCLN, TD_QUOT, S(KC_I), KC_COMM, TD_CAPS, OS_CSFT, S(KC_W), S(KC_M), S(KC_L), S(KC_P), S(KC_V)},
    {OS_CTL,  OS_GUI,  OS_ALT,  LT_ESC,  ___fn__, LT_TAB,  KC_DEL,  KC_UNDS, PS_LEFT, S_DOWN,  S_UP,    S_RGHT },
  },

// http://www.keyboard-layout-editor.com/#/gists/8bd8208cd4792a10ccbff660372750e9

  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   Y  |   O  |   U  |   X  | ^Alt | ^GUI |   G  |   C  |   R  |   F  |   Z  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   K  |   H  |   E  |   A  |   ~  | ↑Alt | ↑GUI |   D  |   S  |   T  |   N  |   B  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   J  |   :  |   "  |   I  |   `  | Caps |^Shift|   W  |   M  |   L  |   P  |   V  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt |  Esc |  f() |  Tab |  Del |   _  | Left | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'

  [_LSHIFT] = {
    {S(KC_Q), S(KC_Y), S(KC_O), S(KC_U), S(KC_X), OS_CALT, OS_CGUI, S(KC_G), S(KC_C), S(KC_R), S(KC_F), S(KC_Z)},
    {S(KC_K), S(KC_H), S(KC_E), S(KC_A), TD_TILD, OS_SALT, OS_SGUI, S(KC_D), S(KC_S), S(KC_T), S(KC_N), S(KC_B)},
    {S(KC_J), TD_COLN, TD_DQOT, S(KC_I), TD_GRV,  TD_CAPS, OS_CSFT, S(KC_W), S(KC_M), S(KC_L), S(KC_P), S(KC_V)},
    {OS_CTL,  OS_GUI,  OS_ALT,  LT_ESC,  ___fn__, LT_TAB,  KC_DEL,  KC_UNDS, PS_LEFT, S_DOWN,  S_UP,    S_RGHT },
  },

  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   Y  |   O  |   U  |   X  | ^Alt | ^GUI |   G  |   C  |   R  |   F  |   Z  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   K  |   H  |   E  |   A  |   ?  | ↑Alt | ↑GUI |   D  |   S  |   T  |   N  |   B  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   J  |   :  |   "  |   I  |   /  | Caps |^Shift|   W  |   M  |   L  |   P  |   V  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt | Caps |   -  | ↑Tab | Bksp |  f() | Left | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'

  [_RSHIFT] = {
    {S(KC_Q), S(KC_Y), S(KC_O), S(KC_U), S(KC_X), OS_CALT, OS_CGUI, S(KC_G), S(KC_C), S(KC_R), S(KC_F), S(KC_Z)},
    {S(KC_K), S(KC_H), S(KC_E), S(KC_A), KC_QUES, OS_SALT, OS_SGUI, S(KC_D), S(KC_S), S(KC_T), S(KC_N), S(KC_B)},
    {S(KC_J), TD_COLN, TD_DQOT, S(KC_I), KC_SLSH, TD_CAPS, OS_CSFT, S(KC_W), S(KC_M), S(KC_L), S(KC_P), S(KC_V)},
    {OS_CTL,  OS_GUI,  OS_ALT,  KC_CAPS, KC_MINS, PS_TAB,  LT_BSPC, ___fn__, PS_LEFT, S_DOWN,  S_UP,    S_RGHT },
  },
#endif

// .................................................................... BEAKL 8P
#ifdef BEAKL8P
// http://www.keyboard-layout-editor.com/#/gists/249312bcc3b6ab02be1fe0ace57f2405

  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   Y  |   O  |   U  |   X  | ^Alt | ^GUI |   G  |   C  |   R  |   F  |   Z  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   K  |   H  |   E  |   A  |   .  | ↑Alt | ↑GUI |   D  |   S  |   T  |   N  |   B  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   J  |   ;  |   "  |   I  |   ,  | Caps |^Shift|   W  |   M  |   L  |   P  |   V  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt |  Esc | Space|  Tab | Bksp |  Ent | Left | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'

  [_BASE] = {
    {KC_Q,    KC_Y,    KC_O,    KC_U,    KC_X,    OS_CALT, OS_CGUI,  KC_G,   KC_C,    KC_R,    KC_F,    KC_Z   },
    {KC_K,    KC_H,    KC_E,    KC_A,    KC_DOT,  OS_SALT, OS_SGUI,  KC_D,   KC_S,    KC_T,    KC_N,    KC_B   },
    {KC_J,    KC_SCLN, TD_QUOT, KC_I,    KC_COMM, TD_CAPS, OS_CSFT,  KC_W,   KC_M,    KC_L,    KC_P,    KC_V   },
    {OS_CTL,  OS_GUI,  OS_ALT,  LT_ESC,  TD_SPC,  LT_TAB,  LT_BSPC,  TD_ENT, LT_LEFT, AT_DOWN, GT_UP,   CT_RGHT},
  },

  [_SHIFT] = {
    {S(KC_Q), S(KC_Y), S(KC_O), S(KC_U), S(KC_X), OS_CALT, OS_CGUI, S(KC_G), S(KC_C), S(KC_R), S(KC_F), S(KC_Z)},
    {S(KC_K), S(KC_H), S(KC_E), S(KC_A), KC_DOT,  OS_SALT, OS_SGUI, S(KC_D), S(KC_S), S(KC_T), S(KC_N), S(KC_B)},
    {S(KC_J), KC_SCLN, TD_QUOT, S(KC_I), KC_COMM, TD_CAPS, OS_CSFT, S(KC_W), S(KC_M), S(KC_L), S(KC_P), S(KC_V)},
    {OS_CTL,  OS_GUI,  OS_ALT,  LT_ESC,  ___fn__, LT_TAB,  KC_DEL,  KC_UNDS, PS_LEFT, S_DOWN,  S_UP,    S_RGHT },
  },

// http://www.keyboard-layout-editor.com/#/gists/7af107f03a4a1f06f5a4af35994e3d77

  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   {  |   *  |   }  |   X  | ^Alt | ^GUI |   G  |   C  |   R  |   F  |   Z  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   K  |   (  |   !  |   )  |   ~  | ↑Alt | ↑GUI |   D  |   S  |   T  |   N  |   B  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   J  |   :  |   "  |   I  |   `  | Caps |^Shift|   W  |   M  |   L  |   P  |   V  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt |  Esc |  f() |  Tab |  Del |   _  | Left | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'

  [_LSHIFT] = {
    {S(KC_Q), KC_LCBR, KC_ASTR, TD_RCBR, S(KC_X), OS_CALT, OS_CGUI, S(KC_G), S(KC_C), S(KC_R), S(KC_F), S(KC_Z)},
    {S(KC_K), PS_LPRN, KC_EXLM, TD_RPRN, TD_TILD, OS_SALT, OS_SGUI, S(KC_D), S(KC_S), S(KC_T), S(KC_N), S(KC_B)},
    {S(KC_J), TD_COLN, TD_DQOT, S(KC_I), TD_GRV,  TD_CAPS, OS_CSFT, S(KC_W), S(KC_M), S(KC_L), S(KC_P), S(KC_V)},
    {OS_CTL,  OS_GUI,  OS_ALT,  LT_ESC,  ___fn__, LT_TAB,  KC_DEL,  KC_UNDS, PS_LEFT, S_DOWN,  S_UP,    S_RGHT },
  },

  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   Y  |   O  |   U  |   X  | ^Alt | ^GUI |   G  |   [  |   +  |   ]  |   Z  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   K  |   H  |   E  |   A  |   ?  | ↑Alt | ↑GUI |   D  |   <  |   =  |   >  |   B  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   J  |   :  |   "  |   I  |   /  | Caps |^Shift|   W  |   M  |   L  |   P  |   V  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt | Caps |   -  | ↑Tab | Bksp |  f() | Left | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'

  [_RSHIFT] = {
    {S(KC_Q), S(KC_Y), S(KC_O), S(KC_U), S(KC_X), OS_CALT, OS_CGUI, S(KC_G), TD_LBRC, KC_PLUS, KC_RBRC, S(KC_Z)},
    {S(KC_K), S(KC_H), S(KC_E), S(KC_A), KC_QUES, OS_SALT, OS_SGUI, S(KC_D), TD_LT,   KC_EQL,  TD_GT,   S(KC_B)},
    {S(KC_J), TD_COLN, TD_DQOT, S(KC_I), KC_SLSH, TD_CAPS, OS_CSFT, S(KC_W), S(KC_M), S(KC_L), S(KC_P), S(KC_V)},
    {OS_CTL,  OS_GUI,  OS_ALT,  KC_CAPS, KC_MINS, PS_TAB,  LT_BSPC, ___fn__, PS_LEFT, S_DOWN,  S_UP,    S_RGHT },
  },
#endif

// ...................................................................... Qwerty
#ifdef QWERTY
  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   W  |   E  |   R  |   T  | ^Alt | ^GUI |   Y  |   U  |   I  |   O  |   P  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   A  |   S  |   D  |   F  |   G  | ↑Alt | ↑GUI |   H  |   J  |   K  |   L  |   ;  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   Z  |   X  |   C  |   V  |   B  | Caps |^Shift|   N  |   M  |   ,  |   .  |   "  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt |  Esc | Space|  Tab | Bksp |  Ent | Left | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'

  [_BASE] = {
    {KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    OS_CALT, OS_CGUI,  KC_Y,   KC_U,    KC_I,    KC_O,    KC_P   },
    {KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    OS_SALT, OS_SGUI,  KC_H,   KC_J,    KC_K,    KC_L,    KC_SCLN},
    {KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,    TD_CAPS, OS_CSFT,  KC_N,   KC_M,    KC_COMM, KC_DOT,  TD_QUOT},
    {OS_CTL,  OS_GUI,  OS_ALT,  LT_ESC,  TD_SPC,  LT_TAB,  LT_BSPC,  TD_ENT, LT_LEFT, AT_DOWN, GT_UP,   CT_RGHT},
  },

  [_SHIFT] = {
    {S(KC_Q), S(KC_W), S(KC_E), S(KC_R), S(KC_T), OS_CALT, OS_CGUI, S(KC_Y), S(KC_U), S(KC_I), S(KC_O), S(KC_P)},
    {S(KC_A), S(KC_S), S(KC_D), S(KC_F), S(KC_G), OS_SALT, OS_SGUI, S(KC_H), S(KC_J), S(KC_K), S(KC_L), KC_SCLN},
    {S(KC_Z), S(KC_X), S(KC_C), S(KC_V), S(KC_B), TD_CAPS, OS_CSFT, S(KC_N), S(KC_M), KC_COMM, KC_DOT,  TD_QUOT},
    {OS_CTL,  OS_GUI,  OS_ALT,  LT_ESC,  ___fn__, LT_TAB,  KC_DEL,  KC_MINS, PS_LEFT, S_DOWN,  S_UP,    S_RGHT },
  },

  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   W  |   E  |   R  |   T  | ^Alt | ^GUI |   Y  |   U  |   I  |   O  |   P  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   A  |   S  |   D  |   F  |   G  | ↑Alt | ↑GUI |   H  |   J  |   K  |   L  |   :  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   Z  |   X  |   C  |   V  |   B  | Caps |^Shift|   N  |   M  |   /  |   ?  |   "  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt |  Esc |  f() |  Tab |  Del |   -  | Left | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'

  [_LSHIFT] = {
    {S(KC_Q), S(KC_W), S(KC_E), S(KC_R), S(KC_T), OS_CALT, OS_CGUI, S(KC_Y), S(KC_U), S(KC_I), S(KC_O), S(KC_P)},
    {S(KC_A), S(KC_S), S(KC_D), S(KC_F), S(KC_G), OS_SALT, OS_SGUI, S(KC_H), S(KC_J), S(KC_K), S(KC_L), TD_COLN},
    {S(KC_Z), S(KC_X), S(KC_C), S(KC_V), S(KC_B), TD_CAPS, OS_CSFT, S(KC_N), S(KC_M), KC_SLSH, KC_QUES, TD_DQOT},
    {OS_CTL,  OS_GUI,  OS_ALT,  LT_ESC,  ___fn__, LT_TAB,  KC_DEL,  KC_MINS, PS_LEFT, S_DOWN,  S_UP,    S_RGHT },
  },

  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   W  |   E  |   R  |   T  | ^Alt | ^GUI |   Y  |   U  |   I  |   O  |   P  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   A  |   S  |   D  |   F  |   G  | ↑Alt | ↑GUI |   H  |   J  |   K  |   L  |   :  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   Z  |   X  |   C  |   V  |   B  | Caps |^Shift|   N  |   M  |   ~  |   `  |   "  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt | Caps |   _  | ↑Tab | Bksp |  f() | Left | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'

  [_RSHIFT] = {
    {S(KC_Q), S(KC_W), S(KC_E), S(KC_R), S(KC_T), OS_CALT, OS_CGUI, S(KC_Y), S(KC_U), S(KC_I), S(KC_O), S(KC_P)},
    {S(KC_A), S(KC_S), S(KC_D), S(KC_F), S(KC_G), OS_SALT, OS_SGUI, S(KC_H), S(KC_J), S(KC_K), S(KC_L), TD_COLN},
    {S(KC_Z), S(KC_X), S(KC_C), S(KC_V), S(KC_B), TD_CAPS, OS_CSFT, S(KC_N), S(KC_M), TD_TILD, TD_GRV,  TD_DQOT},
    {OS_CTL,  OS_GUI,  OS_ALT,  KC_CAPS, KC_UNDS, PS_TAB,  LT_BSPC, ___fn__, PS_LEFT, S_DOWN,  S_UP,    S_RGHT },
  },
#endif

// ...................................................................... Plover
//
// http://www.keyboard-layout-editor.com/#/gists/7296e3f601a6bb2eee2aa8f034c58a27

  // ,-----------------------------------------------------------------------------------.
  // |   #  |   #  |   #  |   #  |   #  |      |   #  |   #  |   #  |   #  |   #  |   #  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   S  |   T  |   P  |   H  |   *  |      |   *  |   F  |   P  |   L  |   T  |   D  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   S  |   K  |   W  |   R  |   *  |      |   *  |   R  |   B  |   G  |   S  |   Z  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // |      |      |   A  |   O  |      | Exit |      |   E  |   U  |      |      |      |
  // `-----------------------------------------------------------------------------------'

  [_PLOVER] = {
    {KC_1,    KC_1,    KC_1,    KC_1,    KC_1,    _______, KC_1,    KC_1,    KC_1,    KC_1,    KC_1,    KC_1   },
    {KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    _______, KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC},
    {KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    _______, KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, KC_QUOT},
    {_______, _______, KC_C,    KC_V,    _______, PLOVEX,  _______, KC_N,    KC_M,    _______, _______, _______},
  },

// ......................................................... Number Keypad Layer
//
// http://www.keyboard-layout-editor.com/#/gists/369b861dcae7d219a11ea9b8fbfe88f1

  // .-----------------------------------------------------------------------------------.
  // |      |   F  |   E  |   D  |      |      |      |   /  |   7  |   8  |   9  |   *  |
  // |-----------------------------------------------------------------------------------|
  // | Ctrl |   C  |   B  |   A  |      |      |      |   .  |   4  |   5  |   6  |   -  |
  // |-----------------------------------------------------------------------------------|
  // |      |   #  |   X  |   G  |      |      |      |   ,  |   1  |   2  |   3  |   +  |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |  f() |      |       |      |  0  |   =  |      |      |      |
  // |      |      |      |  f() |      |       |      |  =  |   0  |      |      |      |
  // '-----------------------------------------------------------------------------------'

  [_NUMBER] = {
    {_______, KC_F,    MT_E,    KC_D,    _______, _______, _______, KC_SLSH, KC_7,    KC_8,    KC_9,    KC_ASTR},
    {OS_CTL,  GT_C,    AT_B,    LT_A,    _______, _______, _______, KC_DOT,  KC_4,    KC_5,    KC_6,    KC_MINS},
    {_______, KC_HASH, MT_X,    S(KC_G), _______, _______, _______, TD_COMM, KC_1,    KC_2,    KC_3,    KC_PLUS},
#ifdef THUMB_0
    {___x___, ___x___, ___x___, ___fn__, ___x___, ___x___, ___x___, KC_0,    LT_EQL,  ___x___, ___x___, ___x___},
#else
    {___x___, ___x___, ___x___, ___fn__, ___x___, ___x___, ___x___, KC_EQL,  LT_0,    ___x___, ___x___, ___x___},
#endif
  },

  // .-----------------------------------------------------------------------------------.
  // |      |      |      |      |      |      |      |   {  |   &  |   ?  |   :  |   }  |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |  f() |      |      |      |   (  |   $  |   %  |   ^  |   )  |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |      |      |      |   [  |   <  |   ~  |   >  |   ]  |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |  f() |      |      |      |   |  |   \  |      |      |      |
  // '-----------------------------------------------------------------------------------'

  [_NUMSYM] = {
    {_______, _______, _______, ___x___, _______, _______, _______, TD_LCBR, KC_AMPR, KC_QUES, KC_COLN, KC_RCBR},
    {___x___, ___x___, ___x___, ___fn__, _______, _______, _______, TD_LPRN, KC_DLR,  KC_PERC, KC_CIRC, KC_RPRN},
    {_______, _______, _______, ___x___, _______, _______, _______, TD_LBRC, KC_LT,   KC_TILD, KC_GT,   KC_RBRC},
    {___x___, ___x___, ___x___, ___fn__, ___x___, ___x___, ___x___, KC_PIPE, KC_BSLS, ___x___, ___x___, ___x___},
  },

// ..................................................... Symbol Navigation Layer
//
// http://www.keyboard-layout-editor.com/#/gists/df5aa22deeca8a811e0c7a86a0269d8f

  // .-----------------------------------------------------------------------------------.
  // |   {  |   .  |   *  |   &  |   }  |      |      |      | Home |  Up  |  End | PgUp |
  // |-----------------------------------------------------------------------------------|
  // |   (  |   ^  |   %  |   $  |   )  |      |      |      | Left | Down | Right| PgDn |
  // |-----------------------------------------------------------------------------------|
  // |   [  |   #  |   @  |   !  |   ]  |      |      |      |      |      |      |      |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |   \  |   |  |      |      |      |  f() |      |      |      |
  // '-----------------------------------------------------------------------------------'

  [_SYMBOL] = {
    {KC_LCBR, KC_DOT,  KC_ASTR, KC_AMPR, TD_RCBR, _______, _______, _______, KC_HOME, KC_UP,   KC_END,  KC_PGUP},
    {PS_LPRN, PS_CIRC, PS_PERC, PS_DLR,  TD_RPRN, _______, _______, _______, LT_LFTX, KC_DOWN, KC_RGHT, KC_PGDN},
    {KC_LBRC, KC_HASH, KC_AT,   KC_EXLM, TD_RBRC, _______, _______, _______, _______, _______, _______, _______},
    {___x___, ___x___, ___x___, LT_BSLS, PS_PIPE, ___x___, ___x___, ___fn__, ___x___, ___x___, ___x___, ___x___},
  },

  // .-----------------------------------------------------------------------------------.
  // |      |   ?  |   +  |   ~  |      |      |      |      |      |      |      |      |
  // |-----------------------------------------------------------------------------------|
  // |      |   3  |   2  |   1  |      |      |      |      |  f() |      |      |      |
  // |-----------------------------------------------------------------------------------|
  // |      |   <  |   =  |   >  |      |      |      |      |      |      |      |      |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |      |      |      |      |  f() |      |      |      |
  // '-----------------------------------------------------------------------------------'

  [_SYMREG] = {
    {___x___, KC_QUES, KC_PLUS, KC_TILD, ___x___, _______, _______, _______, ___x___, ___x___, ___x___, ___x___},
    {___x___, KC_3,    KC_2,    KC_1,    ___x___, _______, _______, _______, ___fn__, ___x___, ___x___, ___x___},
    {___x___, KC_LT,   KC_EQL,  KC_GT,   ___x___, _______, _______, _______, _______, _______, _______, _______},
    {___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___fn__, ___x___, ___x___, ___x___},
  },

// ............................................................... Mouse Actions
//
// http://www.keyboard-layout-editor.com/#/gists/863abbf3ae716a267b60337a953588d5

  // .-----------------------------------------------------------------------------------.
  // |      |      |      |      |      |      |      |      | Left |  Up  | Right|  Up  |
  // |-----------------------------------------------------------------------------------|
  // |      | Btn3 | Btn2 | Btn1 |      |      |      |      | Left | Down | Right| Down |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |      |      |      |      |      |      |      |      |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |  f() |      |      |      |  f() |      |      |      |
  // '-----------------------------------------------------------------------------------'

  [_MOUSE] = {
    {_______, _______, _______, _______, _______, _______, _______, _______, KC_WH_L, KC_MS_U, KC_WH_R, KC_WH_U},
    {_______, KC_BTN3, KC_BTN2, KC_BTN1, _______, _______, _______, _______, KC_MS_L, KC_MS_D, KC_MS_R, KC_WH_D},
    {_______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______},
    {_______, _______, _______, _______, ___fn__, _______, _______, _______, ___fn__, _______, _______, _______},
  },

// ............ .................................................. Function Keys
//
// http://www.keyboard-layout-editor.com/#/gists/dc580fd2504d1328913f8044a9861378

  // .-----------------------------------------------------------------------------------.
  // |      |      |      |      |      |      |      |      |  F7  |  F8  |  F9  |  F12 |
  // |-----------------------------------------------------------------------------------|
  // | Ctrl |  GUI |  Alt | Shift|      |      |      |      |  F4  |  F5  |  F6  |  F11 |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |      |      |      |      |  F1  |  F2  |  F3  |  F10 |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |      |  f() |      |   +  |      |      |      |      |
  // '-----------------------------------------------------------------------------------'

  [_FNCKEY] = {
    {_______, _______, _______, _______, _______, _______, _______, _______, KC_F7,   KC_F8,   KC_F9,   KC_F12 },
    {OS_CTL,  OS_GUI,  OS_ALT,  OS_SFT,  _______, _______, _______, _______, KC_F4,   KC_F5,   KC_F6,   KC_F11 },
    {_______, _______, _______, _______, _______, _______, _______, _______, KC_F1,   KC_F2,   KC_F3,   KC_F10 },
    {_______, _______, _______, _______, _______, ___fn__, _______, KC_PLUS, _______, _______, _______, _______},
  },

// .................................................................. Short Cuts
//
// http://www.keyboard-layout-editor.com/#/gists/aeb9c497167a17c3bc8efb2f1d88be48

  // .-----------------------------------------------------------------------------------.
  // |      |      | Copy | Paste|      |      |      |      |      |      |      |      |
  // |--------------------------------------------------------------+------+------+------|
  // | Undo |  Cut | Copy | Paste|      |      |      |      | PRIV |  PUB |      |      |
  // |-----------------------------------------------------------------------------------|
  // |      |      |  Nak |  Eot |      |      |      |      |      |      |      |      |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |      |      |  f() |      |      |      |      |      |
  // '-----------------------------------------------------------------------------------'

  [_EDIT] = {
    {_______, _______, TMCOPY,  TMPASTE, _______, _______, _______, _______, _______, _______, _______, _______},
    {UNDO,    CUT,     COPY,    PASTE,   _______, _______, _______, _______, TD_PRIV, TD_SEND, _______, _______},
    {_______, _______, NAK,     EOT,     _______, _______, _______, _______, _______, _______, _______, _______},
    {_______, _______, _______, _______, _______, _______, ___fn__, _______, _______, _______, _______, _______},
  },

// ................................................................ Adjust Layer
//
// http://www.keyboard-layout-editor.com/#/gists/08f08c811c7dc502617f0758bce59bf6

  // ,-----------------------------------------------------------------------------------.
  // |Plover|      |      |      |      |      |      |      |      |      |      |      |_
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |Aud on|      |      |      |      |      |      |      |      |      |      |      |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // | Reset|      |      |      |      |      |      |      |      |      |      |      |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // |      |      |      |  f() |      |      |      |      |  f() |      |      |      |
  // `-----------------------------------------------------------------------------------'

  [_ADJUST] = {
    {PLOVER,  _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______},
    {AU_ON,   _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______},
    {RESET,   _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______},
    {_______, _______, _______, ___fn__, _______, _______, _______, _______, ___fn__, _______, _______, _______},
  },
};

#ifdef AUDIO_ENABLE
float song_startup  [][2] = SONG(STARTUP_SOUND);
float song_colemak  [][2] = SONG(COLEMAK_SOUND);
float song_qwerty   [][2] = SONG(QWERTY_SOUND);
float song_plover   [][2] = SONG(PLOVER_SOUND);
float song_plover_gb[][2] = SONG(PLOVER_GOODBYE_SOUND);
float song_caps_on  [][2] = SONG(CAPS_LOCK_ON_SOUND);
float song_caps_off [][2] = SONG(CAPS_LOCK_OFF_SOUND);
float music_scale   [][2] = SONG(MUSIC_SCALE_SOUND);
float song_goodbye  [][2] = SONG(GOODBYE_SOUND);
#endif

// .......................................................... Keycode Primitives

// register simple key press
void tap_key(uint16_t keycode)
{
  register_code  (keycode);
  unregister_code(keycode);
}

void shift_key(uint16_t keycode)
{
  register_code  (KC_LSFT);
  tap_key        (keycode);
  unregister_code(KC_LSFT);
}

#define         SHIFT   1
#define         NOSHIFT 0

static uint16_t key_timer = 0;

// key press for com_layer() and lt_shift() macros
bool key_press(uint16_t keycode, uint8_t shift)
{
  if (keycode) {
    if (timer_elapsed(key_timer) < TAPPING_TERM) {
      if (shift) {
        shift_key(keycode);
      }
      else {
        tap_key(keycode);
      }
      return true;
    }
  }
  return false;
}

// ALT_T, CTL_T, GUI_T, SFT_T for shifted keycodes
void mt_shift(keyrecord_t *record, uint16_t modifier, uint16_t keycode)
{
  if (record->event.pressed) {
    register_code (modifier);
    key_timer = timer_read();
  }
  else {
    unregister_code (modifier);
    if (timer_elapsed(key_timer) < TAPPING_TERM) {
      shift_key(keycode);
    }
    key_timer = 0;
  }
}

// ................................................................... Mod Masks

// tap dance persistant mods, see process_record_user()
// keyboard_report->mods (?) appears to be cleared by tap dance
static uint8_t mods = 0;

void tap_mods(keyrecord_t *record, uint16_t keycode)
{
  if (record->event.pressed) {
    mods |= MOD_BIT(keycode);
  }
  else {
    mods &= ~(MOD_BIT(keycode));
  }
}

// (un)register modifiers
void modifier(void (*f)(uint8_t))
{
  if (mods & MOD_BIT(KC_LCTL)) {
    (*f)(KC_LCTL);
  }
  if (mods & MOD_BIT(KC_LGUI)) {
    (*f)(KC_LGUI);
  }
  if (mods & MOD_BIT(KC_LALT)) {
    (*f)(KC_LALT);
  }
}

// ......................................................... Triple Dance Insert

void tilde(qk_tap_dance_state_t *state, void *user_data)
{
  // double tap plus down: repeating keycode
  if (state->count > 2) {
    register_code(KC_LSFT);
    register_code(KC_GRV);
  }
  // tap: keycode
  else {
    shift_key(KC_GRV);
    // double tap: unix home directory
    if (state->count > 1) {
      tap_key(KC_SLSH);
    }
  }
}

void tilde_reset(qk_tap_dance_state_t *state, void *user_data)
{
  unregister_code(KC_GRV);
  unregister_code(KC_LSFT);
}

// .................................................... Triple Dance Shift/Layer

static uint8_t dt_shift = 0;

void double_shift(uint16_t keycode, uint8_t layer)
{
  tap_key (keycode);
  if (DT_SHIFT) {
    // set_oneshot_mods(MOD_LSFT);
    // layer_on(layer);
    layer_on(_SHIFT);
    set_oneshot_layer(_SHIFT, ONESHOT_START);
    dt_shift = 1;
  }
  else {
    layer_on(layer);
  }
}

// tap dance LT (LAYER, KEY) emulation with <KEY><DOWN> -> <KEY><SHIFT> and auto-repeat extensions!
void tap_shift(qk_tap_dance_state_t *state, uint16_t keycode, uint8_t layer)
{
  // double tap plus down
  if (state->count > 2) {
    // double enter shift
    if (keycode == KC_ENT) {
      tap_key     (keycode);
      double_shift(keycode, layer);
    }
    // repeating keycode
    else {
      register_code(keycode);
    }
  }
  // tap plus down (or double tap): keycode (one shot) shift
  else if (state->count > 1) {
    double_shift(keycode, layer);
  }
  // down: shift
  else if (state->pressed) {
    layer_on(layer);
  }
  // tap: keycode
  else {
    modifier(register_code);
    tap_key (keycode);
    modifier(unregister_code);
  }
}

void tap_reset(uint16_t keycode, uint8_t layer)
{
  unregister_code(keycode);
  if (DT_SHIFT && dt_shift) {
    clear_oneshot_layer_state(ONESHOT_PRESSED);
    dt_shift = 0;
  }
  else {
    layer_off(layer);
  }
}

// augment pseudo LT (_RSHIFT, KC_ENT) handling below for rapid <ENTER><SHIFT> sequences
void enter(qk_tap_dance_state_t *state, void *user_data)
{
  tap_shift(state, KC_ENT, _RSHIFT);
}

void enter_reset(qk_tap_dance_state_t *state, void *user_data)
{
  tap_reset(KC_ENT, _RSHIFT);
}

// augment pseudo LT (_LSHIFT, KC_SPC) handling below for rapid <SPACE><SHIFT> sequences
void space(qk_tap_dance_state_t *state, void *user_data)
{
  tap_shift(state, KC_SPC, _LSHIFT);
}

void space_reset(qk_tap_dance_state_t *state, void *user_data)
{
  tap_reset(KC_SPC, _LSHIFT);
}

// ............................................................. Tap Dance Pairs

// tap dance shift rules
#define S_NEVER  0
#define S_SINGLE 1
#define S_DOUBLE 2
#define S_ALWAYS S_SINGLE | S_DOUBLE

void symbol_pair(uint8_t shift, uint16_t left, uint16_t right)
{
  if (shift & S_DOUBLE) {
    shift_key(left);
    shift_key(right);
  }
  else {
    tap_key(left);
    tap_key(right);
  }
}

#define CLOSE 1

// tap dance symbol pairs
void tap_pair(qk_tap_dance_state_t *state, uint8_t shift, uint16_t left, uint16_t right, uint8_t modifier, uint8_t close)
{
  // triple tap: left right with cursor between symbol pair a la vim :-)
  if (state->count > 2) {
    symbol_pair(shift, left, right);
    tap_key(KC_LEFT);
  }
  // double tap: left right
  else if (state->count > 1) {
    symbol_pair(shift, left, right);
  }
  // down: modifier
  else if (state->pressed) {
    if (modifier) {
      register_code(modifier);
    }
  }
  // tap: left (close: right)
  else {
    if (shift & S_SINGLE) {
      shift_key(close ? right : left);
    }
    else {
      tap_key(close ? right : left);
    }
  }
  if (!modifier) {
    reset_tap_dance(state);
  }
}

void doublequote(qk_tap_dance_state_t *state, void *user_data)
{
  tap_pair(state, S_ALWAYS, KC_QUOT, KC_QUOT, 0, 0);
}

void grave(qk_tap_dance_state_t *state, void *user_data)
{
  tap_pair(state, S_NEVER, KC_GRV, KC_GRV, 0, 0);
}

void lbrace(qk_tap_dance_state_t *state, void *user_data)
{
  tap_pair(state, S_NEVER, KC_LBRC, KC_RBRC, 0, 0);
}

void lcurly(qk_tap_dance_state_t *state, void *user_data)
{
  tap_pair(state, S_ALWAYS, KC_LBRC, KC_RBRC, 0, 0);
}

void lparen(qk_tap_dance_state_t *state, void *user_data)
{
  tap_pair(state, S_ALWAYS, KC_9, KC_0, KC_LCTL, 0);
}

void lparen_reset(qk_tap_dance_state_t *state, void *user_data)
{
  unregister_code(KC_LCTL);
}

void quote(qk_tap_dance_state_t *state, void *user_data)
{
  tap_pair(state, S_NEVER, KC_QUOT, KC_QUOT, 0, 0);
}

void rbrace(qk_tap_dance_state_t *state, void *user_data)
{
  tap_pair(state, S_NEVER, KC_LBRC, KC_RBRC, 0, CLOSE);
}

void rcurly(qk_tap_dance_state_t *state, void *user_data)
{
  tap_pair(state, S_ALWAYS, KC_LBRC, KC_RBRC, 0, CLOSE);
}

void rparen(qk_tap_dance_state_t *state, void *user_data)
{
  tap_pair(state, S_ALWAYS, KC_9, KC_0, KC_LCTL, CLOSE);
}

void rparen_reset(qk_tap_dance_state_t *state, void *user_data)
{
  unregister_code(KC_LCTL);
}

// ............................................................ Tap Dance Insert

void colon(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1) {
    tap_key(KC_SPC);
    shift_key(KC_SCLN);
    shift_key(KC_SCLN);
    tap_key(KC_SPC);
  }
  else {
    shift_key(KC_SCLN);
  }
  reset_tap_dance(state);
}

void comma(qk_tap_dance_state_t *state, void *user_data)
{
  tap_key(KC_COMM);
  if (state->count > 1) {
    tap_key(KC_SPC);
  }
  reset_tap_dance(state);
}

void greater(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1) {
    tap_key(KC_SPC);
    tap_key(KC_MINS);
    shift_key(KC_DOT);
    tap_key(KC_SPC);
  }
  else {
    shift_key(KC_DOT);
  }
  reset_tap_dance(state);
}

void lesser(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1) {
    tap_key(KC_SPC);
    shift_key(KC_COMM);
    tap_key(KC_MINS);
    tap_key(KC_SPC);
  }
  else {
    shift_key(KC_COMM);
  }
  reset_tap_dance(state);
}

// compile time macro string, see functions/hardware planck script
void private(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1) {
#ifdef PRIVATE_STRING
#include "private_string.h"
#endif
  }
  reset_tap_dance(state);
}

// config.h defined string
void send(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1) {
    SEND_STRING(PUBLIC_STRING);
  }
  reset_tap_dance(state);
}

// ............................................................ Tap Dance Toggle

void caps(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1) {
    tap_key(KC_CAPS);
  }
  else {
    set_oneshot_mods(MOD_LSFT);
  }
  reset_tap_dance(state);
}

qk_tap_dance_action_t tap_dance_actions[] = {
  [_CAPS] = ACTION_TAP_DANCE_FN         (caps)
 ,[_COLN] = ACTION_TAP_DANCE_FN         (colon)
 ,[_COMM] = ACTION_TAP_DANCE_FN         (comma)
 ,[_DQOT] = ACTION_TAP_DANCE_FN         (doublequote)
 ,[_ENT]  = ACTION_TAP_DANCE_FN_ADVANCED(NULL, enter, enter_reset)
 ,[_GT]   = ACTION_TAP_DANCE_FN         (greater)
 ,[_GRV]  = ACTION_TAP_DANCE_FN         (grave)
 ,[_LBRC] = ACTION_TAP_DANCE_FN         (lbrace)
 ,[_LCBR] = ACTION_TAP_DANCE_FN         (lcurly)
 ,[_LPRN] = ACTION_TAP_DANCE_FN_ADVANCED(NULL, lparen, lparen_reset)
 ,[_LT]   = ACTION_TAP_DANCE_FN         (lesser)
 ,[_PRIV] = ACTION_TAP_DANCE_FN         (private)
 ,[_QUOT] = ACTION_TAP_DANCE_FN         (quote)
 ,[_RBRC] = ACTION_TAP_DANCE_FN         (rbrace)
 ,[_RCBR] = ACTION_TAP_DANCE_FN         (rcurly)
 ,[_RPRN] = ACTION_TAP_DANCE_FN_ADVANCED(NULL, rparen, rparen_reset)
 ,[_SEND] = ACTION_TAP_DANCE_FN         (send)
 ,[_SPC]  = ACTION_TAP_DANCE_FN_ADVANCED(NULL, space, space_reset)
 ,[_TILD] = ACTION_TAP_DANCE_FN_ADVANCED(NULL, tilde, tilde_reset)
};

// .............................................................. Dynamic Layers

#define        LEFT    1
#define        RIGHT   2
static uint8_t thumb = 0;

// LEFT (KC_SPC, S(KC_BSLS)), RIGHT (KC_LEFT, S(KC_LEFT)) opposite thumb combinations, see process_record_user()
// up,   up   -> _COLEMAK
// up,   down -> _SYMBOL
// down, up   -> _NUMBER
// down, down -> _MOUSE
#define THUMBS_DOWN _MOUSE

static uint8_t overlayer = THUMBS_DOWN;

// left right layer combinations
void com_layer(keyrecord_t *record, uint8_t side, uint16_t keycode, uint8_t shift, uint8_t layer, uint8_t default_layer)
{
  if (record->event.pressed) {
    // layer_on via tap_layer(), see process_record_user()
    key_timer = timer_read();
    thumb     = thumb | side;
  }
  else {
    layer_off(layer);
    // opposite com_layer() thumb may have switched effective layer!
    if (overlayer) {
      layer_off(overlayer);
      overlayer = THUMBS_DOWN;
    }
    if (!key_press(keycode, shift)) {
      // opposite thumb down? see left right combination layer table above
      if (thumb & (side == LEFT ? RIGHT : LEFT)) {
        layer_on(default_layer);
        overlayer = default_layer;
      }
    }
    clear_mods();
    thumb     = thumb & ~side;
    key_timer = 0;
  }
}

// LT for S(keycode)
void lt_shift(keyrecord_t *record, uint16_t keycode, uint8_t layer)
{
  if (record->event.pressed) {
    layer_on(layer);
    key_timer = timer_read();
  }
  else {
    layer_off(layer);
    // for shifted keycodes, hence, LT_SHIFT
    key_press(keycode, SHIFT);
    clear_mods();
    key_timer = 0;
  }
}

// set layer asap to overcome macro latency errors, notably tap dance and LT usage
// this routine inexplicably (?) sets layer_on() faster than can be done in com_layer()
void tap_layer(keyrecord_t *record, uint8_t layer)
{
  if (record->event.pressed) {
    layer_on(layer);
  }
  else {
    layer_off(layer);
  }
}

// ..................................................................... Keymaps

void persistant_default_layer_set(uint16_t default_layer)
{
  eeconfig_update_default_layer(default_layer);
  default_layer_set            (default_layer);
}

void clear_layers(void)
{
  uint8_t layer;
  for (layer = 0; layer <= _ADJUST; layer++) {
    layer_off(layer);
  }
}

void toggle_plover(void)
{
  // toggle window manager plover application, see herbstluftwm/config/appbinds
  register_code  (KC_LGUI);
  shift_key      (KC_RGHT);
  unregister_code(KC_LGUI);
}

void plover(keyrecord_t *record)
{
  if (record->event.pressed) {
#ifdef AUDIO_ENABLE
    PLAY_SONG(song_plover);
#endif
    clear_layers();
    layer_on(_PLOVER);
    if (!eeconfig_is_enabled()) {
      eeconfig_init();
    }
    keymap_config.raw  = eeconfig_read_keymap();
    keymap_config.nkro = 1;
    eeconfig_update_keymap(keymap_config.raw);
    toggle_plover();
  }
}

void plovex(keyrecord_t *record)
{
  if (record->event.pressed) {
#ifdef AUDIO_ENABLE
    PLAY_SONG(song_plover_gb);
#endif
    layer_off(_PLOVER);
    toggle_plover();
  }
}

// ........................................................... User Keycode Trap

bool process_record_user(uint16_t keycode, keyrecord_t *record)
{
  switch (keycode) {
    case AT_DOWN:
      tap_mods(record, KC_LALT);
      break;
    case CT_RGHT:
      tap_mods(record, KC_LCTL);
      break;
    case GT_UP:
      tap_mods(record, KC_LGUI);
      break;
    case LT_ESC:
      tap_layer(record, _NUMBER);
      break;
    case LT_LEFT:
      tap_layer(record, _SYMBOL);
      // LT (_SYMBOL, KC_LEFT) left right combination layer
      com_layer(record, RIGHT, 0, 0, _SYMBOL, _LSHIFT);
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
    case PS_CIRC:
      // GUI_T(S(KC_6))
      mt_shift(record, KC_LGUI, KC_6);
      break;
    case PS_DLR:
      // SFT_T(S(KC_4))
      mt_shift(record, KC_LSFT, KC_4);
      break;
    case PS_LPRN:
      // CTL_T(S(KC_9))
      mt_shift(record, KC_LCTL, KC_9);
      break;
    case PS_PERC:
      // ALT_T(S(KC_5))
      mt_shift(record, KC_LALT, KC_5);
      break;
    case PS_LEFT:
      tap_layer(record, _MOUSE);
      // LT (_MOUSE, S(KC_LEFT)) left right combination layer
      com_layer(record, RIGHT, KC_LEFT, SHIFT, _MOUSE, _LSHIFT);
      break;
    case PS_PIPE:
      tap_layer(record, _MOUSE);
      // LT (_MOUSE, S(KC_BSLS)) left right combination layer
      com_layer(record, LEFT, KC_BSLS, SHIFT, _MOUSE, _SYMBOL);
      break;
    case PS_TAB:
      // LT (_FNCKEY, S(KC_TAB)) emulation
      lt_shift(record, KC_TAB, _FNCKEY);
      break;
    case TD_ENT:
      tap_layer(record, _RSHIFT);
      // LT (_RSHIFT, KC_ENT) emulation, see tap dance enter
      break;
    case TD_SPC:
      tap_layer(record, _LSHIFT);
      // LT (_LSHIFT, KC_SPC) left right combination layer, see tap dance space
      com_layer(record, LEFT, 0, 0, _LSHIFT, _SYMBOL);
      break;
    case PLOVER:
      plover(record);
      return false;
    case PLOVEX:
      plovex(record);
      return false;
  }
  return true;
}

// ....................................................................... Audio

void matrix_init_user(void)
{
#ifdef AUDIO_ENABLE
  startup_user();
#endif
}

#ifdef AUDIO_ENABLE
#ifdef BACKLIGHT_ENABLE
void led_set_user(uint8_t usb_led)
{
  static uint8_t old_usb_led = 0;
  _delay_ms(10);                            // gets rid of tick
  if (!is_playing_notes()) {
    if ((usb_led & (1<<USB_LED_CAPS_LOCK)) && !(old_usb_led & (1<<USB_LED_CAPS_LOCK))) {
      // if capslock LED is turning on
      PLAY_SONG(song_caps_on);
    }
    else if (!(usb_led & (1<<USB_LED_CAPS_LOCK)) && (old_usb_led & (1<<USB_LED_CAPS_LOCK))) {
      // if capslock LED is turning off
      PLAY_SONG(song_caps_off);
    }
  }
  old_usb_led = usb_led;
}
#endif

void startup_user(void)
{
  _delay_ms(20);                            // gets rid of tick
  PLAY_SONG(song_startup);
}

void shutdown_user(void)
{
  PLAY_SONG(song_goodbye);
  _delay_ms(150);
  stop_all_notes();
}

void music_on_user(void)
{
  music_scale_user();
}

void music_scale_user(void)
{
  PLAY_SONG(music_scale);
}
#endif
