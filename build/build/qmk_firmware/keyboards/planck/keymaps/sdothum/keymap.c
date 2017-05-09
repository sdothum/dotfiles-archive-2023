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
// Modifier clusters
// ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
//   The num and sym keys together access the navigation pad layer
//
//   ,-----------------------------------------------------------------------------------.
//   | Ctrl |  GUI |  Alt |  Esc | Space|  Tab | Bksp |  Ent | Left | Down |  Up  |Right |
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
//   Proper case naming for modifier key names to avoid define conflicts (DOWN
//   and UP in particular) adds readability bonus
//
//   _One shot modifier keymap table defines
//   __Double tap modifier keymap table defines
//
// Change history
// ▔▔▔▔▔▔▔▔▔▔▔▔▔▔
//   See http://thedarnedestthing.com/planck%20constant

#include "config.h"
#include "planck.h"
#include "action_layer.h"
#ifdef AUDIO_ENABLE
#include "audio.h"
#endif
#include "eeconfig.h"

extern keymap_config_t keymap_config;

enum planck_layers {
  _COLEMAK = 0
 ,_PLOVER
 ,_LSHIFT
 ,_RSHIFT
 ,_NUMBER
 ,_SYMBOL
 ,_REGHEX
 ,_SFTNAV
 ,_FNCKEY
 ,_ADJUST
};

enum planck_keycodes {
  COLEMAK = SAFE_RANGE
 ,PLOVER
 ,PLOVEX
 ,P_LEFT    // pseudo LT(_SFTNAV, S(KC_LEFT)) for modified key-codes, see process_record_user()
 ,P_PIPE    // pseudo LT(_SFTNAV, S(KC_BSLS)) for modified key-codes, see process_record_user()
 ,P_TAB     // pseudo LT(_FNCKEY, S(KC_TAB))  for modified key-codes, see process_record_user()
 ,L_BSPC  = LT (_ADJUST, KC_BSPC)
 ,L_ENT   = LT (_RSHIFT, KC_ENT)
 ,L_ESC   = LT (_NUMBER, KC_ESC)
 ,L_LEFT  = LT (_SYMBOL, KC_LEFT)           // see process_record_user() for extended handling of Left
 ,L_TAB   = LT (_FNCKEY, KC_TAB)
 ,O_ALT   = OSM(MOD_LALT)
 ,O_CALT  = OSM(MOD_LALT | MOD_LCTL)
 ,O_CGUI  = OSM(MOD_LGUI | MOD_LCTL)
 ,O_CSFT  = OSM(MOD_LSFT | MOD_LCTL)
 ,O_CTL   = OSM(MOD_LCTL)
 ,O_GUI   = OSM(MOD_LGUI)
 ,O_SALT  = OSM(MOD_LALT | MOD_LSFT)
 ,O_SFT   = OSM(MOD_LSFT)
 ,O_SGUI  = OSM(MOD_LGUI | MOD_LSFT)
};

// modifier keys
#define A_DOWN  ALT_T(KC_DOWN)
#define C_RGHT  CTL_T(KC_RGHT)
#define G_UP    GUI_T(KC_UP)
#define S_DOWN  S    (KC_DOWN)
#define S_END   S    (KC_END)
#define S_HOME  S    (KC_HOME)
#define S_LEFT  S    (KC_LEFT)
#define S_PGDN  S    (KC_PGDN)
#define S_PGUP  S    (KC_PGUP)
#define S_RGHT  S    (KC_RGHT)
#define S_UP    S    (KC_UP)

// tap dance keys
enum tap_dance {
  _CAPS = 0
 ,_LBRC
 ,_LCBR
 ,_LPRN
 ,_LT
 ,_QUOT
 ,_SEND
 ,_SPC
 ,_SPRN
};

#define T_CAPS  TD(_CAPS)
#define T_LBRC  TD(_LBRC)
#define T_LCBR  TD(_LCBR)
#define T_LPRN  TD(_LPRN)
#define T_LT    TD(_LT)
#define T_QUOT  TD(_QUOT)
#define T_SEND  TD(_SEND)                   // compile time macro string
#define T_SPC   TD(_SPC)                    // see process_record_user() for extended handling of Spc
#define T_SPRN  TD(_SPRN)

// layer keys
#define M_REGEX MO(_REGHEX)

// keycodes
#define ___x___ KC_TRNS
#define _______ KC_NO

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

// ..................................................................... Colemak
//
// http://www.keyboard-layout-editor.com/#/gists/34a2cb32e4f9267275c08a8089ca2d3c

  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   W  |   F  |   P  |   V  | Caps |^Shift|   J  |   L  |   U  |   Y  |   ;  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   A  |   R  |   S  |   T  |   G  | ^Alt | ^GUI |   M  |   N  |   E  |   I  |   O  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   Z  |   X  |   C  |   D  |   B  | ↑Alt | ↑GUI |   K  |   H  |   ,  |   .  |   "  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt |  Esc | Space|  Tab | Bksp |  Ent | Left | Down |  Up  |Right |
  // `-----------------------------------------------------------------------------------'

  [_COLEMAK] = {
    {KC_Q,    KC_W,    KC_F,    KC_P,    KC_V,    T_CAPS,  O_CSFT,   KC_J,    KC_L,    KC_U,    KC_Y,   KC_SCLN},
    {KC_A,    KC_R,    KC_S,    KC_T,    KC_G,    O_CALT,  O_CGUI,   KC_M,    KC_N,    KC_E,    KC_I,   KC_O   },
    {KC_Z,    KC_X,    KC_C,    KC_D,    KC_B,    O_SALT,  O_SGUI,   KC_K,    KC_H,    KC_COMM, KC_DOT, T_QUOT },
    {O_CTL,   O_GUI,   O_ALT,   L_ESC,   T_SPC,   L_TAB,   L_BSPC,   L_ENT,   L_LEFT,  A_DOWN,  G_UP,   C_RGHT },
  },

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
  // |      |      |   A  |   O  |      |      |      |   E  |   U  |      |      | Exit |
  // `-----------------------------------------------------------------------------------'

  [_PLOVER] = {
    {KC_1,    KC_1,    KC_1,    KC_1,    KC_1,    _______, KC_1,    KC_1,    KC_1,    KC_1,    KC_1,    KC_1   },
    {KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    _______, KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC},
    {KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    _______, KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, KC_QUOT},
    {_______, _______, KC_C,    KC_V,    _______, _______, _______, KC_N,    KC_M,    _______, _______, PLOVEX },
  },

// ............................................................... Shift Colemak
//
// http://www.keyboard-layout-editor.com/#/gists/3e7b27b824d0c8b71f07354170756803

  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   W  |   F  |   P  |   V  | Caps |^Shift|   J  |   L  |   U  |   Y  |   ;  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   A  |   R  |   S  |   T  |   G  | ^Alt | ^GUI |   M  |   N  |   E  |   I  |   O  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   Z  |   X  |   C  |   D  |   B  | ↑Alt | ↑GUI |   K  |   H  |   /  |   ?  |   "  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt |  Esc |  f() |  Tab |  Del |   -  | Left | Down |  Up  |Right |
  // `-----------------------------------------------------------------------------------'

  [_LSHIFT] = {
    {S(KC_Q), S(KC_W), S(KC_F), S(KC_P), S(KC_V), T_CAPS,  O_CSFT, S(KC_J),  S(KC_L), S(KC_U), S(KC_Y), KC_COLN},
    {S(KC_A), S(KC_R), S(KC_S), S(KC_T), S(KC_G), O_CALT,  O_CGUI,  S(KC_M), S(KC_N), S(KC_E), S(KC_I), S(KC_O)},
    {S(KC_Z), S(KC_X), S(KC_C), S(KC_D), S(KC_B), O_SALT,  O_SGUI,  S(KC_K), S(KC_H), KC_SLSH, KC_QUES, KC_DQT },
    {O_CTL,    O_GUI,    O_ALT, L_ESC,   ___x___, L_TAB,   KC_DEL,  KC_MINS, P_LEFT,  S_DOWN,  S_UP,    S_RGHT },
  },

  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   W  |   F  |   P  |   V  | Caps |^Shift|   J  |   L  |   U  |   Y  |   ;  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   A  |   R  |   S  |   T  |   G  | ^Alt | ^GUI |   M  |   N  |   E  |   I  |   O  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   Z  |   X  |   C  |   D  |   B  | ↑Alt | ↑GUI |   K  |   H  |   ~  |   `  |   "  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt |  Esc |   _  | ↑Tab | Bksp |  f() | Left | Down |  Up  |Right |
  // `-----------------------------------------------------------------------------------'

  [_RSHIFT] = {
    {S(KC_Q), S(KC_W), S(KC_F), S(KC_P), S(KC_V), T_CAPS,  O_CSFT,  S(KC_J), S(KC_L), S(KC_U), S(KC_Y), KC_COLN},
    {S(KC_A), S(KC_R), S(KC_S), S(KC_T), S(KC_G), O_CALT,  O_CGUI,  S(KC_M), S(KC_N), S(KC_E), S(KC_I), S(KC_O)},
    {S(KC_Z), S(KC_X), S(KC_C), S(KC_D), S(KC_B), O_SALT,  O_SGUI,  S(KC_K), S(KC_H), KC_TILD, KC_GRV,  KC_DQT },
    {O_CTL,   O_GUI,   O_ALT,   L_ESC,   KC_UNDS, P_TAB,   L_BSPC,  ___x___, P_LEFT,  S_DOWN,  S_UP,    S_RGHT },
  },

// ......................................................... Number Keypad Layer
//
// http://www.keyboard-layout-editor.com/#/gists/538d5196b49574fffda305a0f845c794

  // .-----------------------------------------------------------------------------------.
  // |   {  |      |      | ^Alt |   }  |      |      |      |   7  |   8  |   9  |   /  |
  // |-----------------------------------------------------------------------------------|
  // |   (  | Ctrl |  GUI |  Alt |   )  |      |      |      |   4  |   5  |   6  |   *  |
  // |-----------------------------------------------------------------------------------|
  // |   [  |   <  |   >  | ↑Alt |   ]  |      |      |      |   1  |   2  |   3  |   -  |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |  f() |      |       |      |  =  |   0  |   .  |   :  |   +  |
  // '-----------------------------------------------------------------------------------'

  [_NUMBER] = {
    {T_LCBR,  _______, _______, O_CALT,  KC_RCBR, _______, _______, S(KC_C), KC_7,    KC_8,    KC_9,    KC_SLSH},
    {T_SPRN,  O_CTL,    O_GUI,    O_ALT, KC_RPRN, _______, _______, S(KC_B), KC_4,    KC_5,    KC_6,    KC_ASTR},
    {T_LBRC,  T_LT,    KC_GT,   O_SALT,  KC_RBRC, _______, _______, S(KC_A), KC_1,    KC_2,    KC_3,    KC_MINS},
    {___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, KC_EQL,  KC_0,    KC_DOT,  KC_COLN, KC_PLUS},
  },

// ................................................................ Symbol Layer
//
// http://www.keyboard-layout-editor.com/#/gists/b14e93e60f484a7e7c0d89351ea5c663

  // .-----------------------------------------------------------------------------------.
  // |   {  |   .  |   *  |   &  |   }  |      |      |      | Home |  Up  |  End |      |
  // |-----------------------------------------------------------------------------------|
  // |   (  |   ^  |   %  |   $  |   )  |      |      |      | Left | Down | Right|      |
  // |-----------------------------------------------------------------------------------|
  // |   [  |   #  |   @  |   !  |   ]  |      |      |      | PgDn | PgUp |      |      |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |   \  |   |  |      |      |      |  f() |      |      |      |
  // '-----------------------------------------------------------------------------------'

  [_SYMBOL] = {
    {T_LCBR,  KC_DOT,  KC_ASTR, KC_AMPR, KC_RCBR, _______, _______, _______, KC_HOME, KC_UP,   KC_END,  _______},
    {T_LPRN,  KC_CIRC, KC_PERC, KC_DLR,  KC_RPRN, _______, _______, _______, KC_LEFT, KC_DOWN, KC_RGHT, M_REGEX},
    {T_LBRC,  KC_HASH, KC_AT,   KC_EXLM, KC_RBRC, _______, _______, _______, KC_PGDN, KC_PGUP, _______, _______},
    {___x___, ___x___, ___x___, KC_BSLS, P_PIPE,  ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___},
  },

// ....................................................... Number Symbol Overlay
//
// http://www.keyboard-layout-editor.com/#/gists/a5af1dc7defc033feac465339f0cd6bc

  // .-----------------------------------------------------------------------------------.
  // |      |   ?  |   +  |   ~  |      |      |      |   E  |   &  |   *  |   ~  |      |
  // |-----------------------------------------------------------------------------------|
  // |  f() |   3  |   2  |   1  |      |      |      |   C  |   $  |   %  |   ^  |  f() |
  // |-----------------------------------------------------------------------------------|
  // |      |   <  |   >  |   =  |      |      |      |   A  |   !  |   @  |   #  |      |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |   :  |      |      |      |      | Space|   ,  |   \  |   |  |
  // '-----------------------------------------------------------------------------------'

  [_REGHEX] = {
    {___x___, KC_QUES, KC_PLUS, KC_TILD, ___x___, _______, _______, S(KC_F), KC_AMPR, KC_ASTR, KC_TILD, ___x___},
    {___x___, KC_3,    KC_2,    KC_1,    ___x___, _______, _______, S(KC_E), KC_DLR,  KC_PERC, KC_CIRC, ___x___},
    {___x___, T_LT,    KC_GT,   KC_EQL,  ___x___, _______, _______, S(KC_D), KC_EXLM, KC_AT,   KC_HASH, ___x___},
    {___x___, ___x___, ___x___, KC_COLN, ___x___, ___x___, ___x___, ___x___, KC_SPC,  KC_COMM, KC_BSLS, KC_PIPE},
  },

// ...................................................... Shift Navigation Layer
//
// http://www.keyboard-layout-editor.com/#/gists/3e7b27b824d0c8b71f07354170756803
// http://www.keyboard-layout-editor.com/#/gists/b14e93e60f484a7e7c0d89351ea5c663

  // .-----------------------------------------------------------------------------------.
  // |      |      |      |      |      |      |      |      | ↑Home|  ↑Up | ↑End |      |
  // |-----------------------------------------------------------------------------------|
  // |      | Ctrl |  GUI |  Alt |      |      |      |      | ↑Left| ↑Down|↑Right|      |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |      |      |      |      | ↑PgDn| ↑PgUp|      |      |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |  f() |      |      |      |  f() |      |      |      |
  // '-----------------------------------------------------------------------------------'

  [_SFTNAV] = {
    {_______, _______, _______, _______, _______, _______, _______, _______, S_HOME,  S_UP,    S_END,   _______},
    {_______, O_CTL,   O_GUI,   O_ALT,   _______, _______, _______, _______, S_LEFT,  S_DOWN,  S_RGHT,  _______},
    {_______, _______, _______, _______, _______, _______, _______, _______, S_PGDN,  S_PGUP,  _______, _______},
    {_______, _______, _______, _______, ___x___, _______, _______, _______, ___x___, _______, _______, _______},
  },

// ............ .................................................. Function Keys
//
// http://www.keyboard-layout-editor.com/#/gists/d937a7bb9dec5dc6e73b28bf95d4d517

  // .-----------------------------------------------------------------------------------.
  // |      |      |      |      |      |      |      |      |  F7  |  F8  |  F9  |  F12 |
  // |-----------------------------------------------------------------------------------|
  // | Ctrl |  GUI |  Alt | Shift| Send |      |      |      |  F4  |  F5  |  F6  |  F11 |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |      |      |      |      |  F1  |  F2  |  F3  |  F10 |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |      |  f() |      |   +  |      |      |      |      |
  // '-----------------------------------------------------------------------------------'

  [_FNCKEY] = {
    {_______, _______, _______, _______, _______, _______, _______, _______, KC_F7,   KC_F8,   KC_F9,   KC_F12 },
    {O_SFT,   O_CTL,   O_GUI,   O_ALT,   T_SEND,  _______, _______, _______, KC_F4,   KC_F5,   KC_F6,   KC_F11 },
    {_______, _______, _______, _______, _______, _______, _______, _______, KC_F1,   KC_F2,   KC_F3,   KC_F10 },
    {_______, _______, _______, _______, _______, ___x___, _______, KC_PLUS, _______, _______, _______, _______},
  },

// ................................................................ Adjust Layer
//
// http://www.keyboard-layout-editor.com/#/gists/ac56b98d8737118f2beef3d6855d760e

  // ,-----------------------------------------------------------------------------------.
  // |      |      |      |      |      |      |      |      |      |      |      | Reset|
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |AGnorm|Voice-|Audoff|Musoff|MIDIof|      |      |      |Colemk|      |      |      |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |AGswap|Voice+|Aud on|Mus on|MIDIon|      |      |      |      |   <  |   >  |      |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // |      |      |      |      |      |      |  f() |      |      |      |      |Plover|
  // `-----------------------------------------------------------------------------------'

  [_ADJUST] = {
    {_______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, RESET  },
    {AG_NORM, MUV_DE,  AU_OFF,  MU_OFF,  MI_OFF,  _______, _______, _______, COLEMAK, _______, _______, _______},
    {AG_SWAP, MUV_IN,  AU_ON,   MU_ON,   MI_ON,   _______, _______, _______, _______, T_LT,    KC_GT,   _______},
    {_______, _______, _______, _______, _______, _______, ___x___, _______, _______, _______, _______, PLOVER },
  },
};

#ifdef AUDIO_ENABLE
#define CAPSLOCK_ON_SOUND   E__NOTE(_E7)  \
                           ,Q__NOTE(_GS7)
#define CAPSLOCK_OFF_SOUND  E__NOTE(_GS7) \
                           ,Q__NOTE(_E7)
float tone_startup  [][2] = SONG   (STARTUP_SOUND);
float tone_colemak  [][2] = SONG   (COLEMAK_SOUND);
float tone_plover   [][2] = SONG   (PLOVER_SOUND);
float tone_plover_gb[][2] = SONG   (PLOVER_GOODBYE_SOUND);
float tone_caps_on  [][2] = SONG   (CAPSLOCK_ON_SOUND);
float tone_caps_off [][2] = SONG   (CAPSLOCK_OFF_SOUND);
float music_scale   [][2] = SONG   (MUSIC_SCALE_SOUND);
float tone_goodbye  [][2] = SONG   (GOODBYE_SOUND);
#endif

static uint16_t key_timer = 0;
static int      keymap    = 0;

void matrix_scan_user(void)
{
  // set layer of LT (layer, key) for modified key value, see process_record_user()
  if (key_timer != 0) {
    if (timer_elapsed(key_timer) > TAPPING_TERM) {
      key_timer = 0;
      if (keymap != 0) {
        layer_on(keymap);
      }
    }
  }
}

void tap_key(int kc)
{
  register_code  (kc);
  unregister_code(kc);
}

#define S_NEVER  0
#define S_SINGLE 1
#define S_DOUBLE 2
#define S_ALWAYS S_SINGLE | S_DOUBLE

void tap_pair(qk_tap_dance_state_t *state, int shift, int left, int right, int layer)
{
  // double tap: left right
  if (state->count > 1) {
    if (shift & S_DOUBLE) {
      register_code(KC_LSFT);
    }
    tap_key(left);
    tap_key(right);
    if (shift & S_DOUBLE) {
      unregister_code(KC_LSFT);
    }
    // cursor placement removed for opposite effect within vim :-)
    // register_code  (KC_LEFT);
    // unregister_code(KC_LEFT);
  }
  // down: layer
  else if (state->pressed) {
    if (layer != 0) {
      layer_on(layer);
    }
  }
  // tap: left
  else {
    if (shift & S_SINGLE) {
      register_code(KC_LSFT);
    }
    tap_key(left);
    if (shift & S_SINGLE) {
      unregister_code(KC_LSFT);
    }
  }
  reset_tap_dance(state);
}

void paren_layer(qk_tap_dance_state_t *state, void *user_data)
{
  tap_pair(state, S_ALWAYS, KC_9, KC_0, _REGHEX);
}

void paren_reset(qk_tap_dance_state_t *state, void *user_data)
{
  layer_off(_REGHEX);
}

// tap dance persistant mods, see process_record_user()
// keyboard_report->mods appears to be cleared by tap dance(?)
static int mods = 0;

void tap_mods(keyrecord_t *record, uint16_t keycode)
{
  if (record->event.pressed) {
    mods |= MOD_BIT(keycode);
  }
  else {
    mods &= ~(MOD_BIT(keycode));
  }
}

// (un)register modifier
void modifier(void (*f)(uint8_t))
{
  if (mods & MOD_BIT(KC_LCTL)) {
    (*f)(KC_LCTL);
  }
  else if (mods & MOD_BIT(KC_LGUI)) {
    (*f)(KC_LGUI);
  }
  else if (mods & MOD_BIT(KC_LALT)) {
    (*f)(KC_LALT);
  }
}

// augment pseudo LT (_LSHIFT, KC_SPC) handling below for rapid <SPACE><SHIFT> sequences
void shift(qk_tap_dance_state_t *state, void *user_data)
{
  // double tap down: repeating space
  if (state->count > 2) {
    tap_key      (KC_SPC);
    register_code(KC_SPC);
  }
  // tap down: space shift
  else if (state->count > 1) {
    tap_key (KC_SPC);
    layer_on(_LSHIFT);
  }
  // down: shift
  else if (state->pressed) {
    layer_on(_LSHIFT);
  }
  // tap: space
  else {
    modifier(register_code);
    tap_key (KC_SPC);
    modifier(unregister_code);
  }
  reset_tap_dance(state);
}

void shift_reset(qk_tap_dance_state_t *state, void *user_data)
{
  unregister_code(KC_SPC);
  layer_off      (_LSHIFT);
}

void angle(qk_tap_dance_state_t *state, void *user_data)
{
  tap_pair(state, S_ALWAYS, KC_COMM, KC_DOT, 0);
}

void brace(qk_tap_dance_state_t *state, void *user_data)
{
  tap_pair(state, S_NEVER, KC_LBRC, KC_RBRC, 0);
}

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

void curly(qk_tap_dance_state_t *state, void *user_data)
{
  tap_pair(state, S_ALWAYS, KC_LBRC, KC_RBRC, 0);
}

void paren(qk_tap_dance_state_t *state, void *user_data)
{
  tap_pair(state, S_ALWAYS, KC_9, KC_0, 0);
}

void quote(qk_tap_dance_state_t *state, void *user_data)
{
  tap_pair(state, S_DOUBLE, KC_QUOT, KC_QUOT, 0);
}

void send(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1) {
#ifdef COMPILE_STRING
#include "compile_string.h"
#endif
  }
}

qk_tap_dance_action_t tap_dance_actions[] = {
  [_SPRN] = {
    .fn        = { NULL, paren_layer, paren_reset },
    .user_data = NULL
  }
  ,[_SPC] = {
    .fn        = { NULL, shift, shift_reset },
    .user_data = NULL
  }
 ,[_CAPS] = ACTION_TAP_DANCE_FN(caps)
 ,[_LBRC] = ACTION_TAP_DANCE_FN(brace)
 ,[_LCBR] = ACTION_TAP_DANCE_FN(curly)
 ,[_LPRN] = ACTION_TAP_DANCE_FN(paren)
 ,[_LT]   = ACTION_TAP_DANCE_FN(angle)
 ,[_QUOT] = ACTION_TAP_DANCE_FN(quote)
 ,[_SEND] = ACTION_TAP_DANCE_FN(send)
};

void persistant_default_layer_set(uint16_t default_layer)
{
  eeconfig_update_default_layer(default_layer);
  default_layer_set            (default_layer);
}

void clear_layers(void)
{
  layer_off(_COLEMAK);
  layer_off(_LSHIFT);
  layer_off(_RSHIFT);
  layer_off(_NUMBER);
  layer_off(_REGHEX);
  layer_off(_SYMBOL);
  layer_off(_SFTNAV);
  layer_off(_FNCKEY);
  layer_off(_ADJUST);
}

void clear_sticky(void)
{
  // undo sticky modifiers
  unregister_code(KC_LALT);
  unregister_code(KC_LGUI);
  unregister_code(KC_LCTL);
}

void toggle_plover(void)
{
  // toggle window manager plover application, see herbstluftwm/config/appbinds
  register_code  (KC_LGUI);
  register_code  (KC_LSFT);
  tap_key        (KC_RGHT);
  unregister_code(KC_LSFT);
  unregister_code(KC_LGUI);
}

#define    LEFT    1
#define    RIGHT   2
static int thumb = 0;

void rolling_layer(keyrecord_t *record, uint16_t timer, int side, int kc, int layer, int overlay_layer, int rollover_layer)
{
  if (record->event.pressed) {
    // set layer, see matrix_scan_user()
    key_timer = timer;
    thumb     = thumb | side;
    keymap    = layer;
  }
  else {
    layer_off  (_SFTNAV);
    // opposite thumb keycode handler may have switched effective layer!
    if (overlay_layer) {
      layer_off(overlay_layer);
    }
    if (key_timer > 0) {
      if (timer_elapsed(key_timer) < TAPPING_TERM) {
        register_code  (KC_LSFT);
        tap_key        (kc);
        unregister_code(KC_LSFT);
      }
    }
    // rollover to opposite thumb layer
    else if (thumb & (side == LEFT ? RIGHT : LEFT)) {
      layer_on(rollover_layer);
    }
    thumb     = thumb & ~side;
    key_timer = 0;
    keymap    = 0;
    clear_sticky();
  }
}

void modifier_layer(keyrecord_t *record, uint16_t timer, int kc, int layer)
{
  if (record->event.pressed) {
    // set layer, see matrix_scan_user()
    key_timer = timer_read();
    keymap    = layer;
  }
  else {
    layer_off(layer);
    if (key_timer > 0) {
      if (timer_elapsed(key_timer) < TAPPING_TERM) {
        register_code  (KC_LSFT);
        tap_key        (kc);
        unregister_code(KC_LSFT);
      }
    }
    key_timer = 0;
    keymap    = 0;
    clear_sticky();
  }
}

bool process_record_user(uint16_t keycode, keyrecord_t *record)
{
  switch (keycode) {
    case O_CTL:
      tap_mods(record, KC_LCTL);
      break;
    case O_GUI:
      tap_mods(record, KC_LGUI);
      break;
    case O_ALT:
      tap_mods(record, KC_LALT);
      break;
    case A_DOWN:
      tap_mods(record, KC_LALT);
      break;
    case G_UP:
      tap_mods(record, KC_LGUI);
      break;
    case C_RGHT:
      tap_mods(record, KC_LCTL);
      break;
    // emulate LT (_FNCKEY, S(KC_TAB))
    case P_TAB:
      modifier_layer(record, timer_read(), KC_TAB, _FNCKEY);
      break;
    // emulate LT (_SFTNAV, S(KC_LEFT))
    case P_LEFT:
      rolling_layer(record, timer_read(), RIGHT, KC_LEFT, _SFTNAV, _SYMBOL, _LSHIFT);
      break;
    // LT (_LSHIFT, KC_SPC) handling extensions
    case T_SPC:
      rolling_layer(record, 0, LEFT, 0, 0, 0, _SYMBOL);
      break;
    // emulate LT (_SFTNAV, S(KC_BSLS))
    case P_PIPE:
      rolling_layer(record, timer_read(), LEFT, KC_BSLS, _SFTNAV, _LSHIFT, _SYMBOL);
      break;
    // LT (_SYMBOL, KC_LEFT) handling extensions
    case L_LEFT:
      rolling_layer(record, 0, RIGHT, 0, 0, 0, _LSHIFT);
      break;
    case COLEMAK:
      if (record->event.pressed) {
#ifdef AUDIO_ENABLE
        PLAY_NOTE_ARRAY(tone_colemak, false, 0);
#endif
        clear_layers();
        persistant_default_layer_set(1UL<<_COLEMAK);
      }
      return false;
      break;
    case PLOVER:
      if (record->event.pressed) {
#ifdef AUDIO_ENABLE
        stop_all_notes();
        PLAY_NOTE_ARRAY(tone_plover, false, 0);
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
      return false;
      break;
    case PLOVEX:
      if (record->event.pressed) {
#ifdef AUDIO_ENABLE
        PLAY_NOTE_ARRAY(tone_plover_gb, false, 0);
#endif
        layer_off(_PLOVER);
        clear_layers();
        toggle_plover();
      }
      return false;
      break;
  }
  return true;
}

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
      PLAY_NOTE_ARRAY(tone_caps_on,  false, 0);
    }
    else if (!(usb_led & (1<<USB_LED_CAPS_LOCK)) && (old_usb_led & (1<<USB_LED_CAPS_LOCK))) {
      // if capslock LED is turning off
      PLAY_NOTE_ARRAY(tone_caps_off, false, LEGATO);
    }
  }
  old_usb_led = usb_led;
}
#endif

void startup_user()
{
  _delay_ms      (20);                      // gets rid of tick
  PLAY_NOTE_ARRAY(tone_startup, false, 0);
}

void shutdown_user()
{
  PLAY_NOTE_ARRAY(tone_goodbye, false, 0);
  _delay_ms      (150);
  stop_all_notes();
}

void music_on_user(void)
{
  music_scale_user();
}

void music_scale_user(void)
{
  PLAY_NOTE_ARRAY(music_scale, false, 0);
}
#endif

// set ft comment style
// vim: set ft=cpp: //
