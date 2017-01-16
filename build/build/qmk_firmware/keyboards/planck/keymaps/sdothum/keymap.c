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
// Attention
// ▔▔▔▔▔▔▔▔▔
//   Turn layer_off _ADJUST before setting default layer, else a usb reset
//   will be necessary (disconnect, connect) after changing layouts
//
// Notes
// ▔▔▔▔▔
//   ** S T R E T C H ** Layout
//
//   New colemak-dh, number, symbol/function and navigation pad layers
//
//   Autocompletion tap dance key pairs (),[],{} are available from the
//   number/symbol layer, as well as, numerous (un)shift key values
//
//   The navigation pad provides a single hand right thumb activated cluster
//   with left hand modifiers
//
//   Plover layer toggling has added binding to the herbstluftwm window manager
//   to enable/disable the necessary plover software
//
//   Adjust layer is enabled from the navigation layer i.e. num+sym+grave
//   to prevent accidental keyboard reset
//
//   Tri-layer toggling requires defining toggle layer hack for the zero key
//   and dot keys of the number and symbol layers, else num+sym navigation
//   layer is inaccessible -- pretty cool!
//
// Modifier clusters
// ▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔▔
//   The num and sym keys together access the navigation pad layer
//
//   ,-----------------------------------------------------------------------------------.
//   |  Kpd | Ctrl | GUI  | Alt  | Num  |Shift |Shift | Sym  | Alt  | GUI  | Ctrl | Rght |
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
 ,_NUMBER
 ,_NUMSYM
 ,_SYMBOL
 ,_SYMREG
 ,_NAVPAD
 ,_KEYTEST
 ,_ADJUST
};

// update_tri_layer hack from https://www.reddit.com/r/olkb/comments/4x3dei/hack_too_ugly_to_live/?ref=search_posts
enum planck_keycodes {
  COLEMAK = SAFE_RANGE
 ,PLOVER
 ,PLOVEX
 ,KEYTEST
 ,_Ctl  = OSM (MOD_LCTL)
 ,_Gui  = OSM (MOD_LGUI)
 ,_Alt  = OSM (MOD_LALT)
 ,_SGui = OSM (MOD_LGUI | MOD_LSFT)
 ,_SAlt = OSM (MOD_LALT | MOD_LSFT)
 ,_CAlt = OSM (MOD_LALT | MOD_LCTL)
 ,_Sft  = OSM (MOD_LSFT)
 ,Dot   = LT  (_NUMBER, KC_DOT)
 ,Esc   = LT  (_NUMBER, KC_ESC)
 ,Ent   = LT  (_SYMBOL, KC_ENT)
 ,Zero  = LT  (_SYMBOL, KC_0)
};

enum tap_dance {
  _LPRN = 0
 ,_LBRC
 ,_LCBR
 ,_QUOT
 ,_SLSH
 ,_PLUS
};

// modifier keys
#define Q       GUI_T (SFT_T (KC_Q))
#define A       ALT_T (SFT_T (KC_A))
#define Z       ALT_T (CTL_T (KC_Z))
#define Spc     SFT_T (KC_SPC)
#define Tab     SFT_T (CTL_T (KC_TAB))
#define Del     SFT_T (CTL_T (KC_DEL))
#define Bspc    SFT_T (KC_BSPC)
#define Down    ALT_T (KC_DOWN)
#define Left    GUI_T (KC_LEFT)
#define Rght    CTL_T (KC_RGHT)
#define Up      ALT_T (CTL_T (KC_UP))
#define O       ALT_T (SFT_T (KC_O))
#define Scln    GUI_T (SFT_T (KC_SCLN))

// tap dance keys
#define __Lcbr  TD (_LCBR)
#define __Lprn  TD (_LPRN)
#define __Lbrc  TD (_LBRC)
#define __Quot  TD (_QUOT)
#define __Slsh  TD (_SLSH)
#define __Plus  TD (_PLUS)

// layer keys
#define ADJUST  MO (_ADJUST)
#define SYMBOL  MO (_NUMSYM)
#define Ftwo    LT (_SYMREG, KC_F2)

// keycodes
#define Colon   S (KC_SCLN)
#define Greater S (KC_DOT)
#define Lessor  S (KC_COMM)
#define Bar     S (KC_BSLS)
#define ___x___ KC_TRNS
#define _______ KC_NO

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

// ..................................................................... Colemak
//
// http://www.keyboard-layout-editor.com/#/gists/f812508b6fbfcad6f9acd6c198a83e13

  // ,-----------------------------------------------------------------------------------.
  // |   Q  |   W  |   F  |   P  |   V  |   ~  |   \  |   J  |   L  |   U  |   Y  |   ;  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   A  |   R  |   S  |   T  |   G  |   -  |   "  |   M  |   N  |   E  |   I  |   O  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   Z  |   X  |   C  |   D  |   B  |   =  |   /  |   K  |   H  |   ,  |   .  |  Up  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt |  Esc | Space|  Tab |  Del | Bksp |  Ent | Down | Left |Right |
  // `-----------------------------------------------------------------------------------'

  [_COLEMAK] = {
    {Q,       KC_W,    KC_F,    KC_P,    KC_V,    KC_GRV,  KC_BSLS, KC_J,    KC_L,    KC_U,    KC_Y,    Scln   },
    {A,       KC_R,    KC_S,    KC_T,    KC_G,    KC_MINS, __Quot,  KC_M,    KC_N,    KC_E,    KC_I,    O      },
    {Z,       KC_X,    KC_C,    KC_D,    KC_B,    KC_EQL,  KC_SLSH, KC_K,    KC_H,    KC_COMM, KC_DOT,  Up     },
    {_Ctl,    _Gui,    _Alt,    Esc,     Spc,     Tab,     Del,     Bspc,    Ent,     Down,    Left,    Rght   },
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

// ................................................................ Number Layer
//
// http://www.keyboard-layout-editor.com/#/gists/dc0f33114da5ea86a2518ad718798f2a

  // .-----------------------------------------------------------------------------------.
  // | ↑GUI |   *  |   -  |   {  |   }  |      |      |   E  |   7  |   8  |   9  |   F  |
  // |-----------------------------------------------------------------------------------|
  // | ↑Alt |   /  |   +  |   (  |   )  |      |      |   C  |   4  |   5  |   6  |   D  |
  // |-----------------------------------------------------------------------------------|
  // | ^Alt |      |Symbol|   [  |   ]  |      |      |   A  |   1  |   2  |   3  |   B  |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |  f() |      |       |      |     |   0  |   .  |      |      |
  // '-----------------------------------------------------------------------------------'

  [_NUMBER] = {
    {_SGui,   KC_ASTR, KC_MINS, __Lcbr,  KC_RCBR, _______, _______, S(KC_E), KC_7,    KC_8,    KC_9,    S(KC_F)},
    {_SAlt,   __Slsh,  __Plus,  __Lprn,  KC_RPRN, _______, _______, S(KC_C), KC_4,    KC_5,    KC_6,    S(KC_D)},
    {_CAlt,   _______, SYMBOL,  __Lbrc,  KC_RBRC, _______, _______, S(KC_A), KC_1,    KC_2,    KC_3,    S(KC_B)},
    {___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, Zero,    KC_DOT,  ___x___, ___x___},
  },

// ........................................................ Number Layer Symbols

  // .-----------------------------------------------------------------------------------.
  // |      |      |      |      |      |      |      |      |   &  |   *  |   :  |   ;  |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |      |      |      |      |   $  |   %  |   ^  |   >  |
  // |-----------------------------------------------------------------------------------|
  // |      |      |  f() |      |      |      |      |      |   !  |   @  |   #  |   <  |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |  f() |      |      |      |      | Space|   ,  |      |      |
  // '-----------------------------------------------------------------------------------'

  [_NUMSYM] = {
    {_______, _______, _______, _______, _______, _______, _______, S(KC_E), KC_AMPR, KC_ASTR, Colon,   KC_SCLN},
    {_______, _______, _______, _______, _______, _______, _______, S(KC_C), KC_DLR,  KC_PERC, KC_CIRC, Greater},
    {_______, _______, ___x___, _______, _______, _______, _______, S(KC_A), KC_EXLM, KC_AT,   KC_HASH, Lessor },
    {___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, KC_SPC,  KC_COMM, ___x___, ___x___},
  },

// ................................................................ Symbol Layer
//
// http://www.keyboard-layout-editor.com/#/gists/ff466007b5cfef86e31209debb0cc9e3

  // .-----------------------------------------------------------------------------------.
  // |   &  |   *  |   \  |   {  |   }  |      |      |      |  F9  |  F10 |  F11 |  F12 |
  // |-----------------------------------------------------------------------------------|
  // |   $  |   %  |   ^  |   (  |   )  |      |      |      |  F5  |  F6  |  F7  |  F8  |
  // |-----------------------------------------------------------------------------------|
  // |   !  |   @  |   #  |   [  |   ]  |      |      |      |  F1  |  F2  |  F3  |  F4  |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |   .  |      |      |      |      |  f() |      |      |      |
  // '-----------------------------------------------------------------------------------'

  [_SYMBOL] = {
    {KC_AMPR, KC_ASTR, KC_BSLS, __Lcbr,  KC_RCBR, _______, _______, _______, KC_F9,   KC_F10,  KC_F11,  KC_F12 },
    {KC_DLR,  KC_PERC, KC_CIRC, __Lprn,  KC_RPRN, _______, _______, _______, KC_F5,   KC_F6,   KC_F7,   KC_F8  },
    {KC_EXLM, KC_AT,   KC_HASH, __Lbrc,  KC_RBRC, _______, _______, _______, KC_F1,   Ftwo,    KC_F3,   KC_F4  },
    {___x___, ___x___, ___x___, Dot,     ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___},
  },

// .......................................................... Symbol Layer Regex

  // .-----------------------------------------------------------------------------------.
  // |   ~  |   +  |   |  |      |      |      |      |      |      |      |      |      |
  // |-----------------------------------------------------------------------------------|
  // |   <  |   =  |   >  |      |      |      |      |      |      |      |      |      |
  // |-----------------------------------------------------------------------------------|
  // |   1  |   2  |   3  |      |      |      |      |      |      |  f() |      |      |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |   ?  |      |      |      |      |  f() |      |      |      |
  // '-----------------------------------------------------------------------------------'

  [_SYMREG] = {
    {KC_TILD, KC_PLUS, KC_PIPE, __Lcbr,  KC_RCBR, _______, _______, _______, _______, _______, _______, _______},
    {KC_LABK, KC_EQL,  KC_RABK, __Lprn,  KC_RPRN, _______, _______, _______, _______, _______, _______, _______},
    {KC_1,    KC_2,    KC_3,    __Lbrc,  KC_RBRC, _______, _______, _______, _______, ___x___, _______, _______},
    {___x___, ___x___, ___x___, KC_QUES, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___},
  },

// ............ ..................................................Navigation Pad
//
// http://www.keyboard-layout-editor.com/#/gists/8a4b014d874ae0b15db613009fb7a4c9

  // .-----------------------------------------------------------------------------------.
  // |Adjust|      |      |      |      |      |      |      | Home |  Up  |  End |      |
  // |-----------------------------------------------------------------------------------|
  // | Caps |  GUI | Ctrl | Shift|      |      |      |      | Left | Down | Right|      |
  // |-----------------------------------------------------------------------------------|
  // |      |      | Bksp |  Del |      |      |      |      | PgDn | PgUp |      |      |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |  f() |      |      |      |      |  f() |      |      |      |
  // '-----------------------------------------------------------------------------------'

  [_NAVPAD] = {
    {ADJUST,  _______, _______, _______, _______, _______, _______, _______, KC_HOME, KC_UP,   KC_END,  _______},
    {KC_CAPS, _Gui,    _Ctl,    _Sft,    _______, _______, _______, _______, KC_LEFT, KC_DOWN, KC_RGHT, _______},
    {_______, _______, KC_BSPC, KC_DEL,  _______, _______, _______, _______, KC_PGDN, KC_PGUP, _______, _______},
    {_______, _______, _______, ___x___, _______, _______, _______, _______, ___x___, _______, _______, _______},
  },

// ...................................................... Test Non-printing Keys

  // ,-----------------------------------------------------------------------------------.
  // |Adjust|   .  |   .  |   .  |   .  |   .  |   .  |   .  |   .  |   .  |   .  |   .  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   E  |   .  |   .  |   .  |   .  |   .  |   .  |   .  |   .  |   .  |   .  |   R  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   .  |   .  |   .  |   .  |   .  |   .  |   .  |   .  |   .  |   .  |   .  |   .  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // |   .  |   C  |   G  |   A  |   T  |   S  |   B  |   D  |   L  |   D  |   U  |   R  |
  // `-----------------------------------------------------------------------------------'

  [_KEYTEST] = {
    {ADJUST,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT },
    {KC_E,    KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_R   },
    {KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT,  KC_DOT },
    {KC_DOT,  KC_C,    KC_G,    KC_A,    KC_T,    KC_S,    KC_B,    KC_D,    KC_L,    KC_D,    KC_U,    KC_R   },
  },

// ................................................................ Adjust Layer

  // ,-----------------------------------------------------------------------------------.
  // |  f() | Reset|Keytst|      |      |      |      |      |      |      |      |      |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |      |      |Aud on|Audoff|AGnorm|AGswap|      |      |Colemk|      |      |Plover|
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |Voice-|Voice+|Mus on|Musoff|MIDIon|MIDIof|      |      |      |      |      |      |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // |      |      |      |      |      |      |      |      |      |      |      |      |
  // `-----------------------------------------------------------------------------------'

  [_ADJUST] = {
    {___x___, RESET,   KEYTEST, _______, _______, _______, _______, _______, _______, _______, _______, _______},
    {_______, _______, AU_ON,   AU_OFF,  AG_NORM, AG_SWAP, _______, _______, COLEMAK, _______, _______, PLOVER },
    {MUV_DE,  MUV_IN,  MU_ON,   MU_OFF,  MI_ON,   MI_OFF,  _______, _______, _______, _______, _______, _______},
    {___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___, ___x___},
  },
};

#ifdef AUDIO_ENABLE
#define CAPSLOCK_ON_SOUND   E__NOTE(_E7  ) \
                           ,Q__NOTE(_GS7 )
#define CAPSLOCK_OFF_SOUND  E__NOTE(_GS7 ) \
                           ,Q__NOTE(_E7  )
float tone_startup[][2]   = SONG (STARTUP_SOUND);
float tone_colemak[][2]   = SONG (COLEMAK_SOUND);
float tone_plover[][2]    = SONG (PLOVER_SOUND);
float tone_plover_gb[][2] = SONG (PLOVER_GOODBYE_SOUND);
float tone_caps_on[][2]   = SONG (CAPSLOCK_ON_SOUND);
float tone_caps_off[][2]  = SONG (CAPSLOCK_OFF_SOUND);
float music_scale[][2]    = SONG (MUSIC_SCALE_SOUND);
float tone_goodbye[][2]   = SONG (GOODBYE_SOUND);
#endif

void paren(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1) {
    register_code   (KC_LSFT);
    register_code   (KC_9);
    unregister_code (KC_9);
    register_code   (KC_0);
    unregister_code (KC_0);
    unregister_code (KC_LSFT);
    register_code   (KC_LEFT);
    unregister_code (KC_LEFT);
  } else {
    register_code   (KC_LSFT);
    register_code   (KC_9);
    unregister_code (KC_9);
    unregister_code (KC_LSFT);
  }
  reset_tap_dance(state);
}

void brace(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1) {
    register_code   (KC_LBRC);
    unregister_code (KC_LBRC);
    register_code   (KC_RBRC);
    unregister_code (KC_RBRC);
    register_code   (KC_LEFT);
    unregister_code (KC_LEFT);
  } else {
    register_code   (KC_LBRC);
    unregister_code (KC_LBRC);
  }
  reset_tap_dance(state);
}

void curly(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1) {
    register_code   (KC_LSFT);
    register_code   (KC_LBRC);
    unregister_code (KC_LBRC);
    register_code   (KC_RBRC);
    unregister_code (KC_RBRC);
    unregister_code (KC_LSFT);
    register_code   (KC_LEFT);
    unregister_code (KC_LEFT);
  } else {
    register_code   (KC_LSFT);
    register_code   (KC_LBRC);
    unregister_code (KC_LBRC);
    unregister_code (KC_LSFT);
  }
  reset_tap_dance(state);
}

void quote(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1) {
    register_code   (KC_LSFT);
    register_code   (KC_QUOT);
    unregister_code (KC_QUOT);
    register_code   (KC_QUOT);
    unregister_code (KC_QUOT);
    unregister_code (KC_LSFT);
    register_code   (KC_LEFT);
    unregister_code (KC_LEFT);
  } else {
    register_code   (KC_QUOT);
    unregister_code (KC_QUOT);
  }
  reset_tap_dance(state);
}


void plus(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1) {
    register_code   (KC_EQL);
    unregister_code (KC_EQL);
  } else {
    register_code   (KC_LSFT);
    register_code   (KC_EQL);
    unregister_code (KC_EQL);
    unregister_code (KC_LSFT);
  }
  reset_tap_dance(state);
}

void slash(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1) {
    register_code   (KC_BSLS);
    unregister_code (KC_BSLS);
  } else {
    register_code   (KC_SLSH);
    unregister_code (KC_SLSH);
  }
  reset_tap_dance(state);
}

qk_tap_dance_action_t tap_dance_actions[] = {
  [_LPRN] = ACTION_TAP_DANCE_FN (paren)
 ,[_LBRC] = ACTION_TAP_DANCE_FN (brace)
 ,[_LCBR] = ACTION_TAP_DANCE_FN (curly)
 ,[_QUOT] = ACTION_TAP_DANCE_FN (quote)
 ,[_SLSH] = ACTION_TAP_DANCE_FN (slash)
 ,[_PLUS] = ACTION_TAP_DANCE_FN (plus)
};

void persistant_default_layer_set(uint16_t default_layer)
{
  eeconfig_update_default_layer(default_layer);
  default_layer_set(default_layer);
}

void clear_layers()
{
  layer_off (_COLEMAK);
  layer_off (_PLOVER);
  layer_off (_NUMBER);
  layer_off (_NUMSYM);
  layer_off (_SYMBOL);
  layer_off (_SYMREG);
  layer_off (_NAVPAD);
  layer_off (_KEYTEST);
  layer_off (_ADJUST);
}

bool process_record_user(uint16_t keycode, keyrecord_t *record)
{
  switch (keycode) {
    // case _Ctl:
    //   if (record->event.pressed) {
    //     register_code   (KC_LCTL);
    //   } else {
    //     unregister_code (KC_LCTL);
    //   }
    //   // LT hack
    //   // return false;
    //   break;
    // case _Gui:
    //   if (record->event.pressed) {
    //     register_code   (KC_LGUI);
    //   } else {
    //     unregister_code (KC_LGUI);
    //   }
    //   // LT hack
    //   // return false;
    //   break;
    // case _Alt:
    //   if (record->event.pressed) {
    //     register_code   (KC_LALT);
    //   } else {
    //     unregister_code (KC_LALT);
    //   }
    //   // LT hack
    //   // return false;
    //   break;
    // case _Sft:
    //   if (record->event.pressed) {
    //     register_code   (KC_LSFT);
    //   } else {
    //     unregister_code (KC_LSFT);
    //   }
    //   // LT hack
    //   // return false;
    //   break;
    case Dot:
      if (record->event.pressed) {
        layer_on        (_NUMBER);
        update_tri_layer(_NUMBER, _SYMBOL, _NAVPAD);
      } else {
        layer_off       (_NUMBER);
        update_tri_layer(_NUMBER, _SYMBOL, _NAVPAD);
        // undo sticky modifiers
        unregister_code (KC_LGUI);
        unregister_code (KC_LSFT);
        unregister_code (KC_LCTL);
      }
      // LT hack
      // return false;
      break;
    case Esc:
      if (record->event.pressed) {
        layer_on        (_NUMBER);
        update_tri_layer(_NUMBER, _SYMBOL, _NAVPAD);
      } else {
        layer_off       (_NUMBER);
        update_tri_layer(_NUMBER, _SYMBOL, _NAVPAD);
        // undo sticky modifiers
        unregister_code (KC_LGUI);
        unregister_code (KC_LSFT);
        unregister_code (KC_LCTL);
      }
      // LT hack
      // return false;
      break;
    case Ent:
      if (record->event.pressed) {
        layer_on        (_SYMBOL);
        update_tri_layer(_NUMBER, _SYMBOL, _NAVPAD);
      } else {
        layer_off       (_SYMBOL);
        update_tri_layer(_NUMBER, _SYMBOL, _NAVPAD);
        // undo sticky modifiers
        unregister_code (KC_LGUI);
        unregister_code (KC_LSFT);
        unregister_code (KC_LCTL);
      }
      // LT hack
      // return false;
      break;
    case Zero:
      if (record->event.pressed) {
        layer_on        (_SYMBOL);
        update_tri_layer(_NUMBER, _SYMBOL, _NAVPAD);
      } else {
        layer_off       (_SYMBOL);
        update_tri_layer(_NUMBER, _SYMBOL, _NAVPAD);
        // undo sticky modifiers
        unregister_code (KC_LGUI);
        unregister_code (KC_LSFT);
        unregister_code (KC_LCTL);
      }
      // LT hack
      // return false;
      break;
    case COLEMAK:
      if (record->event.pressed) {
#ifdef AUDIO_ENABLE
        PLAY_NOTE_ARRAY (tone_colemak, false, 0);
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
        PLAY_NOTE_ARRAY (tone_plover, false, 0);
#endif
        clear_layers();
        layer_on        (_PLOVER);
        if (!eeconfig_is_enabled()) {
            eeconfig_init();
        }
        keymap_config.raw = eeconfig_read_keymap();
        keymap_config.nkro = 1;
        eeconfig_update_keymap(keymap_config.raw);
        // toggle plover application on, see herbstluftwm/config/appbinds
        register_code   (KC_LGUI);
        register_code   (KC_LSFT);
        register_code   (KC_EQL);
        unregister_code (KC_EQL);
        // unregister modifiers as late as possible (convention)
        unregister_code (KC_LSFT);
        unregister_code (KC_LGUI);
      }
      return false;
      break;
    case PLOVEX:
      if (record->event.pressed) {
#ifdef AUDIO_ENABLE
        PLAY_NOTE_ARRAY (tone_plover_gb, false, 0);
#endif
        clear_layers();
        // toggle plover application off, see herbstluftwm/config/appbinds
        register_code   (KC_LGUI);
        register_code   (KC_LSFT);
        register_code   (KC_EQL);
        unregister_code (KC_EQL);
        // unregister modifiers as late as possible (convention)
        unregister_code (KC_LSFT);
        unregister_code (KC_LGUI);
      }
      return false;
      break;
    case KEYTEST:
      if (record->event.pressed) {
#ifdef AUDIO_ENABLE
        PLAY_NOTE_ARRAY (music_scale, false, 0);
#endif
        clear_layers();
        persistant_default_layer_set(1UL<<_KEYTEST);
      }
      return false;
      break;
    // default:
    //   if (record->event.pressed) {
    //     // clear timer to preserve action_get_macro shift key value
    //     key_timer = 0;
    //   }
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
void led_set_user(uint8_t usb_led)
{
  static uint8_t old_usb_led = 0;
  // gets rid of tick
  _delay_ms(10);
  if (!is_playing_notes()) {
    if ((usb_led & (1<<USB_LED_CAPS_LOCK)) && !(old_usb_led & (1<<USB_LED_CAPS_LOCK))) {
      // if capslock LED is turning on
      PLAY_NOTE_ARRAY(tone_caps_on,  false, 0);
    } else if (!(usb_led & (1<<USB_LED_CAPS_LOCK)) && (old_usb_led & (1<<USB_LED_CAPS_LOCK))) {
      // if capslock LED is turning off
      PLAY_NOTE_ARRAY(tone_caps_off, false, LEGATO);
    }
  }
  old_usb_led = usb_led;
}

void startup_user()
{
  _delay_ms(20); // gets rid of tick
  PLAY_NOTE_ARRAY (tone_startup, false, 0);
}

void shutdown_user()
{
  PLAY_NOTE_ARRAY (tone_goodbye, false, 0);
  _delay_ms(150);
  stop_all_notes();
}

void music_on_user(void)
{
  music_scale_user();
}

void music_scale_user(void)
{
  PLAY_NOTE_ARRAY (music_scale, false, 0);
}
#endif

// set ft comment style
// vim: set ft=cpp: //
