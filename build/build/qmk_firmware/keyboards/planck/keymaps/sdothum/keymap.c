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
//   New colemak-dh, number, symbol/function and navigation pad layers
//
//   Original qwerty and dvorak layers have normalized enter key, tap key esc
//   tap key right-shift and modifier row (consistent with colemak-dh)
//
//   The number/symbol layer is optimized for symbol keys over function key
//   placement order
//
//   Autocompletion tap dance key pairs (),[],{} are available from the
//   number/symbol layer
//
//   The navigation pad provides a single hand right thumb activated cluster
//
//   Plover layer toggling has added binding to the herbstluftwm window manager
//   to enable/disable the necessary plover software
//
//   Adjust layer is enabled from the number layer i.e. num+grave
//   to prevent accidental keyboard reset
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

#include "config.h"
#include "planck.h"
#include "action_layer.h"
#ifdef AUDIO_ENABLE
#include "audio.h"
#endif
#include "eeconfig.h"

extern keymap_config_t keymap_config;

enum planck_layers {
  _COLEMAK = 0,
  _QWERTY,
  _DVORAK,
  _PLOVER,
  _NUMBER,
  _SYMBOL,
  _NAVPAD,
  _KEYTEST,
  _ADJUST,
};

// update_tri_layer hack from https://www.reddit.com/r/olkb/comments/4x3dei/hack_too_ugly_to_live/?ref=search_posts 
enum planck_keycodes {
  COLEMAK = SAFE_RANGE,
  QWERTY,
  DVORAK,
  PLOVER,
  PLOVEX,
  KEYTEST,
  Tab = LT (_NUMBER, KC_TAB),
  Del = LT (_SYMBOL, KC_DEL),
};

enum tap_dance {
  _LPRN = 0,
  _LBRC,
  _LCBR,
};

// modifier keys
#define Grv     GUI_T (KC_GRV)
#define Esc     CTL_T (KC_ESC)
#define Mins    SFT_T (KC_MINS)
#define Caps    GUI_T (KC_CAPS)
#define Spc     SFT_T (KC_SPC)
#define Bspc    SFT_T (KC_BSPC)
#define Left    ALT_T (KC_LEFT)
#define Down    GUI_T (KC_DOWN)
#define Up      CTL_T (KC_UP)
#define Quot    SFT_T (KC_QUOT)
#define Slsh    SFT_T (KC_SLSH)
#define Ent     CTL_T (KC_ENT)
#define Bsls    GUI_T (KC_BSLS)

// tap dance keys
#define Lcbr    TD    (_LCBR)
#define Lprn    TD    (_LPRN)
#define Lbrc    TD    (_LBRC)

// adjust layer key
#define ADJUST  MO    (_ADJUST)

#define ___x___ KC_TRNS
#define _______ KC_NO

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

// ..................................................................... Colemak

  // ,-----------------------------------------------------------------------------------.
  // |   ~  |   Q  |   W  |   F  |   P  |   B  |   J  |   L  |   U  |   Y  |   ;  |  \   |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |  Esc |   A  |   R  |   S  |   T  |   G  |   M  |   N  |   E  |   I  |   O  |Enter |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   -  |   Z  |   X  |   C  |   D  |   V  |   K  |   H  |   ,  |   .  |   "  |  /   |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // |   =  | Ctrl |  GUI |  Alt |  Tab | Space| Bksp |  Del | Left | Down |  Up  |Right |
  // `-----------------------------------------------------------------------------------'

[_COLEMAK] = {
  {Grv,     KC_Q,    KC_W,    KC_F,    KC_P,    KC_B,    KC_J,    KC_L,    KC_U,    KC_Y,    KC_SCLN, Bsls   },
  {Esc,     KC_A,    KC_R,    KC_S,    KC_T,    KC_G,    KC_M,    KC_N,    KC_E,    KC_I,    KC_O,    Ent    },
  {Mins,    KC_Z,    KC_X,    KC_C,    KC_D,    KC_V,    KC_K,    KC_H,    KC_COMM, KC_DOT,  KC_QUOT, Slsh   },
  {KC_EQL,  KC_LCTL, Caps,    KC_LALT, Tab,     Spc,     Bspc,    Del,     Left,    Down,    Up,      KC_RGHT},
},

// ...................................................................... Qwerty

  // ,-----------------------------------------------------------------------------------.
  // |   ~  |   Q  |   W  |   E  |   R  |   T  |   Y  |   U  |   I  |   O  |   P  |  \   |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |  Esc |   A  |   S  |   D  |   F  |   G  |   H  |   J  |   K  |   L  |   ;  |Enter |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   -  |   Z  |   X  |   C  |   V  |   B  |   N  |   M  |   ,  |   .  |   /  |  "   |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // |   =  | Ctrl |  GUI |  Alt |  Tab | Space| Bksp |  Del | Left | Down |  Up  |Right |
  // `-----------------------------------------------------------------------------------'

[_QWERTY] = {
  {Grv,     KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    Bsls   },
  {Esc,     KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, Ent    },
  {Mins,    KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,    KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH, Quot   },
  {KC_EQL,  KC_LCTL, Caps,    KC_LALT, Tab,     Spc,     Bspc,    Del,     Left,    Down,    Up,      KC_RGHT},
},

// ...................................................................... Dvorak

  // ,-----------------------------------------------------------------------------------.
  // |   ~  |   "  |   ,  |   .  |   P  |   Y  |   F  |   G  |   C  |   R  |   L  |  \   |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |  Esc |   A  |   O  |   E  |   U  |   I  |   D  |   H  |   T  |   N  |   S  |Enter |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   -  |   ;  |   Q  |   J  |   K  |   X  |   B  |   M  |   W  |   V  |   Z  |  /   |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // |   =  | Ctrl |  GUI |  Alt |  Tab | Space| Bksp |  Del | Left | Down |  Up  |Right |
  // `-----------------------------------------------------------------------------------'

[_DVORAK] = {
  {Grv,     KC_QUOT, KC_COMM, KC_DOT,  KC_P,    KC_Y,    KC_F,    KC_G,    KC_C,    KC_R,    KC_L,    Bsls   },
  {Esc,     KC_A,    KC_O,    KC_E,    KC_U,    KC_I,    KC_D,    KC_H,    KC_T,    KC_N,    KC_S,    Ent    },
  {Mins,    KC_SCLN, KC_Q,    KC_J,    KC_K,    KC_X,    KC_B,    KC_M,    KC_W,    KC_V,    KC_Z,    Slsh   },
  {KC_EQL,  KC_LCTL, Caps,    KC_LALT, Tab,     Spc,     Bspc,    Del,     Left,    Down,    Up,      KC_RGHT},
},

// ...................................................................... Plover

  // ,-----------------------------------------------------------------------------------.
  // |      |   #  |   #  |   #  |   #  |   #  |   #  |   #  |   #  |   #  |   #  |   #  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |      |   S  |   T  |   P  |   H  |   *  |   *  |   F  |   P  |   L  |   T  |   D  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |      |   S  |   K  |   W  |   R  |   *  |   *  |   R  |   B  |   G  |   S  |   Z  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Exit |      |      |   A  |   O  |      |      |   E  |   U  |      |      |      |
  // `-----------------------------------------------------------------------------------'

[_PLOVER] = {
  {_______,  KC_1,    KC_1,    KC_1,    KC_1,    KC_1,    KC_1,    KC_1,    KC_1,    KC_1,    KC_1,    KC_1   },
  {_______,  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC},
  {_______,  KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, KC_QUOT},
  {PLOVEX,   _______, _______, KC_C,    KC_V,    _______, _______, KC_N,    KC_M,    _______, _______, _______},
 },

// ...................................................................... Adjust

  // ,-----------------------------------------------------------------------------------.
  // |  f() | Reset|Keytst|      |      |      |      |      |      |      |      |      |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |      |      |      |Aud on|Audoff|AGnorm|AGswap|Colemk|Qwerty|Dvorak|Plover|      |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |      |Voice-|Voice+|Mus on|Musoff|MIDIon|MIDIof|      |      |      |      |      |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // |      |      |      |      |      |      |      |      |      |      |      |      |
  // `-----------------------------------------------------------------------------------'

[_ADJUST] = {
  {___x___, RESET,   KEYTEST, _______, _______, _______, _______, _______, _______, _______, _______, _______},
  {_______, _______, _______, AU_ON,   AU_OFF,  AG_NORM, AG_SWAP, COLEMAK, QWERTY,  DVORAK,  PLOVER,  _______},
  {_______, MUV_DE,  MUV_IN,  MU_ON,   MU_OFF,  MI_ON,   MI_OFF,  _______, _______, _______, _______, _______},
  {_______, ___x___, KC_LGUI, ___x___, ___x___, ___x___, ___x___, ___x___, KC_LALT, KC_LGUI, KC_LCTL, _______},
},

// ....................................................... Silent Keys Echo Test

  // ,-----------------------------------------------------------------------------------.
  // |Adjust|      |      |      |      |      |      |      |      |      |      |      |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   0  |      |      |      |      |      |      |      |      |      |      |   1  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |      |      |      |      |      |      |      |      |      |      |      |      |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // |      |   2  |   3  |   4  |   5  |   6  |   7  |   8  |   9  |   A  |   B  |   C  |
  // `-----------------------------------------------------------------------------------'

[_KEYTEST] = {
  {ADJUST,  _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______},
  {KC_0,    _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, KC_1   },
  {_______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______},
  {_______, KC_2,    KC_3,    KC_4,    KC_5,    KC_6,    KC_7,    KC_8,    KC_9,    KC_A,    KC_B,    KC_C   },
},

// ................................................................ Number Layer

  // .-----------------------------------------------------------------------------------.
  // |Adjust|      |      |      |   ,  |      |   #  |   7  |   8  |   9  |   E  |   F  |
  // |-----------------------------------------------------------------------------------|
  // |      |   /  |   *  |   -  |   +  |      |   .  |   4  |   5  |   6  |   C  |   D  |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |   (  |   )  |      |   0  |   1  |   2  |   3  |   A  |   B  |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |  f() |      |      |      | Home | PgDn | PgUp |  End |
  // '-----------------------------------------------------------------------------------'

[_NUMBER] = {
  {ADJUST,  _______, _______, _______, KC_COMM, _______, KC_HASH, KC_7,    KC_8,    KC_9,    S(KC_E), S(KC_F)},
  {_______, KC_SLSH, KC_ASTR, KC_MINS, KC_PLUS, _______, KC_DOT,  KC_4,    KC_5,    KC_6,    S(KC_C), S(KC_D)},
  {_______, _______, _______, KC_LPRN, KC_RPRN, _______, KC_0,    KC_1,    KC_2,    KC_3,    S(KC_A), S(KC_B)},
  {_______, ___x___, KC_LGUI, ___x___, ___x___, ___x___, ___x___, ___x___, KC_HOME, KC_PGDN, KC_PGUP, KC_END },
},

// ................................................................ Symbol Layer

  // .-----------------------------------------------------------------------------------.
  // |  GUI |   &  |   *  |   .  |   {  |   }  |      |  F9  |  F10 |  F11 |  F12 |      |
  // |-----------------------------------------------------------------------------------|
  // | Ctrl |   $  |   %  |   ^  |   (  |   )  |      |  F5  |  F6  |  F7  |  F8  |      |
  // |-----------------------------------------------------------------------------------|
  // | Shift|   !  |   @  |   #  |   [  |   ]  |      |  F1  |  F2  |  F3  |  F4  |      |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |      |      |      |  f() |      |      |      |      |
  // '-----------------------------------------------------------------------------------'


[_SYMBOL] = {
  {KC_LGUI, KC_AMPR, KC_ASTR, KC_DOT,  Lcbr,    KC_RCBR, _______, KC_F9,   KC_F10,  KC_F11,  KC_F12,  _______},
  {KC_LCTL, KC_DLR,  KC_PERC, KC_CIRC, Lprn,    KC_RPRN, _______, KC_F5,   KC_F6,   KC_F7,   KC_F8,   _______},
  {KC_LSFT, KC_EXLM, KC_AT,   KC_HASH, Lbrc,    KC_RBRC, _______, KC_F1,   KC_F2,   KC_F3,   KC_F4,   _______},
  {_______, ___x___, KC_LGUI, ___x___, ___x___, ___x___, ___x___, ___x___, KC_LALT, KC_PGDN, KC_PGUP, _______},
},

// ............ ..................................................Navigation Pad

  // .-----------------------------------------------------------------------------------.
  // |      |      |      |      |      |      |      | Home |  Up  |  End |      |      |
  // |-----------------------------------------------------------------------------------|
  // |      |      |  GUI | Ctrl | Shift|      |      | Left | Down | Right|      |      |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      | Bksp |  Del |      |      | PgDn | PgUp |      |      |      |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |  f() |      |      |  f() |      |      |      |      |
  // '-----------------------------------------------------------------------------------'

[_NAVPAD] = {
  {_______, _______, _______, _______, _______, _______, _______, KC_HOME, KC_UP,   KC_END,  _______, _______},
  {_______, _______, KC_LGUI, KC_LCTL, KC_LSFT, _______, _______, KC_LEFT, KC_DOWN, KC_RGHT, _______, _______},
  {_______, _______, _______, KC_BSPC, KC_DEL,  _______, _______, KC_PGDN, KC_PGUP, _______, _______, _______},
  {_______, _______, _______, _______, ___x___, _______, _______, ___x___, _______, _______, _______, _______},
},

};

#ifdef AUDIO_ENABLE
#define CAPSLOCK_ON_SOUND   S__NOTE(_C7 ),ED_NOTE(_E7 )
#define CAPSLOCK_OFF_SOUND  S__NOTE(_E7 ),ED_NOTE(_C7 )
float tone_startup[][2]   = SONG (STARTUP_SOUND);
float tone_qwerty[][2]    = SONG (QWERTY_SOUND);
float tone_dvorak[][2]    = SONG (DVORAK_SOUND);
float tone_colemak[][2]   = SONG (COLEMAK_SOUND);
float tone_plover[][2]    = SONG (PLOVER_SOUND);
float tone_plover_gb[][2] = SONG (PLOVER_GOODBYE_SOUND);
float tone_caps_on[][2]   = SONG (CAPSLOCK_ON_SOUND);
float tone_caps_off[][2]  = SONG (CAPSLOCK_OFF_SOUND);
float music_scale[][2]    = SONG (MUSIC_SCALE_SOUND);
float tone_goodbye[][2]   = SONG (GOODBYE_SOUND);
#endif

void paren(qk_tap_dance_state_t *state, void *user_data) {
  if (state->count > 1) {
    register_code   (KC_LSFT);
    register_code   (KC_9);
    unregister_code (KC_9);
    register_code   (KC_0);
    unregister_code (KC_0);
    unregister_code (KC_LSFT);
  } else {
    register_code   (KC_LSFT);
    register_code   (KC_9);
    unregister_code (KC_9);
    unregister_code (KC_LSFT);
  }
  reset_tap_dance(state);
}

void brace(qk_tap_dance_state_t *state, void *user_data) {
  if (state->count > 1) {
    register_code   (KC_LBRC);
    unregister_code (KC_LBRC);
    register_code   (KC_RBRC);
    unregister_code (KC_RBRC);
  } else {
    register_code   (KC_LBRC);
    unregister_code (KC_LBRC);
  }
  reset_tap_dance(state);
}

void curly(qk_tap_dance_state_t *state, void *user_data) {
  if (state->count > 1) {
    register_code   (KC_LSFT);
    register_code   (KC_LBRC);
    unregister_code (KC_LBRC);
    register_code   (KC_RBRC);
    unregister_code (KC_RBRC);
    unregister_code (KC_LSFT);
  } else {
    register_code   (KC_LSFT);
    register_code   (KC_LBRC);
    unregister_code (KC_LBRC);
    unregister_code (KC_LSFT);
  }
  reset_tap_dance(state);
}

const qk_tap_dance_action_t tap_dance_actions[] = {
  [_LPRN] = ACTION_TAP_DANCE_FN (paren),
  [_LBRC] = ACTION_TAP_DANCE_FN (brace),
  [_LCBR] = ACTION_TAP_DANCE_FN (curly),
};

void persistant_default_layer_set(uint16_t default_layer) {
  eeconfig_update_default_layer(default_layer);
  default_layer_set(default_layer);
}

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  switch (keycode) {
    case Tab:
      if (record->event.pressed) {
        layer_on(_NUMBER);
        update_tri_layer(_NUMBER, _SYMBOL, _NAVPAD);
      } else {
        layer_off(_NUMBER);
        update_tri_layer(_NUMBER, _SYMBOL, _NAVPAD);
        // undo sticky modifiers 
        unregister_code (KC_LGUI);
        unregister_code (KC_LSFT);
        unregister_code (KC_LCTL);
      }
      // LT hack
      // return false;
      break;
    case Del:
      if (record->event.pressed) {
        layer_on(_SYMBOL);
        update_tri_layer(_NUMBER, _SYMBOL, _NAVPAD);
      } else {
        layer_off(_SYMBOL);
        update_tri_layer(_NUMBER, _SYMBOL, _NAVPAD);
        // undo sticky modifiers 
        unregister_code (KC_LGUI);
        unregister_code (KC_LSFT);
        unregister_code (KC_LCTL);
      }
      // LT hack
      // return false;
      break;
    case QWERTY:
      if (record->event.pressed) {
        #ifdef AUDIO_ENABLE
        PLAY_NOTE_ARRAY (tone_qwerty, false, 0);
        #endif
        persistant_default_layer_set(1UL<<_QWERTY);
      }
      return false;
      break;
    case COLEMAK:
      if (record->event.pressed) {
        #ifdef AUDIO_ENABLE
        PLAY_NOTE_ARRAY (tone_colemak, false, 0);
        #endif
        persistant_default_layer_set(1UL<<_COLEMAK);
      }
      return false;
      break;
    case DVORAK:
      if (record->event.pressed) {
        #ifdef AUDIO_ENABLE
        PLAY_NOTE_ARRAY (tone_dvorak, false, 0);
        #endif
        persistant_default_layer_set(1UL<<_DVORAK);
      }
      return false;
      break;
    case PLOVER:
      if (record->event.pressed) {
        #ifdef AUDIO_ENABLE
        stop_all_notes();
        PLAY_NOTE_ARRAY (tone_plover, false, 0);
        #endif
        layer_off       (_NUMBER);
        layer_off       (_SYMBOL);
        layer_off       (_NAVPAD);
        layer_off       (_ADJUST);
        layer_off       (_KEYTEST);
        layer_on        (_PLOVER);
        if (!eeconfig_is_enabled()) {
            eeconfig_init();
        }
        keymap_config.raw = eeconfig_read_keymap();
        keymap_config.nkro = 1;
        eeconfig_update_keymap(keymap_config.raw);
        // toggle plover application on, see herbstluftwm/config/appbinds
        register_code   (KC_LGUI);
        register_code   (KC_MINS);
        unregister_code (KC_MINS);
        // unregister modifiers as late as possible (convention)
        unregister_code (KC_LGUI);
      }
      return false;
      break;
    case PLOVEX:
      if (record->event.pressed) {
        #ifdef AUDIO_ENABLE
        PLAY_NOTE_ARRAY (tone_plover_gb, false, 0);
        #endif
        layer_off       (_PLOVER);
        // toggle plover application off, see herbstluftwm/config/appbinds
        register_code   (KC_LGUI);
        register_code   (KC_MINS);
        unregister_code (KC_MINS);
        // unregister modifiers as late as possible (convention)
        unregister_code (KC_LGUI);
      }
      return false;
      break;
    case KEYTEST:
      if (record->event.pressed) {
        #ifdef AUDIO_ENABLE
        PLAY_NOTE_ARRAY (music_scale, false, 0);
        #endif
        persistant_default_layer_set(1UL<<_KEYTEST);
      }
      return false;
      break;
  }
  return true;
}

void matrix_init_user(void) {
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
