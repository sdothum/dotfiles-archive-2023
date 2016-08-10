// This is the canonical layout file for the Quantum project. If you want to add another keyboard,
// this is the style you want to emulate.
//
// To flash planck firmware:
//   Reset keyboard or press hw reset button on base (hole)
//
//   cd qmk_firmware/keyboards/planck
//   sudo make KEYMAP=sdothum dfu
//
//   sudo make clean     (good practice before flashing)
//   make KEYMAP=sdothum (to compile check)
//
// Package requirements (for arch linux):
//   avr-gcc-atmel
//   avr-libc-atmel
//   dfu-programmer
//
// Notes:
//   New colemak-dh, f(number), keypad and navigation pad layers
//
//   Original qwerty and dvorak layers have normalized enter key, tap key esc
//   tap key right-shift and modifier row (consistent with colemak-dh)
//
//   Plover layer toggling has added binding to the herbstluftwm window manager
//   to enable/disable the necessary plover software
//
//   Adjust layer is available on keypad(=)+esc, see colemak layout
//
// Code:
//   This source is shamelessly based on the "default" planck layout
//   Non-indented #ifdef block structures are syntax highlighted in vim
//   c++ commenting style is used throughout
//
// Issues:
//   AG_SWAP appears to be on by default which swaps the left gui and alt keys
//   mapping "ctrl gui alt" as desired
//
//   Future git updates may undo this and require swapping of gui and alt keys
//   in the colemak-dh layer

#include "config.h"
#include "planck.h"
#include "action_layer.h"
#ifdef AUDIO_ENABLE
#include "audio.h"
#endif
#include "eeconfig.h"

extern keymap_config_t keymap_config;

enum planck_layers {
  _QWERTY = 0,
  _COLEMAK,
  _DVORAK,
  _PLOVER,
  _NUMBER,
  _KEYPAD,
  _NAVPAD,
  _ADJUST,
};

enum planck_keycodes {
  QWERTY = SAFE_RANGE,
  COLEMAK,
  DVORAK,
  PLOVER,
  PLOVEXIT,
};

enum function_id {
  _MIN = 0,
  _EQL,
  _TAB,
  _SPC,
  _BSP,
  _DEL,
  _LFT,
  _QOT,
  _SLS,
};

enum tap_dance {
  _PRN = 0,
  _BRC,
  _CBR,
};

#define _______ KC_TRNS
#define ZZZZZZZ KC_NO

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

// ...................................................................... Qwerty

  // ,-----------------------------------------------------------------------------------.
  // |   `  |   Q  |   W  |   E  |   R  |   T  |   Y  |   U  |   I  |   O  |   P  | Bksp |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |  Esc |   A  |   S  |   D  |   F  |   G  |   H  |   J  |   K  |   L  |   ;  |Enter |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   -  |   Z  |   X  |   C  |   V  |   B  |   N  |   M  |   ,  |   .  |   /  |  "   |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // |   =  | Ctrl | GUI  | Alt  | Tab  | Space| Bksp | Del  | Left | Down |  Up  |Right |
  // `-----------------------------------------------------------------------------------'

[_QWERTY] = {
  {KC_GRV,        KC_Q,    KC_W,           KC_E,    KC_R,    KC_T,    KC_Y,    KC_U,    KC_I,    KC_O,           KC_P,         KC_BSPC       },
  {GUI_T(KC_ESC), KC_A,    KC_S,           KC_D,    KC_F,    KC_G,    KC_H,    KC_J,    KC_K,    KC_L,           KC_SCLN,      CTL_T(KC_ENT) },
  {F(_MIN),       KC_Z,    KC_X,           KC_C,    KC_V,    KC_B,    KC_N,    KC_M,    KC_COMM, KC_DOT,         KC_SLSH,      F(_QOT)       },
  {F(_EQL),       KC_LCTL, GUI_T(KC_CAPS), KC_LGUI, F(_TAB), F(_SPC), F(_BSP), F(_DEL), F(_LFT), ALT_T(KC_DOWN), GUI_T(KC_UP), CTL_T(KC_RGHT)},
},

// ..................................................................... Colemak

  // ,-----------------------------------------------------------------------------------.
  // |   `  |   Q  |   W  |   F  |   P  |   B  |   J  |   L  |   U  |   Y  |   ;  |  \   |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |  Esc |   A  |   R  |   S  |   T  |   G  |   M  |   N  |   E  |   I  |   O  |Enter |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   -  |   Z  |   X  |   C  |   D  |   V  |   K  |   H  |   ,  |   .  |   /  |  "   |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // |   =  | Ctrl | GUI  | Alt  | Tab  | Space| Bksp | Del  | Left | Down |  Up  |Right |
  // `-----------------------------------------------------------------------------------'

[_COLEMAK] = {
  {KC_GRV,        KC_Q,    KC_W,           KC_F,    KC_P,    KC_B,    KC_J,    KC_L,    KC_U,    KC_Y,           KC_SCLN,      KC_BSLS       },
  {GUI_T(KC_ESC), KC_A,    KC_R,           KC_S,    KC_T,    KC_G,    KC_M,    KC_N,    KC_E,    KC_I,           KC_O,         CTL_T(KC_ENT) },
  {F(_MIN),       KC_Z,    KC_X,           KC_C,    KC_D,    KC_V,    KC_K,    KC_H,    KC_COMM, KC_DOT,         KC_SLSH,      F(_QOT)       },
  {F(_EQL),       KC_LCTL, GUI_T(KC_CAPS), KC_LGUI, F(_TAB), F(_SPC), F(_BSP), F(_DEL), F(_LFT), ALT_T(KC_DOWN), GUI_T(KC_UP), CTL_T(KC_RGHT)},
},

// ...................................................................... Dvorak

  // ,-----------------------------------------------------------------------------------.
  // |   `  |   "  |   ,  |   .  |   P  |   Y  |   F  |   G  |   C  |   R  |   L  | Bksp |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |  Esc |   A  |   O  |   E  |   U  |   I  |   D  |   H  |   T  |   N  |   S  |Enter |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   -  |   ;  |   Q  |   J  |   K  |   X  |   B  |   M  |   W  |   V  |   Z  |  /   |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // |   =  | Ctrl | GUI  | Alt  | Tab  | Space| Bksp | Del  | Left | Down |  Up  |Right |
  // `-----------------------------------------------------------------------------------'

[_DVORAK] = {
  {KC_GRV,        KC_QUOT, KC_COMM,        KC_DOT,  KC_P,    KC_Y,    KC_F,    KC_G,    KC_C,    KC_R,           KC_L,         KC_BSPC       },
  {GUI_T(KC_ESC), KC_A,    KC_O,           KC_E,    KC_U,    KC_I,    KC_D,    KC_H,    KC_T,    KC_N,           KC_S,         CTL_T(KC_ENT) },
  {F(_MIN),       KC_SCLN, KC_Q,           KC_J,    KC_K,    KC_X,    KC_B,    KC_M,    KC_W,    KC_V,           KC_Z,         F(_SLS)       },
  {F(_EQL),       KC_LCTL, GUI_T(KC_CAPS), KC_LGUI, F(_TAB), F(_SPC), F(_BSP), F(_DEL), F(_LFT), ALT_T(KC_DOWN), GUI_T(KC_UP), CTL_T(KC_RGHT)},
},

// ...................................................................... Plover

  // ,-----------------------------------------------------------------------------------.
  // |   #  |   #  |   #  |   #  |   #  |   #  |   #  |   #  |   #  |   #  |   #  |   #  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |      |   S  |   T  |   P  |   H  |   *  |   *  |   F  |   P  |   L  |   T  |   D  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |      |   S  |   K  |   W  |   R  |   *  |   *  |   R  |   B  |   G  |   S  |   Z  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Exit |      |      |   A  |   O  |      |      |   E  |   U  |      |      |      |
  // `-----------------------------------------------------------------------------------'

[_PLOVER] = {
  {KC_1,     KC_1,    KC_1,    KC_1,   KC_1,    KC_1,    KC_1,    KC_1,    KC_1,    KC_1,    KC_1,    KC_1   },
  {ZZZZZZZ,  KC_Q,    KC_W,    KC_E,   KC_R,    KC_T,    KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC},
  {ZZZZZZZ,  KC_A,    KC_S,    KC_D,   KC_F,    KC_G,    KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, KC_QUOT},
  {PLOVEXIT, ZZZZZZZ, ZZZZZZZ, KC_C,   KC_V,    ZZZZZZZ, ZZZZZZZ, KC_N,    KC_M,    ZZZZZZZ, ZZZZZZZ, ZZZZZZZ},
 },

// ...................................................................... Adjust

  // ,-----------------------------------------------------------------------------------.
  // |      | Reset|      |      |      |      |      |      |      |      |      |  Del |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |  f() |      |      |Aud on|Audoff|AGnorm|AGswap|Qwerty|Colemk|Dvorak|Plover|      |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |      |Voice-|Voice+|Mus on|Musoff|MIDIon|MIDIof|      |      |      |      |      |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // |      |      |      |      |      |      |      |      | Home | PgDn | PgUp |  End |
  // `-----------------------------------------------------------------------------------'

[_ADJUST] = {
  {_______, RESET,   _______, _______, _______, _______, _______, _______, _______, _______, _______, KC_DEL },
  {_______, _______, _______, AU_ON,   AU_OFF,  AG_NORM, AG_SWAP, QWERTY,  COLEMAK, DVORAK,  PLOVER,  _______},
  {_______, MUV_DE,  MUV_IN,  MU_ON,   MU_OFF,  MI_ON,   MI_OFF,  _______, _______, _______, _______, _______},
  {_______, _______, _______, _______, _______, _______, _______, _______, KC_HOME, KC_PGDN, KC_PGUP, KC_END },
},

// .................................................. Number / symbol / function

  // .-----------------------------------------------------------------------------------.
  // |   }  |   1  |   2  |   3  |   4  |   5  |   6  |   7  |   8  |   9  |   0  |   ]  |
  // |-----------------------------------------------------------------------------------|
  // |   {  |   !  |   @  |   #  |   $  |   %  |   ^  |   &  |   *  |   (  |   )  |   [  |
  // |-----------------------------------------------------------------------------------|
  // |  F12 |  F1  |  F2  |  F3  |  F4  |  F5  |  F6  |  F7  |  F8  |  F9  |  F10 |  F11 |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |  f() |      |      |  f() | Home | PgDn | PgUp |  End |
  // '-----------------------------------------------------------------------------------'

[_NUMBER] = {
  {KC_RCBR,  KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    KC_6,    KC_7,    KC_8,    KC_9,     KC_0,    KC_RBRC },
  {TD(_CBR), KC_EXLM, KC_AT,   KC_HASH, KC_DLR,  KC_PERC, KC_CIRC, KC_AMPR, KC_ASTR, TD(_PRN), KC_RPRN, TD(_BRC)},
  {KC_F12,   KC_F1,   KC_F2,   KC_F3,   KC_F4,   KC_F5,   KC_F6,   KC_F7,   KC_F8,   KC_F9,    KC_F10,  KC_F11  },
  {_______,  _______, _______, _______, _______, _______, _______, _______, KC_HOME, KC_PGDN,  KC_PGUP, KC_END  },
},

// .......................................................... Hexadecimal Keypad

  // .-----------------------------------------------------------------------------------.
  // |      |      |      |      |      |      |   E  |   F  |   7  |   8  |   9  |   -  |
  // |-----------------------------------------------------------------------------------|
  // |Adjust|      |      |      |      |      |   C  |   D  |   4  |   5  |   6  |   +  |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |      |      |   A  |   B  |   1  |   2  |   3  |   =  |
  // |-----------------------------------------------------------------------------------|
  // |  f() |      |      |      |      |      |   (  |   )  |   0  |   .  |   /  |   *  |
  // '-----------------------------------------------------------------------------------'

[_KEYPAD] = {
  {_______,     _______, _______, _______, _______, _______, S(KC_E), S(KC_F), KC_7, KC_8,   KC_9,    KC_MINS},
  {MO(_ADJUST), _______, _______, _______, _______, _______, S(KC_C), S(KC_D), KC_4, KC_5,   KC_6,    KC_PLUS},
  {_______,     _______, _______, _______, _______, _______, S(KC_A), S(KC_B), KC_1, KC_2,   KC_3,    KC_EQL },
  {_______,     _______, _______, _______, _______, _______, KC_LPRN, KC_RPRN, KC_0, KC_DOT, KC_SLSH, KC_ASTR},
},

// ............ ..................................................Navigation Pad

  // .-----------------------------------------------------------------------------------.
  // |      |      |      |      |      |      |      |      | Home |  Up  |  End | PgUp |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |      |      |      |      | Left | Down | Right| PgDn |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |      |      |      |      |      |      |      |      |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |      |      |      |      |  f() |      |      |      |
  // '-----------------------------------------------------------------------------------'

[_NAVPAD] = {
  {_______, _______, _______, _______, _______, _______, _______, _______, KC_HOME, KC_UP,   KC_END,  KC_PGUP},
  {_______, _______, _______, _______, _______, _______, _______, _______, KC_LEFT, KC_DOWN, KC_RGHT, KC_PGDN},
  {_______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______},
  {_______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______},
},

};

#ifdef AUDIO_ENABLE
float tone_startup[][2]   = SONG (STARTUP_SOUND       );
float tone_qwerty[][2]    = SONG (QWERTY_SOUND        );
float tone_dvorak[][2]    = SONG (DVORAK_SOUND        );
float tone_colemak[][2]   = SONG (COLEMAK_SOUND       );
float tone_plover[][2]    = SONG (PLOVER_SOUND        );
float tone_plover_gb[][2] = SONG (PLOVER_GOODBYE_SOUND);
float tone_caps_on[][2]   = SONG (CAPS_LOCK_ON_SOUND  );
float tone_caps_off[][2]  = SONG (CAPS_LOCK_OFF_SOUND );
float music_scale[][2]    = SONG (MUSIC_SCALE_SOUND   );
float tone_goodbye[][2]   = SONG (GOODBYE_SOUND       );
#endif

const uint16_t PROGMEM fn_actions[] = {
  [_MIN] = ACTION_MODS_TAP_KEY  (MOD_LSFT, KC_MINS),
  [_EQL] = ACTION_LAYER_TAP_KEY (_KEYPAD,  KC_EQL ),
  [_TAB] = ACTION_LAYER_TAP_KEY (_NUMBER,  KC_TAB ),
  [_SPC] = ACTION_MODS_TAP_KEY  (MOD_LSFT, KC_SPC ),
  [_BSP] = ACTION_MODS_TAP_KEY  (MOD_RSFT, KC_BSPC),
  [_DEL] = ACTION_LAYER_TAP_KEY (_NUMBER,  KC_DEL ),
  [_LFT] = ACTION_LAYER_TAP_KEY (_NAVPAD,  KC_LEFT),
  [_QOT] = ACTION_MODS_TAP_KEY  (MOD_RSFT, KC_QUOT),
  [_SLS] = ACTION_MODS_TAP_KEY  (MOD_RSFT, KC_SLSH),
};

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
  [_PRN] = ACTION_TAP_DANCE_FN (paren),
  [_BRC] = ACTION_TAP_DANCE_FN (brace),
  [_CBR] = ACTION_TAP_DANCE_FN (curly),
};

void persistant_default_layer_set(uint16_t default_layer) {
  eeconfig_update_default_layer(default_layer);
  default_layer_set(default_layer);
}

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  switch (keycode) {
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
        layer_off       (_KEYPAD);
        layer_off       (_NAVPAD);
        layer_off       (_ADJUST);
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
    case PLOVEXIT:
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
  }
  return true;
}

void matrix_init_user(void) {
  #ifdef AUDIO_ENABLE
  startup_user();
  #endif
}

#ifdef AUDIO_ENABLE
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
