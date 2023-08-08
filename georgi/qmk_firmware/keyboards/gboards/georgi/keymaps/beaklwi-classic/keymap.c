#include QMK_KEYBOARD_H
#include "keymap_steno.h"

#define H_TOP1 ((HASH_TYPE) 1 << 0)
#define H_TOP2 ((HASH_TYPE) 1 << 1)
#define H_TOP3 ((HASH_TYPE) 1 << 2)
#define H_TOP4 ((HASH_TYPE) 1 << 3)
#define H_TOP5 ((HASH_TYPE) 1 << 4)
#define H_TOP6 ((HASH_TYPE) 1 << 5)
#define H_TOP7 ((HASH_TYPE) 1 << 6)
#define H_TOP8 ((HASH_TYPE) 1 << 7)
#define H_TOP9 ((HASH_TYPE) 1 << 8)
#define H_TOP10 ((HASH_TYPE) 1 << 9)
#define H_TOP11 ((HASH_TYPE) 1 << 10)
#define H_TOP12 ((HASH_TYPE) 1 << 11)
#define H_BOT1 ((HASH_TYPE) 1 << 12)
#define H_BOT2 ((HASH_TYPE) 1 << 13)
#define H_BOT3 ((HASH_TYPE) 1 << 14)
#define H_BOT4 ((HASH_TYPE) 1 << 15)
#define H_BOT5 ((HASH_TYPE) 1 << 16)
#define H_BOT6 ((HASH_TYPE) 1 << 17)
#define H_BOT7 ((HASH_TYPE) 1 << 18)
#define H_BOT8 ((HASH_TYPE) 1 << 19)
#define H_BOT9 ((HASH_TYPE) 1 << 20)
#define H_BOT10 ((HASH_TYPE) 1 << 21)
#define H_BOT11 ((HASH_TYPE) 1 << 22)
#define H_BOT12 ((HASH_TYPE) 1 << 23)
#define H_THU1 ((HASH_TYPE) 1 << 24)
#define H_THU2 ((HASH_TYPE) 1 << 25)
#define H_THU3 ((HASH_TYPE) 1 << 26)
#define H_THU4 ((HASH_TYPE) 1 << 27)
#define H_THU5 ((HASH_TYPE) 1 << 28)
#define H_THU6 ((HASH_TYPE) 1 << 29)

enum internal_keycodes {
    TOP1 = SAFE_RANGE,
    TOP2, TOP3, TOP4, TOP5, TOP6, TOP7, TOP8, TOP9, TOP10, TOP11, TOP12, BOT1, BOT2, BOT3, BOT4, BOT5, BOT6, BOT7, BOT8, BOT9, BOT10, BOT11, BOT12, THU1, THU2, THU3, THU4, THU5, THU6,
    FIRST_INTERNAL_KEYCODE = TOP1,
    LAST_INTERNAL_KEYCODE = THU6
};

enum pseudolayers {
    ALWAYS_ON, BEAKL, CAPS, REGEX, SYM, SYMBOL, NUM, FNC, NAV, MOUSE, STENO, PLOVER
};

#define CHORD_TIMEOUT 200
#define DANCE_TIMEOUT 200
#define LEADER_TIMEOUT 750
#define TAP_TIMEOUT 50
#define LONG_PRESS_MULTIPLIER 3
#define DYNAMIC_MACRO_MAX_LENGTH 20
#define COMMAND_MAX_LENGTH 5
#define STRING_MAX_LENGTH 16
#define LEADER_MAX_LENGTH 5
#define HASH_TYPE uint32_t
#define NUMBER_OF_KEYS 30
#define DEFAULT_PSEUDOLAYER BEAKL

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
    [0] = LAYOUT_georgi(TOP1, TOP2, TOP3, TOP4, TOP5, TOP6, TOP7, TOP8, TOP9, TOP10, TOP11, TOP12, BOT1, BOT2, BOT3, BOT4, BOT5, BOT6, BOT7, BOT8, BOT9, BOT10, BOT11, BOT12, THU1, THU2, THU3, THU4, THU5, THU6),
};
size_t keymapsCount = 1;

uint8_t keycodes_buffer_array[] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

uint8_t command_buffer[] = {
    0, 0, 0, 0, 0
};

uint16_t leader_buffer[] = {
    0, 0, 0, 0, 0
};

uint8_t dynamic_macro_buffer[] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

enum chord_states {
    IDLE,
    READY,
    ACTIVATED,
    DEACTIVATED,
    PRESS_FROM_ACTIVE,
    FINISHED_FROM_ACTIVE,
    IDLE_IN_DANCE,
    READY_IN_DANCE,
    FINISHED,
    LOCKED,
    READY_LOCKED,
    RESTART,
    IN_ONE_SHOT
};

struct Chord {
    uint32_t keycodes_hash;
    uint8_t pseudolayer;
    uint8_t* state;
    uint8_t* counter;
    uint16_t value1;
    uint8_t value2;
    void (*function) (const struct Chord*);
};

uint8_t current_pseudolayer = DEFAULT_PSEUDOLAYER;
bool lock_next = false;
uint16_t chord_timer = 0;
uint16_t dance_timer = 0;
bool autoshift_mode = true;
uint8_t keycode_index = 0;
uint8_t command_mode = 0;
uint8_t command_ind = 0;
bool in_leader_mode = false;
uint8_t leader_ind = 0;
uint16_t leader_timer = 0;
uint8_t dynamic_macro_mode = false;
uint8_t dynamic_macro_ind = 0;
bool a_key_went_through = false;
struct Chord* last_chord = NULL;

bool handle_US_ANSI_shifted_keys(int16_t keycode, bool in) {
    bool is_US_ANSI_shifted = true;
    
    int16_t regular_keycode = KC_NO;
    switch (keycode) {
        case KC_TILDE:
            regular_keycode = KC_GRAVE;
            break;
        case KC_EXCLAIM:
            regular_keycode = KC_1;
            break;
        case KC_AT:
            regular_keycode = KC_2;
            break;
        case KC_HASH:
            regular_keycode = KC_3;
            break;
        case KC_DOLLAR:
            regular_keycode = KC_4;
            break;
        case KC_PERCENT:
            regular_keycode = KC_5;
            break;
        case KC_CIRCUMFLEX:
            regular_keycode = KC_6;
            break;
        case KC_AMPERSAND:
            regular_keycode = KC_7;
            break;
        case KC_ASTERISK:
            regular_keycode = KC_8;
            break;
        case KC_LEFT_PAREN:
            regular_keycode = KC_9;
            break;
        case KC_RIGHT_PAREN:
            regular_keycode = KC_0;
            break;
        case KC_UNDERSCORE:
            regular_keycode = KC_MINUS;
            break;
        case KC_PLUS:
            regular_keycode = KC_EQUAL;
            break;
        case KC_LEFT_CURLY_BRACE:
            regular_keycode = KC_LBRACKET;
            break;
        case KC_RIGHT_CURLY_BRACE:
            regular_keycode = KC_RBRACKET;
            break;
        case KC_PIPE:
            regular_keycode = KC_BSLASH;
            break;
        case KC_COLON:
            regular_keycode = KC_SCOLON;
            break;
        case KC_DOUBLE_QUOTE:
            regular_keycode = KC_QUOTE;
            break;
        case KC_LEFT_ANGLE_BRACKET:
            regular_keycode = KC_COMMA;
            break;
        case KC_RIGHT_ANGLE_BRACKET:
            regular_keycode = KC_DOT;
            break;
        case KC_QUESTION:
            regular_keycode = KC_SLASH;
            break;
        default:
            is_US_ANSI_shifted = false;
    }
    if (is_US_ANSI_shifted) {
        if (in) {
            register_code(KC_LSFT);
            register_code(regular_keycode);
        } else {
            unregister_code(regular_keycode);
            unregister_code(KC_LSFT);
        }
    }
    return is_US_ANSI_shifted;
}

void key_in(int16_t keycode) {
    if (command_mode == 1 && command_ind < COMMAND_MAX_LENGTH) {
        command_buffer[command_ind] = keycode;
        command_ind++;
        a_key_went_through = true;
    } else if (in_leader_mode && leader_ind < LEADER_MAX_LENGTH) {
        leader_buffer[leader_ind] = keycode;
        leader_ind++;
        a_key_went_through = true;
    } else if (dynamic_macro_mode && dynamic_macro_ind < DYNAMIC_MACRO_MAX_LENGTH) {
        dynamic_macro_buffer[dynamic_macro_ind] = keycode;
        dynamic_macro_ind++;
        a_key_went_through = true;
    } else {
        if (!handle_US_ANSI_shifted_keys(keycode, true)) {
            register_code(keycode);
        }
        send_keyboard_report();
        a_key_went_through = true;
    }
}

void key_out(int16_t keycode) {
    if (command_mode == 0) {
        if (!handle_US_ANSI_shifted_keys(keycode, false)) {
            if (command_mode == 0 && in_leader_mode == false && dynamic_macro_mode == false) {
                unregister_code(keycode);
            }
        }
        send_keyboard_report();
    }
}

void tap_key(int16_t keycode) {
    key_in(keycode);
    wait_ms(TAP_TIMEOUT);
    key_out(keycode);
}void cap(const struct Chord* self) {
  switch (*self->state) {
  case ACTIVATED:
    key_in(self->value1);
    key_in(self->value2);
    break;
  case DEACTIVATED:
    key_out(self->value1);
    key_out(self->value2);
    break;
  case FINISHED:
  case FINISHED_FROM_ACTIVE:
    //O(CAPS);
    break;
  case RESTART:
    key_out(self->value1);
    key_out(self->value2);
    break;
  default:
    break;
  }
}


const char string_0 [] PROGMEM = "::";
const char string_1 [] PROGMEM = "~/";
const char string_2 [] PROGMEM = ".*";
const char string_3 [] PROGMEM = "<-";
const char string_4 [] PROGMEM = "->";
const char string_5 [] PROGMEM = "=~";
const char string_6 [] PROGMEM = "<-";
const char string_7 [] PROGMEM = "->";
const char string_8 [] PROGMEM = "=~";
const char string_9 [] PROGMEM = "SECRET";
const char string_10 [] PROGMEM = "PUBLIC";

const char * const strings[] PROGMEM = {
    string_0, string_1, string_2, string_3, string_4, string_5, string_6, string_7, string_8, string_9, string_10
};

void single_dance(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(self->value1);
            break;
        case DEACTIVATED:
            key_out(self->value1);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(self->value1);
            break;
        default:
            break;
    }
}

void key_layer_dance(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            current_pseudolayer = self->value2;
            a_key_went_through = false;
            break;
        case DEACTIVATED:
        case RESTART:
            if (!a_key_went_through) {
                tap_key(self->value1);
            }
            current_pseudolayer = self->pseudolayer;
            *self->state = IDLE; // does not have effect if the state was RESTART
            break;
        default:
            break;
    }
}

void key_mod_dance(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(self->value2);
            a_key_went_through = false;
            break;
        case DEACTIVATED:
        case RESTART:
            key_out(self->value2);
            if (!a_key_went_through) {
                tap_key(self->value1);
            }
            *self->state = IDLE; // does not have effect if the state was RESTART
            break;
        default:
            break;
    }
}

void key_key_dance(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            break;
        case DEACTIVATED:
            tap_key(self->value1);
            *self->state = IDLE;
            break;
        case FINISHED:
        case PRESS_FROM_ACTIVE:
            key_in(self->value2);
            break;
        case RESTART:
            key_out(self->value2);
            break;
        default:
            break;
    }
}

void autoshift_dance_impl(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            *self->counter = 0;
            break;
        case DEACTIVATED:
        case RESTART:
            tap_key(self->value1);
            *self->state = IDLE;
            break;
        case FINISHED_FROM_ACTIVE:
            if (*self->counter == (LONG_PRESS_MULTIPLIER - 2)) {
                key_in(KC_LSFT);
                tap_key(self->value1);
                key_out(KC_LSFT);
                *self->state = IDLE;
                // the skip to IDLE is usually just a lag optimization,
                // in this case it has a logic function, on a short
                // press (still longer than a tap) the key does not get shifted
            } else {
                *self->counter += 1;
                *self->state = PRESS_FROM_ACTIVE;
                dance_timer = timer_read();
            }
            break;
        default:
            break;
    }
}

void autoshift_dance(const struct Chord* self) {
    if (autoshift_mode) {
        autoshift_dance_impl(self);
    } else {
        single_dance(self);
    }
}

void autoshift_toggle(const struct Chord* self){
    if (*self->state == ACTIVATED) {
        autoshift_mode = !autoshift_mode;
        *self->state = IDLE;
    }
}

void temp_pseudolayer(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            current_pseudolayer = self->value1;
            break;
        case DEACTIVATED:
            current_pseudolayer = self->pseudolayer;
            *self->state = IDLE;
            break;
        case RESTART:
            current_pseudolayer = self->pseudolayer;
            break;
        default:
            break;
    }
}

void temp_pseudolayer_alt(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            current_pseudolayer = self->value1;
            break;
        case DEACTIVATED:
            current_pseudolayer = self->value2;
            *self->state = IDLE;
            break;
        case RESTART:
            current_pseudolayer = self->value2;
            break;
        default:
            break;
    }
}

void perm_pseudolayer(const struct Chord* self) {
    if (*self->state == ACTIVATED) {
        current_pseudolayer = self->value1;
        *self->state = IDLE;
    }
}

void switch_layer(const struct Chord* self) {
    if (*self->state == ACTIVATED) {
        layer_move(self->value1);
        *self->state = IDLE;
    }
}

void lock(const struct Chord* self) {
    if (*self->state == ACTIVATED) {
        lock_next = true;
        *self->state = IDLE;
    }
}

void one_shot_key(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            break;
        case DEACTIVATED:
            key_in(self->value1);
            *self->state = IN_ONE_SHOT;
            break;
        case FINISHED:
        case PRESS_FROM_ACTIVE:
            key_in(self->value1);
            a_key_went_through = false;
            break;
        case RESTART:
            if (a_key_went_through) {
                key_out(self->value1);
            } else {
                *self->state = IN_ONE_SHOT;
            }
        default:
            break;
    }
}

void one_shot_layer(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            break;
        case DEACTIVATED:
            current_pseudolayer = self->value1;
            *self->state = IN_ONE_SHOT;
            break;
        case FINISHED:
        case PRESS_FROM_ACTIVE:
            current_pseudolayer = self->value1;
            a_key_went_through = false;
            break;
        case RESTART:
            if (a_key_went_through) {
                current_pseudolayer = self->pseudolayer;
            } else {
                *self->state = IN_ONE_SHOT;
            }
        default:
            break;
    }
}

void command(const struct Chord* self) {
    if (*self->state == ACTIVATED) {
        command_mode++;
        *self->state = IDLE;
    }
}

bool identical(uint16_t* buffer1, uint16_t* buffer2) {
    bool same = true;
    for (int i = 0; i < LEADER_MAX_LENGTH; i++) {
        same = same && (buffer1[i] == buffer2[i]);
    }
    return same;
}

void leader(const struct Chord* self) {
    if (*self->state == ACTIVATED) {
        in_leader_mode = true;
        *self->state = IDLE;
    }
}

void dynamic_macro_record(const struct Chord* self) {
    if (*self->state == ACTIVATED) {
        for (int i = 0; i < DYNAMIC_MACRO_MAX_LENGTH; i++) {
            dynamic_macro_buffer[i] = 0;
        }
        dynamic_macro_mode = true;
        *self->state = IDLE;
    }
}

void dynamic_macro_next(const struct Chord* self) {
    if (*self->state == ACTIVATED) {
        if (dynamic_macro_mode && dynamic_macro_ind < DYNAMIC_MACRO_MAX_LENGTH) {
            dynamic_macro_buffer[dynamic_macro_ind] = 0;
            dynamic_macro_ind++;
        }
        *self->state = IDLE;
    }
}

void dynamic_macro_end(const struct Chord* self) {
    if (*self->state == ACTIVATED) {
        if (dynamic_macro_mode) {
            dynamic_macro_mode = false;
        }
        *self->state = IDLE;
    }
}

void dynamic_macro_play(const struct Chord* self) {
    if (*self->state == ACTIVATED) {
        int ind_start = 0;
        while (ind_start < DYNAMIC_MACRO_MAX_LENGTH) {
            for (int i = ind_start; i < DYNAMIC_MACRO_MAX_LENGTH; i++) {
                if (dynamic_macro_buffer[i] == 0) {
                    break;
                }
                register_code(dynamic_macro_buffer[i]);
            }\
            send_keyboard_report();
            wait_ms(TAP_TIMEOUT);
            for (int i = ind_start; i < DYNAMIC_MACRO_MAX_LENGTH; i++) {
                if (dynamic_macro_buffer[i] == 0) {
                    ind_start = i + 1;
                    break;
                }
                unregister_code(dynamic_macro_buffer[i]);
            }
            send_keyboard_report();
        }
        *self->state = IDLE;
    }
}

void string_in(const struct Chord* self) {
    if (*self->state == ACTIVATED) {
        char buffer[STRING_MAX_LENGTH];
        strcpy_P(buffer, (char*)pgm_read_word(&(strings[self->value1])));
        send_string(buffer);
    }
}

void clear(const struct Chord* self);

void reset_keyboard_kb(void){
#ifdef WATCHDOG_ENABLE
    MCUSR = 0;
    wdt_disable();
    wdt_reset();
#endif
    reset_keyboard();
}

void reset(const struct Chord* self) {
    if (*self->state == ACTIVATED) {
        reset_keyboard_kb();
    }
}

uint8_t state_0 = IDLE;
const struct Chord chord_0 PROGMEM = {H_TOP1 + H_TOP12 + H_BOT1 + H_BOT12, ALWAYS_ON, &state_0, NULL, 0, 0, clear};
uint8_t state_1 = IDLE;
const struct Chord chord_1 PROGMEM = {H_TOP6 + H_TOP7 + H_BOT6 + H_BOT7, ALWAYS_ON, &state_1, NULL, 0, 0, command};
uint8_t state_2 = IDLE;
const struct Chord chord_2 PROGMEM = {H_TOP1, BEAKL, &state_2, NULL, FNC, 0, perm_pseudolayer};
uint8_t state_3 = IDLE;
const struct Chord chord_3 PROGMEM = {H_TOP2, BEAKL, &state_3, NULL, KC_COLON, 0, single_dance};
uint8_t state_4 = IDLE;
const struct Chord chord_4 PROGMEM = {H_TOP3, BEAKL, &state_4, NULL, KC_Y, 0, single_dance};
uint8_t state_5 = IDLE;
const struct Chord chord_5 PROGMEM = {H_TOP4, BEAKL, &state_5, NULL, KC_O, 0, single_dance};
uint8_t state_6 = IDLE;
const struct Chord chord_6 PROGMEM = {H_TOP5, BEAKL, &state_6, NULL, KC_U, 0, single_dance};
uint8_t state_7 = IDLE;
const struct Chord chord_7 PROGMEM = {H_TOP6, BEAKL, &state_7, NULL, KC_MINUS, 0, single_dance};
uint8_t state_8 = IDLE;
const struct Chord chord_8 PROGMEM = {H_TOP7, BEAKL, &state_8, NULL, KC_G, 0, single_dance};
uint8_t state_9 = IDLE;
const struct Chord chord_9 PROGMEM = {H_TOP8, BEAKL, &state_9, NULL, KC_D, 0, single_dance};
uint8_t state_10 = IDLE;
const struct Chord chord_10 PROGMEM = {H_TOP9, BEAKL, &state_10, NULL, KC_N, 0, single_dance};
uint8_t state_11 = IDLE;
const struct Chord chord_11 PROGMEM = {H_TOP10, BEAKL, &state_11, NULL, KC_M, 0, single_dance};
uint8_t state_12 = IDLE;
const struct Chord chord_12 PROGMEM = {H_TOP11, BEAKL, &state_12, NULL, KC_X, 0, single_dance};
uint8_t state_13 = IDLE;
const struct Chord chord_13 PROGMEM = {H_TOP12, BEAKL, &state_13, NULL, SYMBOL, 0, perm_pseudolayer};
uint8_t state_14 = IDLE;
const struct Chord chord_14 PROGMEM = {H_TOP1 + H_BOT1, BEAKL, &state_14, NULL, PLOVER, 0, perm_pseudolayer};
uint8_t state_15 = IDLE;
uint8_t counter_15 = 0;
const struct Chord chord_15 PROGMEM = {H_TOP2 + H_BOT2, BEAKL, &state_15, &counter_15, KC_Q, KC_LGUI, key_key_dance};
uint8_t state_16 = IDLE;
const struct Chord chord_16 PROGMEM = {H_TOP3 + H_BOT3, BEAKL, &state_16, NULL, KC_H, KC_LCTL, key_mod_dance};
uint8_t state_17 = IDLE;
const struct Chord chord_17 PROGMEM = {H_TOP4 + H_BOT4, BEAKL, &state_17, NULL, KC_E, KC_LALT, key_mod_dance};
uint8_t state_18 = IDLE;
uint8_t counter_18 = 0;
const struct Chord chord_18 PROGMEM = {H_TOP5 + H_BOT5, BEAKL, &state_18, &counter_18, KC_A, KC_LSFT, key_key_dance};
uint8_t state_19 = IDLE;
const struct Chord chord_19 PROGMEM = {H_TOP6 + H_BOT6, BEAKL, &state_19, NULL, KC_W, 0, single_dance};
uint8_t state_20 = IDLE;
const struct Chord chord_20 PROGMEM = {H_TOP7 + H_BOT7, BEAKL, &state_20, NULL, KC_C, 0, single_dance};
uint8_t state_21 = IDLE;
uint8_t counter_21 = 0;
const struct Chord chord_21 PROGMEM = {H_TOP8 + H_BOT8, BEAKL, &state_21, &counter_21, KC_T, KC_RSFT, key_key_dance};
uint8_t state_22 = IDLE;
const struct Chord chord_22 PROGMEM = {H_TOP9 + H_BOT9, BEAKL, &state_22, NULL, KC_R, KC_RALT, key_mod_dance};
uint8_t state_23 = IDLE;
const struct Chord chord_23 PROGMEM = {H_TOP10 + H_BOT10, BEAKL, &state_23, NULL, KC_S, KC_RCTL, key_mod_dance};
uint8_t state_24 = IDLE;
uint8_t counter_24 = 0;
const struct Chord chord_24 PROGMEM = {H_TOP11 + H_BOT11, BEAKL, &state_24, &counter_24, KC_Z, KC_RGUI, key_key_dance};
uint8_t state_25 = IDLE;
const struct Chord chord_25 PROGMEM = {H_TOP12 + H_BOT12, BEAKL, &state_25, NULL, NAV, 0, perm_pseudolayer};
uint8_t state_26 = IDLE;
const struct Chord chord_26 PROGMEM = {H_BOT1, BEAKL, &state_26, NULL, KC_CAPS, 0, single_dance};
uint8_t state_27 = IDLE;
const struct Chord chord_27 PROGMEM = {H_BOT2, BEAKL, &state_27, NULL, KC_J, 0, single_dance};
uint8_t state_28 = IDLE;
const struct Chord chord_28 PROGMEM = {H_BOT3, BEAKL, &state_28, NULL, KC_COMMA, 0, single_dance};
uint8_t state_29 = IDLE;
const struct Chord chord_29 PROGMEM = {H_BOT4, BEAKL, &state_29, NULL, KC_DOT, 0, single_dance};
uint8_t state_30 = IDLE;
const struct Chord chord_30 PROGMEM = {H_BOT5, BEAKL, &state_30, NULL, KC_K, 0, single_dance};
uint8_t state_31 = IDLE;
const struct Chord chord_31 PROGMEM = {H_BOT6, BEAKL, &state_31, NULL, KC_QUOTE, 0, single_dance};
uint8_t state_32 = IDLE;
const struct Chord chord_32 PROGMEM = {H_BOT7, BEAKL, &state_32, NULL, KC_B, 0, single_dance};
uint8_t state_33 = IDLE;
const struct Chord chord_33 PROGMEM = {H_BOT8, BEAKL, &state_33, NULL, KC_P, 0, single_dance};
uint8_t state_34 = IDLE;
const struct Chord chord_34 PROGMEM = {H_BOT9, BEAKL, &state_34, NULL, KC_L, 0, single_dance};
uint8_t state_35 = IDLE;
const struct Chord chord_35 PROGMEM = {H_BOT10, BEAKL, &state_35, NULL, KC_F, 0, single_dance};
uint8_t state_36 = IDLE;
const struct Chord chord_36 PROGMEM = {H_BOT11, BEAKL, &state_36, NULL, KC_V, 0, single_dance};
uint8_t state_37 = IDLE;
const struct Chord chord_37 PROGMEM = {H_BOT12, BEAKL, &state_37, NULL, NUM, 0, perm_pseudolayer};
uint8_t state_38 = IDLE;
const struct Chord chord_38 PROGMEM = {H_TOP2 + H_TOP8 + H_BOT8, BEAKL, &state_38, NULL, KC_SCOLON, 0, single_dance};
uint8_t state_39 = IDLE;
const struct Chord chord_39 PROGMEM = {H_TOP2 + H_TOP3, BEAKL, &state_39, NULL, 0, 0, string_in};
uint8_t state_40 = IDLE;
const struct Chord chord_40 PROGMEM = {H_BOT3 + H_TOP8 + H_BOT8, BEAKL, &state_40, NULL, KC_GRAVE, 0, single_dance};
uint8_t state_41 = IDLE;
const struct Chord chord_41 PROGMEM = {H_BOT4 + H_TOP8 + H_BOT8, BEAKL, &state_41, NULL, KC_TILDE, 0, single_dance};
uint8_t state_42 = IDLE;
const struct Chord chord_42 PROGMEM = {H_BOT4 + H_BOT5 + H_TOP8 + H_BOT8, BEAKL, &state_42, NULL, 1, 0, string_in};
uint8_t state_43 = IDLE;
const struct Chord chord_43 PROGMEM = {H_TOP6 + H_TOP8 + H_BOT8, BEAKL, &state_43, NULL, KC_UNDERSCORE, 0, single_dance};
uint8_t state_44 = IDLE;
const struct Chord chord_44 PROGMEM = {H_BOT6 + H_TOP8 + H_BOT8, BEAKL, &state_44, NULL, KC_DOUBLE_QUOTE, 0, single_dance};
uint8_t state_45 = IDLE;
const struct Chord chord_45 PROGMEM = {H_TOP5 + H_BOT5 + H_THU6, BEAKL, &state_45, NULL, KC_DEL, 0, single_dance};
uint8_t state_46 = IDLE;
const struct Chord chord_46 PROGMEM = {H_THU5 + H_THU6, BEAKL, &state_46, NULL, KC_DEL, 0, single_dance};
uint8_t state_47 = IDLE;
uint8_t counter_47 = 0;
const struct Chord chord_47 PROGMEM = {H_THU1, BEAKL, &state_47, &counter_47, KC_ESC, FNC, key_layer_dance};
uint8_t state_48 = IDLE;
uint8_t counter_48 = 0;
const struct Chord chord_48 PROGMEM = {H_THU2, BEAKL, &state_48, &counter_48, KC_I, REGEX, key_layer_dance};
uint8_t state_49 = IDLE;
uint8_t counter_49 = 0;
const struct Chord chord_49 PROGMEM = {H_THU3, BEAKL, &state_49, &counter_49, KC_TAB, NUM, key_layer_dance};
uint8_t state_50 = IDLE;
uint8_t counter_50 = 0;
const struct Chord chord_50 PROGMEM = {H_THU4, BEAKL, &state_50, &counter_50, KC_ENTER, NAV, key_layer_dance};
uint8_t state_51 = IDLE;
uint8_t counter_51 = 0;
const struct Chord chord_51 PROGMEM = {H_THU5, BEAKL, &state_51, &counter_51, KC_SPC, SYM, key_layer_dance};
uint8_t state_52 = IDLE;
uint8_t counter_52 = 0;
const struct Chord chord_52 PROGMEM = {H_THU6, BEAKL, &state_52, &counter_52, KC_BSPC, MOUSE, key_layer_dance};
uint8_t state_53 = IDLE;
const struct Chord chord_53 PROGMEM = {H_TOP2, CAPS, &state_53, NULL, KC_COLON, 0, single_dance};
void function_54(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_Y);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_Y);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_Y);
            break;
        default:
            break;
    };
}
uint8_t state_54 = IDLE;
uint8_t counter_54 = 0;
const struct Chord chord_54 PROGMEM = {H_TOP3, CAPS, &state_54, &counter_54, 0, 0, function_54};
void function_55(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_O);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_O);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_O);
            break;
        default:
            break;
    };
}
uint8_t state_55 = IDLE;
uint8_t counter_55 = 0;
const struct Chord chord_55 PROGMEM = {H_TOP4, CAPS, &state_55, &counter_55, 0, 0, function_55};
void function_56(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_U);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_U);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_U);
            break;
        default:
            break;
    };
}
uint8_t state_56 = IDLE;
uint8_t counter_56 = 0;
const struct Chord chord_56 PROGMEM = {H_TOP5, CAPS, &state_56, &counter_56, 0, 0, function_56};
uint8_t state_57 = IDLE;
const struct Chord chord_57 PROGMEM = {H_TOP6, CAPS, &state_57, NULL, KC_MINUS, 0, single_dance};
void function_58(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_G);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_G);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_G);
            break;
        default:
            break;
    };
}
uint8_t state_58 = IDLE;
uint8_t counter_58 = 0;
const struct Chord chord_58 PROGMEM = {H_TOP7, CAPS, &state_58, &counter_58, 0, 0, function_58};
void function_59(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_D);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_D);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_D);
            break;
        default:
            break;
    };
}
uint8_t state_59 = IDLE;
uint8_t counter_59 = 0;
const struct Chord chord_59 PROGMEM = {H_TOP8, CAPS, &state_59, &counter_59, 0, 0, function_59};
void function_60(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_N);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_N);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_N);
            break;
        default:
            break;
    };
}
uint8_t state_60 = IDLE;
uint8_t counter_60 = 0;
const struct Chord chord_60 PROGMEM = {H_TOP9, CAPS, &state_60, &counter_60, 0, 0, function_60};
void function_61(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_M);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_M);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_M);
            break;
        default:
            break;
    };
}
uint8_t state_61 = IDLE;
uint8_t counter_61 = 0;
const struct Chord chord_61 PROGMEM = {H_TOP10, CAPS, &state_61, &counter_61, 0, 0, function_61};
void function_62(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_X);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_X);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_X);
            break;
        default:
            break;
    };
}
uint8_t state_62 = IDLE;
uint8_t counter_62 = 0;
const struct Chord chord_62 PROGMEM = {H_TOP11, CAPS, &state_62, &counter_62, 0, 0, function_62};
void function_63(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_Q);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_Q);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_Q);
            break;
        default:
            break;
    };
}
uint8_t state_63 = IDLE;
uint8_t counter_63 = 0;
const struct Chord chord_63 PROGMEM = {H_TOP2 + H_BOT2, CAPS, &state_63, &counter_63, 0, 0, function_63};
void function_64(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_H);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_H);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_H);
            break;
        default:
            break;
    };
}
uint8_t state_64 = IDLE;
uint8_t counter_64 = 0;
const struct Chord chord_64 PROGMEM = {H_TOP3 + H_BOT3, CAPS, &state_64, &counter_64, 0, 0, function_64};
void function_65(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_E);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_E);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_E);
            break;
        default:
            break;
    };
}
uint8_t state_65 = IDLE;
uint8_t counter_65 = 0;
const struct Chord chord_65 PROGMEM = {H_TOP4 + H_BOT4, CAPS, &state_65, &counter_65, 0, 0, function_65};
void function_66(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_A);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_A);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_A);
            break;
        default:
            break;
    };
}
uint8_t state_66 = IDLE;
uint8_t counter_66 = 0;
const struct Chord chord_66 PROGMEM = {H_TOP5 + H_BOT5, CAPS, &state_66, &counter_66, 0, 0, function_66};
void function_67(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_W);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_W);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_W);
            break;
        default:
            break;
    };
}
uint8_t state_67 = IDLE;
uint8_t counter_67 = 0;
const struct Chord chord_67 PROGMEM = {H_TOP6 + H_BOT6, CAPS, &state_67, &counter_67, 0, 0, function_67};
void function_68(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_C);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_C);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_C);
            break;
        default:
            break;
    };
}
uint8_t state_68 = IDLE;
uint8_t counter_68 = 0;
const struct Chord chord_68 PROGMEM = {H_TOP7 + H_BOT7, CAPS, &state_68, &counter_68, 0, 0, function_68};
void function_69(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_T);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_T);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_T);
            break;
        default:
            break;
    };
}
uint8_t state_69 = IDLE;
uint8_t counter_69 = 0;
const struct Chord chord_69 PROGMEM = {H_TOP8 + H_BOT8, CAPS, &state_69, &counter_69, 0, 0, function_69};
void function_70(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_R);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_R);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_R);
            break;
        default:
            break;
    };
}
uint8_t state_70 = IDLE;
uint8_t counter_70 = 0;
const struct Chord chord_70 PROGMEM = {H_TOP9 + H_BOT9, CAPS, &state_70, &counter_70, 0, 0, function_70};
void function_71(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_S);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_S);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_S);
            break;
        default:
            break;
    };
}
uint8_t state_71 = IDLE;
uint8_t counter_71 = 0;
const struct Chord chord_71 PROGMEM = {H_TOP10 + H_BOT10, CAPS, &state_71, &counter_71, 0, 0, function_71};
void function_72(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_Z);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_Z);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_Z);
            break;
        default:
            break;
    };
}
uint8_t state_72 = IDLE;
uint8_t counter_72 = 0;
const struct Chord chord_72 PROGMEM = {H_TOP11 + H_BOT11, CAPS, &state_72, &counter_72, 0, 0, function_72};
void function_73(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_J);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_J);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_J);
            break;
        default:
            break;
    };
}
uint8_t state_73 = IDLE;
uint8_t counter_73 = 0;
const struct Chord chord_73 PROGMEM = {H_BOT2, CAPS, &state_73, &counter_73, 0, 0, function_73};
uint8_t state_74 = IDLE;
const struct Chord chord_74 PROGMEM = {H_BOT3, CAPS, &state_74, NULL, KC_COMMA, 0, single_dance};
uint8_t state_75 = IDLE;
const struct Chord chord_75 PROGMEM = {H_BOT4, CAPS, &state_75, NULL, KC_DOT, 0, single_dance};
void function_76(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_K);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_K);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_K);
            break;
        default:
            break;
    };
}
uint8_t state_76 = IDLE;
uint8_t counter_76 = 0;
const struct Chord chord_76 PROGMEM = {H_BOT5, CAPS, &state_76, &counter_76, 0, 0, function_76};
uint8_t state_77 = IDLE;
const struct Chord chord_77 PROGMEM = {H_BOT6, CAPS, &state_77, NULL, KC_QUOTE, 0, single_dance};
void function_78(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_B);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_B);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_B);
            break;
        default:
            break;
    };
}
uint8_t state_78 = IDLE;
uint8_t counter_78 = 0;
const struct Chord chord_78 PROGMEM = {H_BOT7, CAPS, &state_78, &counter_78, 0, 0, function_78};
void function_79(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_P);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_P);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_P);
            break;
        default:
            break;
    };
}
uint8_t state_79 = IDLE;
uint8_t counter_79 = 0;
const struct Chord chord_79 PROGMEM = {H_BOT8, CAPS, &state_79, &counter_79, 0, 0, function_79};
void function_80(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_L);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_L);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_L);
            break;
        default:
            break;
    };
}
uint8_t state_80 = IDLE;
uint8_t counter_80 = 0;
const struct Chord chord_80 PROGMEM = {H_BOT9, CAPS, &state_80, &counter_80, 0, 0, function_80};
void function_81(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_F);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_F);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_F);
            break;
        default:
            break;
    };
}
uint8_t state_81 = IDLE;
uint8_t counter_81 = 0;
const struct Chord chord_81 PROGMEM = {H_BOT10, CAPS, &state_81, &counter_81, 0, 0, function_81};
void function_82(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_V);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_V);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_V);
            break;
        default:
            break;
    };
}
uint8_t state_82 = IDLE;
uint8_t counter_82 = 0;
const struct Chord chord_82 PROGMEM = {H_BOT11, CAPS, &state_82, &counter_82, 0, 0, function_82};
void function_83(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LSFT);
            key_in(KC_I);
            break;
        case DEACTIVATED:
            key_out(KC_LSFT);
            key_out(KC_I);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LSFT);
            key_out(KC_I);
            break;
        default:
            break;
    };
}
uint8_t state_83 = IDLE;
uint8_t counter_83 = 0;
const struct Chord chord_83 PROGMEM = {H_THU2, CAPS, &state_83, &counter_83, 0, 0, function_83};
uint8_t state_84 = IDLE;
const struct Chord chord_84 PROGMEM = {H_TOP7, REGEX, &state_84, NULL, KC_ASTERISK, 0, single_dance};
uint8_t state_85 = IDLE;
const struct Chord chord_85 PROGMEM = {H_TOP8, REGEX, &state_85, NULL, KC_LBRACKET, 0, single_dance};
uint8_t state_86 = IDLE;
const struct Chord chord_86 PROGMEM = {H_TOP9, REGEX, &state_86, NULL, KC_CIRCUMFLEX, 0, single_dance};
uint8_t state_87 = IDLE;
const struct Chord chord_87 PROGMEM = {H_TOP10, REGEX, &state_87, NULL, KC_RBRACKET, 0, single_dance};
uint8_t state_88 = IDLE;
const struct Chord chord_88 PROGMEM = {H_TOP7 + H_BOT7, REGEX, &state_88, NULL, KC_QUESTION, 0, single_dance};
uint8_t state_89 = IDLE;
const struct Chord chord_89 PROGMEM = {H_TOP8 + H_BOT8, REGEX, &state_89, NULL, KC_LPRN, 0, single_dance};
uint8_t state_90 = IDLE;
const struct Chord chord_90 PROGMEM = {H_TOP9 + H_BOT9, REGEX, &state_90, NULL, KC_DOLLAR, 0, single_dance};
uint8_t state_91 = IDLE;
const struct Chord chord_91 PROGMEM = {H_TOP10 + H_BOT10, REGEX, &state_91, NULL, KC_RPRN, 0, single_dance};
uint8_t state_92 = IDLE;
const struct Chord chord_92 PROGMEM = {H_BOT7, REGEX, &state_92, NULL, KC_PIPE, 0, single_dance};
uint8_t state_93 = IDLE;
const struct Chord chord_93 PROGMEM = {H_BOT8, REGEX, &state_93, NULL, KC_LEFT_CURLY_BRACE, 0, single_dance};
uint8_t state_94 = IDLE;
const struct Chord chord_94 PROGMEM = {H_BOT9, REGEX, &state_94, NULL, KC_HASH, 0, single_dance};
uint8_t state_95 = IDLE;
const struct Chord chord_95 PROGMEM = {H_BOT10, REGEX, &state_95, NULL, KC_RIGHT_CURLY_BRACE, 0, single_dance};
uint8_t state_96 = IDLE;
const struct Chord chord_96 PROGMEM = {H_THU5, REGEX, &state_96, NULL, KC_BSLASH, 0, single_dance};
uint8_t state_97 = IDLE;
const struct Chord chord_97 PROGMEM = {H_THU6, REGEX, &state_97, NULL, KC_DEL, 0, single_dance};
uint8_t state_98 = IDLE;
const struct Chord chord_98 PROGMEM = {H_TOP7 + H_TOP8, REGEX, &state_98, NULL, 2, 0, string_in};
uint8_t state_99 = IDLE;
const struct Chord chord_99 PROGMEM = {H_TOP3, SYM, &state_99, NULL, KC_DOT, 0, single_dance};
uint8_t state_100 = IDLE;
const struct Chord chord_100 PROGMEM = {H_TOP4, SYM, &state_100, NULL, KC_ASTERISK, 0, single_dance};
uint8_t state_101 = IDLE;
const struct Chord chord_101 PROGMEM = {H_TOP5, SYM, &state_101, NULL, KC_AMPERSAND, 0, single_dance};
uint8_t state_102 = IDLE;
const struct Chord chord_102 PROGMEM = {H_TOP6, SYM, &state_102, NULL, KC_PLUS, 0, single_dance};
uint8_t state_103 = IDLE;
const struct Chord chord_103 PROGMEM = {H_TOP3 + H_BOT3, SYM, &state_103, NULL, KC_QUESTION, 0, single_dance};
uint8_t state_104 = IDLE;
const struct Chord chord_104 PROGMEM = {H_TOP4 + H_BOT4, SYM, &state_104, NULL, KC_EXCLAIM, 0, single_dance};
uint8_t state_105 = IDLE;
const struct Chord chord_105 PROGMEM = {H_TOP5 + H_BOT5, SYM, &state_105, NULL, KC_SLASH, 0, single_dance};
uint8_t state_106 = IDLE;
const struct Chord chord_106 PROGMEM = {H_TOP6 + H_BOT6, SYM, &state_106, NULL, KC_PIPE, 0, single_dance};
uint8_t state_107 = IDLE;
const struct Chord chord_107 PROGMEM = {H_BOT3, SYM, &state_107, NULL, KC_LEFT_ANGLE_BRACKET, 0, single_dance};
uint8_t state_108 = IDLE;
const struct Chord chord_108 PROGMEM = {H_BOT4, SYM, &state_108, NULL, KC_RIGHT_ANGLE_BRACKET, 0, single_dance};
uint8_t state_109 = IDLE;
const struct Chord chord_109 PROGMEM = {H_BOT5, SYM, &state_109, NULL, KC_PERCENT, 0, single_dance};
uint8_t state_110 = IDLE;
const struct Chord chord_110 PROGMEM = {H_BOT6, SYM, &state_110, NULL, KC_AT, 0, single_dance};
uint8_t state_111 = IDLE;
const struct Chord chord_111 PROGMEM = {H_THU2, SYM, &state_111, NULL, KC_EQUAL, 0, single_dance};
uint8_t state_112 = IDLE;
const struct Chord chord_112 PROGMEM = {H_THU3, SYM, &state_112, NULL, KC_BSLASH, 0, single_dance};
uint8_t state_113 = IDLE;
const struct Chord chord_113 PROGMEM = {H_BOT2 + H_BOT3, SYM, &state_113, NULL, 3, 0, string_in};
uint8_t state_114 = IDLE;
const struct Chord chord_114 PROGMEM = {H_BOT4 + H_BOT5, SYM, &state_114, NULL, 4, 0, string_in};
uint8_t state_115 = IDLE;
const struct Chord chord_115 PROGMEM = {H_THU1 + H_THU2, SYM, &state_115, NULL, 5, 0, string_in};
uint8_t state_116 = IDLE;
const struct Chord chord_116 PROGMEM = {H_TOP3, SYMBOL, &state_116, NULL, KC_DOT, 0, single_dance};
uint8_t state_117 = IDLE;
const struct Chord chord_117 PROGMEM = {H_TOP4, SYMBOL, &state_117, NULL, KC_ASTERISK, 0, single_dance};
uint8_t state_118 = IDLE;
const struct Chord chord_118 PROGMEM = {H_TOP5, SYMBOL, &state_118, NULL, KC_AMPERSAND, 0, single_dance};
uint8_t state_119 = IDLE;
const struct Chord chord_119 PROGMEM = {H_TOP6, SYMBOL, &state_119, NULL, KC_PLUS, 0, single_dance};
uint8_t state_120 = IDLE;
const struct Chord chord_120 PROGMEM = {H_TOP7, SYMBOL, &state_120, NULL, KC_ASTERISK, 0, single_dance};
uint8_t state_121 = IDLE;
const struct Chord chord_121 PROGMEM = {H_TOP8, SYMBOL, &state_121, NULL, KC_LBRACKET, 0, single_dance};
uint8_t state_122 = IDLE;
const struct Chord chord_122 PROGMEM = {H_TOP9, SYMBOL, &state_122, NULL, KC_CIRCUMFLEX, 0, single_dance};
uint8_t state_123 = IDLE;
const struct Chord chord_123 PROGMEM = {H_TOP10, SYMBOL, &state_123, NULL, KC_RBRACKET, 0, single_dance};
uint8_t state_124 = IDLE;
const struct Chord chord_124 PROGMEM = {H_TOP12, SYMBOL, &state_124, NULL, BEAKL, 0, perm_pseudolayer};
uint8_t state_125 = IDLE;
const struct Chord chord_125 PROGMEM = {H_TOP3 + H_BOT3, SYMBOL, &state_125, NULL, KC_QUESTION, 0, single_dance};
uint8_t state_126 = IDLE;
const struct Chord chord_126 PROGMEM = {H_TOP4 + H_BOT4, SYMBOL, &state_126, NULL, KC_EXCLAIM, 0, single_dance};
uint8_t state_127 = IDLE;
const struct Chord chord_127 PROGMEM = {H_TOP5 + H_BOT5, SYMBOL, &state_127, NULL, KC_SLASH, 0, single_dance};
uint8_t state_128 = IDLE;
const struct Chord chord_128 PROGMEM = {H_TOP6 + H_BOT6, SYMBOL, &state_128, NULL, KC_PIPE, 0, single_dance};
uint8_t state_129 = IDLE;
const struct Chord chord_129 PROGMEM = {H_TOP7 + H_BOT7, SYMBOL, &state_129, NULL, KC_QUESTION, 0, single_dance};
uint8_t state_130 = IDLE;
const struct Chord chord_130 PROGMEM = {H_TOP8 + H_BOT8, SYMBOL, &state_130, NULL, KC_LPRN, 0, single_dance};
uint8_t state_131 = IDLE;
const struct Chord chord_131 PROGMEM = {H_TOP9 + H_BOT9, SYMBOL, &state_131, NULL, KC_DOLLAR, 0, single_dance};
uint8_t state_132 = IDLE;
const struct Chord chord_132 PROGMEM = {H_TOP10 + H_BOT10, SYMBOL, &state_132, NULL, KC_RPRN, 0, single_dance};
uint8_t state_133 = IDLE;
const struct Chord chord_133 PROGMEM = {H_BOT3, SYMBOL, &state_133, NULL, KC_LEFT_ANGLE_BRACKET, 0, single_dance};
uint8_t state_134 = IDLE;
const struct Chord chord_134 PROGMEM = {H_BOT4, SYMBOL, &state_134, NULL, KC_RIGHT_ANGLE_BRACKET, 0, single_dance};
uint8_t state_135 = IDLE;
const struct Chord chord_135 PROGMEM = {H_BOT5, SYMBOL, &state_135, NULL, KC_PERCENT, 0, single_dance};
uint8_t state_136 = IDLE;
const struct Chord chord_136 PROGMEM = {H_BOT6, SYMBOL, &state_136, NULL, KC_AT, 0, single_dance};
uint8_t state_137 = IDLE;
const struct Chord chord_137 PROGMEM = {H_BOT7, SYMBOL, &state_137, NULL, KC_PIPE, 0, single_dance};
uint8_t state_138 = IDLE;
const struct Chord chord_138 PROGMEM = {H_BOT8, SYMBOL, &state_138, NULL, KC_LEFT_CURLY_BRACE, 0, single_dance};
uint8_t state_139 = IDLE;
const struct Chord chord_139 PROGMEM = {H_BOT9, SYMBOL, &state_139, NULL, KC_HASH, 0, single_dance};
uint8_t state_140 = IDLE;
const struct Chord chord_140 PROGMEM = {H_BOT10, SYMBOL, &state_140, NULL, KC_RIGHT_CURLY_BRACE, 0, single_dance};
uint8_t state_141 = IDLE;
const struct Chord chord_141 PROGMEM = {H_THU1, SYMBOL, &state_141, NULL, BEAKL, 0, perm_pseudolayer};
uint8_t state_142 = IDLE;
const struct Chord chord_142 PROGMEM = {H_THU2, SYMBOL, &state_142, NULL, KC_EQUAL, 0, single_dance};
uint8_t state_143 = IDLE;
const struct Chord chord_143 PROGMEM = {H_THU3, SYMBOL, &state_143, NULL, KC_BSLASH, 0, single_dance};
uint8_t state_144 = IDLE;
const struct Chord chord_144 PROGMEM = {H_THU5, SYMBOL, &state_144, NULL, KC_SPC, 0, single_dance};
uint8_t state_145 = IDLE;
const struct Chord chord_145 PROGMEM = {H_THU6, SYMBOL, &state_145, NULL, KC_DEL, 0, single_dance};
uint8_t state_146 = IDLE;
const struct Chord chord_146 PROGMEM = {H_BOT2 + H_BOT3, SYMBOL, &state_146, NULL, 6, 0, string_in};
uint8_t state_147 = IDLE;
const struct Chord chord_147 PROGMEM = {H_BOT4 + H_BOT5, SYMBOL, &state_147, NULL, 7, 0, string_in};
uint8_t state_148 = IDLE;
const struct Chord chord_148 PROGMEM = {H_THU1 + H_THU2, SYMBOL, &state_148, NULL, 8, 0, string_in};
uint8_t state_149 = IDLE;
const struct Chord chord_149 PROGMEM = {H_TOP3, NUM, &state_149, NULL, KC_A, 0, single_dance};
uint8_t state_150 = IDLE;
const struct Chord chord_150 PROGMEM = {H_TOP4, NUM, &state_150, NULL, KC_B, 0, single_dance};
uint8_t state_151 = IDLE;
const struct Chord chord_151 PROGMEM = {H_TOP5, NUM, &state_151, NULL, KC_C, 0, single_dance};
uint8_t state_152 = IDLE;
const struct Chord chord_152 PROGMEM = {H_TOP7, NUM, &state_152, NULL, KC_SLASH, 0, single_dance};
uint8_t state_153 = IDLE;
const struct Chord chord_153 PROGMEM = {H_TOP8, NUM, &state_153, NULL, KC_4, 0, single_dance};
uint8_t state_154 = IDLE;
const struct Chord chord_154 PROGMEM = {H_TOP9, NUM, &state_154, NULL, KC_5, 0, single_dance};
uint8_t state_155 = IDLE;
const struct Chord chord_155 PROGMEM = {H_TOP10, NUM, &state_155, NULL, KC_9, 0, single_dance};
uint8_t state_156 = IDLE;
const struct Chord chord_156 PROGMEM = {H_TOP11, NUM, &state_156, NULL, KC_ASTERISK, 0, single_dance};
uint8_t state_157 = IDLE;
const struct Chord chord_157 PROGMEM = {H_TOP2 + H_BOT2, NUM, &state_157, NULL, KC_LGUI, 0, single_dance};
uint8_t state_158 = IDLE;
const struct Chord chord_158 PROGMEM = {H_TOP3 + H_BOT3, NUM, &state_158, NULL, KC_D, KC_LCTL, key_mod_dance};
uint8_t state_159 = IDLE;
const struct Chord chord_159 PROGMEM = {H_TOP4 + H_BOT4, NUM, &state_159, NULL, KC_E, KC_LALT, key_mod_dance};
uint8_t state_160 = IDLE;
uint8_t counter_160 = 0;
const struct Chord chord_160 PROGMEM = {H_TOP5 + H_BOT5, NUM, &state_160, &counter_160, KC_F, KC_LSFT, key_key_dance};
uint8_t state_161 = IDLE;
const struct Chord chord_161 PROGMEM = {H_TOP7 + H_BOT7, NUM, &state_161, NULL, KC_DOT, 0, single_dance};
uint8_t state_162 = IDLE;
const struct Chord chord_162 PROGMEM = {H_TOP8 + H_BOT8, NUM, &state_162, NULL, KC_1, 0, single_dance};
uint8_t state_163 = IDLE;
const struct Chord chord_163 PROGMEM = {H_TOP9 + H_BOT9, NUM, &state_163, NULL, KC_2, 0, single_dance};
uint8_t state_164 = IDLE;
const struct Chord chord_164 PROGMEM = {H_TOP10 + H_BOT10, NUM, &state_164, NULL, KC_3, 0, single_dance};
uint8_t state_165 = IDLE;
const struct Chord chord_165 PROGMEM = {H_TOP11 + H_BOT11, NUM, &state_165, NULL, KC_MINUS, 0, single_dance};
uint8_t state_166 = IDLE;
const struct Chord chord_166 PROGMEM = {H_BOT3, NUM, &state_166, NULL, KC_LBRACKET, 0, single_dance};
uint8_t state_167 = IDLE;
const struct Chord chord_167 PROGMEM = {H_BOT4, NUM, &state_167, NULL, KC_RBRACKET, 0, single_dance};
uint8_t state_168 = IDLE;
const struct Chord chord_168 PROGMEM = {H_BOT5, NUM, &state_168, NULL, KC_G, 0, single_dance};
uint8_t state_169 = IDLE;
const struct Chord chord_169 PROGMEM = {H_BOT8, NUM, &state_169, NULL, KC_8, 0, single_dance};
uint8_t state_170 = IDLE;
const struct Chord chord_170 PROGMEM = {H_BOT9, NUM, &state_170, NULL, KC_6, 0, single_dance};
uint8_t state_171 = IDLE;
const struct Chord chord_171 PROGMEM = {H_BOT10, NUM, &state_171, NULL, KC_7, 0, single_dance};
uint8_t state_172 = IDLE;
const struct Chord chord_172 PROGMEM = {H_BOT11, NUM, &state_172, NULL, KC_PLUS, 0, single_dance};
uint8_t state_173 = IDLE;
const struct Chord chord_173 PROGMEM = {H_BOT12, NUM, &state_173, NULL, BEAKL, 0, perm_pseudolayer};
uint8_t state_174 = IDLE;
const struct Chord chord_174 PROGMEM = {H_THU1, NUM, &state_174, NULL, BEAKL, 0, perm_pseudolayer};
uint8_t state_175 = IDLE;
const struct Chord chord_175 PROGMEM = {H_THU4, NUM, &state_175, NULL, KC_BSLASH, 0, single_dance};
uint8_t state_176 = IDLE;
const struct Chord chord_176 PROGMEM = {H_THU5, NUM, &state_176, NULL, KC_0, 0, single_dance};
uint8_t state_177 = IDLE;
const struct Chord chord_177 PROGMEM = {H_THU6, NUM, &state_177, NULL, KC_EQUAL, 0, single_dance};
uint8_t state_178 = IDLE;
const struct Chord chord_178 PROGMEM = {H_TOP7 + H_TOP8, NUM, &state_178, NULL, KC_COLON, 0, single_dance};
uint8_t state_179 = IDLE;
const struct Chord chord_179 PROGMEM = {H_BOT7 + H_BOT8, NUM, &state_179, NULL, KC_SCOLON, 0, single_dance};
uint8_t state_180 = IDLE;
const struct Chord chord_180 PROGMEM = {H_TOP1, FNC, &state_180, NULL, BEAKL, 0, perm_pseudolayer};
uint8_t state_181 = IDLE;
const struct Chord chord_181 PROGMEM = {H_TOP8, FNC, &state_181, NULL, KC_F4, 0, single_dance};
uint8_t state_182 = IDLE;
const struct Chord chord_182 PROGMEM = {H_TOP9, FNC, &state_182, NULL, KC_F5, 0, single_dance};
uint8_t state_183 = IDLE;
const struct Chord chord_183 PROGMEM = {H_TOP10, FNC, &state_183, NULL, KC_F9, 0, single_dance};
uint8_t state_184 = IDLE;
const struct Chord chord_184 PROGMEM = {H_TOP11, FNC, &state_184, NULL, KC_F12, 0, single_dance};
uint8_t state_185 = IDLE;
const struct Chord chord_185 PROGMEM = {H_TOP2 + H_BOT2, FNC, &state_185, NULL, KC_LGUI, 0, single_dance};
uint8_t state_186 = IDLE;
const struct Chord chord_186 PROGMEM = {H_TOP3 + H_BOT3, FNC, &state_186, NULL, KC_LCTL, 0, single_dance};
uint8_t state_187 = IDLE;
const struct Chord chord_187 PROGMEM = {H_TOP4 + H_BOT4, FNC, &state_187, NULL, KC_LALT, 0, single_dance};
uint8_t state_188 = IDLE;
const struct Chord chord_188 PROGMEM = {H_TOP5 + H_BOT5, FNC, &state_188, NULL, KC_LSFT, 0, single_dance};
uint8_t state_189 = IDLE;
const struct Chord chord_189 PROGMEM = {H_TOP8 + H_BOT8, FNC, &state_189, NULL, KC_F1, 0, single_dance};
uint8_t state_190 = IDLE;
const struct Chord chord_190 PROGMEM = {H_TOP9 + H_BOT9, FNC, &state_190, NULL, KC_F2, 0, single_dance};
uint8_t state_191 = IDLE;
const struct Chord chord_191 PROGMEM = {H_TOP10 + H_BOT10, FNC, &state_191, NULL, KC_F3, 0, single_dance};
uint8_t state_192 = IDLE;
const struct Chord chord_192 PROGMEM = {H_TOP11 + H_BOT11, FNC, &state_192, NULL, KC_F11, 0, single_dance};
uint8_t state_193 = IDLE;
const struct Chord chord_193 PROGMEM = {H_BOT8, FNC, &state_193, NULL, KC_F8, 0, single_dance};
uint8_t state_194 = IDLE;
const struct Chord chord_194 PROGMEM = {H_BOT9, FNC, &state_194, NULL, KC_F6, 0, single_dance};
uint8_t state_195 = IDLE;
const struct Chord chord_195 PROGMEM = {H_BOT10, FNC, &state_195, NULL, KC_F7, 0, single_dance};
uint8_t state_196 = IDLE;
const struct Chord chord_196 PROGMEM = {H_BOT11, FNC, &state_196, NULL, KC_F10, 0, single_dance};
uint8_t state_197 = IDLE;
const struct Chord chord_197 PROGMEM = {H_THU1, FNC, &state_197, NULL, BEAKL, 0, perm_pseudolayer};
void function_198(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LCTL);
            key_in(KC_LSFT);
            key_in(KC_C);
            break;
        case DEACTIVATED:
            key_out(KC_LCTL);
            key_out(KC_LSFT);
            key_out(KC_C);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LCTL);
            key_out(KC_LSFT);
            key_out(KC_C);
            break;
        default:
            break;
    };
}
uint8_t state_198 = IDLE;
uint8_t counter_198 = 0;
const struct Chord chord_198 PROGMEM = {H_TOP4, NAV, &state_198, &counter_198, 0, 0, function_198};
void function_199(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LCTL);
            key_in(KC_LSFT);
            key_in(KC_V);
            break;
        case DEACTIVATED:
            key_out(KC_LCTL);
            key_out(KC_LSFT);
            key_out(KC_V);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LCTL);
            key_out(KC_LSFT);
            key_out(KC_V);
            break;
        default:
            break;
    };
}
uint8_t state_199 = IDLE;
uint8_t counter_199 = 0;
const struct Chord chord_199 PROGMEM = {H_TOP5, NAV, &state_199, &counter_199, 0, 0, function_199};
uint8_t state_200 = IDLE;
const struct Chord chord_200 PROGMEM = {H_TOP8, NAV, &state_200, NULL, KC_HOME, 0, single_dance};
uint8_t state_201 = IDLE;
const struct Chord chord_201 PROGMEM = {H_TOP9, NAV, &state_201, NULL, KC_UP, 0, single_dance};
uint8_t state_202 = IDLE;
const struct Chord chord_202 PROGMEM = {H_TOP10, NAV, &state_202, NULL, KC_END, 0, single_dance};
uint8_t state_203 = IDLE;
const struct Chord chord_203 PROGMEM = {H_TOP11, NAV, &state_203, NULL, KC_PGUP, 0, single_dance};
uint8_t state_204 = IDLE;
const struct Chord chord_204 PROGMEM = {H_TOP2 + H_BOT2, NAV, &state_204, NULL, KC_LGUI, 0, single_dance};
uint8_t state_205 = IDLE;
const struct Chord chord_205 PROGMEM = {H_TOP3 + H_BOT3, NAV, &state_205, NULL, KC_LCTL, 0, single_dance};
uint8_t state_206 = IDLE;
const struct Chord chord_206 PROGMEM = {H_TOP4 + H_BOT4, NAV, &state_206, NULL, KC_LALT, 0, single_dance};
uint8_t state_207 = IDLE;
const struct Chord chord_207 PROGMEM = {H_TOP5 + H_BOT5, NAV, &state_207, NULL, KC_LSFT, 0, single_dance};
uint8_t state_208 = IDLE;
const struct Chord chord_208 PROGMEM = {H_TOP12 + H_BOT12, NAV, &state_208, NULL, BEAKL, 0, perm_pseudolayer};
void function_209(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LCTL);
            key_in(KC_Z);
            break;
        case DEACTIVATED:
            key_out(KC_LCTL);
            key_out(KC_Z);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LCTL);
            key_out(KC_Z);
            break;
        default:
            break;
    };
}
uint8_t state_209 = IDLE;
uint8_t counter_209 = 0;
const struct Chord chord_209 PROGMEM = {H_BOT2, NAV, &state_209, &counter_209, 0, 0, function_209};
void function_210(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LCTL);
            key_in(KC_X);
            break;
        case DEACTIVATED:
            key_out(KC_LCTL);
            key_out(KC_X);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LCTL);
            key_out(KC_X);
            break;
        default:
            break;
    };
}
uint8_t state_210 = IDLE;
uint8_t counter_210 = 0;
const struct Chord chord_210 PROGMEM = {H_BOT3, NAV, &state_210, &counter_210, 0, 0, function_210};
void function_211(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LCTL);
            key_in(KC_C);
            break;
        case DEACTIVATED:
            key_out(KC_LCTL);
            key_out(KC_C);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LCTL);
            key_out(KC_C);
            break;
        default:
            break;
    };
}
uint8_t state_211 = IDLE;
uint8_t counter_211 = 0;
const struct Chord chord_211 PROGMEM = {H_BOT4, NAV, &state_211, &counter_211, 0, 0, function_211};
void function_212(const struct Chord* self) {
    switch (*self->state) {
        case ACTIVATED:
            key_in(KC_LCTL);
            key_in(KC_V);
            break;
        case DEACTIVATED:
            key_out(KC_LCTL);
            key_out(KC_V);
            *self->state = IDLE;
            break;
        case RESTART:
            key_out(KC_LCTL);
            key_out(KC_V);
            break;
        default:
            break;
    };
}
uint8_t state_212 = IDLE;
uint8_t counter_212 = 0;
const struct Chord chord_212 PROGMEM = {H_BOT5, NAV, &state_212, &counter_212, 0, 0, function_212};
uint8_t state_213 = IDLE;
const struct Chord chord_213 PROGMEM = {H_BOT8, NAV, &state_213, NULL, KC_LEFT, 0, single_dance};
uint8_t state_214 = IDLE;
const struct Chord chord_214 PROGMEM = {H_BOT9, NAV, &state_214, NULL, KC_DOWN, 0, single_dance};
uint8_t state_215 = IDLE;
const struct Chord chord_215 PROGMEM = {H_BOT10, NAV, &state_215, NULL, KC_RIGHT, 0, single_dance};
uint8_t state_216 = IDLE;
const struct Chord chord_216 PROGMEM = {H_BOT11, NAV, &state_216, NULL, KC_PGDN, 0, single_dance};
uint8_t state_217 = IDLE;
const struct Chord chord_217 PROGMEM = {H_THU1, NAV, &state_217, NULL, BEAKL, 0, perm_pseudolayer};
uint8_t state_218 = IDLE;
const struct Chord chord_218 PROGMEM = {H_TOP5 + H_TOP6, NAV, &state_218, NULL, 9, 0, string_in};
uint8_t state_219 = IDLE;
const struct Chord chord_219 PROGMEM = {H_BOT5 + H_BOT6, NAV, &state_219, NULL, 10, 0, string_in};
uint8_t state_220 = IDLE;
const struct Chord chord_220 PROGMEM = {H_TOP1, MOUSE, &state_220, NULL, 0, 0, reset};
uint8_t state_221 = IDLE;
const struct Chord chord_221 PROGMEM = {H_TOP8, MOUSE, &state_221, NULL, KC_BTN1, 0, single_dance};
uint8_t state_222 = IDLE;
const struct Chord chord_222 PROGMEM = {H_TOP9, MOUSE, &state_222, NULL, KC_MS_U, 0, single_dance};
uint8_t state_223 = IDLE;
const struct Chord chord_223 PROGMEM = {H_TOP10, MOUSE, &state_223, NULL, KC_BTN2, 0, single_dance};
uint8_t state_224 = IDLE;
const struct Chord chord_224 PROGMEM = {H_TOP11, MOUSE, &state_224, NULL, KC_WH_U, 0, single_dance};
uint8_t state_225 = IDLE;
const struct Chord chord_225 PROGMEM = {H_TOP1 + H_BOT1, MOUSE, &state_225, NULL, BEAKL, 0, perm_pseudolayer};
uint8_t state_226 = IDLE;
const struct Chord chord_226 PROGMEM = {H_TOP3 + H_BOT3, MOUSE, &state_226, NULL, KC_LGUI, 0, single_dance};
uint8_t state_227 = IDLE;
const struct Chord chord_227 PROGMEM = {H_TOP4 + H_BOT4, MOUSE, &state_227, NULL, KC_LCTL, 0, single_dance};
uint8_t state_228 = IDLE;
const struct Chord chord_228 PROGMEM = {H_TOP5 + H_BOT5, MOUSE, &state_228, NULL, KC_LALT, 0, single_dance};
uint8_t state_229 = IDLE;
const struct Chord chord_229 PROGMEM = {H_TOP6 + H_BOT6, MOUSE, &state_229, NULL, KC_LSFT, 0, single_dance};
uint8_t state_230 = IDLE;
const struct Chord chord_230 PROGMEM = {H_BOT8, MOUSE, &state_230, NULL, KC_MS_L, 0, single_dance};
uint8_t state_231 = IDLE;
const struct Chord chord_231 PROGMEM = {H_BOT9, MOUSE, &state_231, NULL, KC_MS_D, 0, single_dance};
uint8_t state_232 = IDLE;
const struct Chord chord_232 PROGMEM = {H_BOT10, MOUSE, &state_232, NULL, KC_MS_R, 0, single_dance};
uint8_t state_233 = IDLE;
const struct Chord chord_233 PROGMEM = {H_BOT11, MOUSE, &state_233, NULL, KC_WH_D, 0, single_dance};
uint8_t state_234 = IDLE;
const struct Chord chord_234 PROGMEM = {H_THU1, MOUSE, &state_234, NULL, BEAKL, 0, perm_pseudolayer};
uint8_t state_235 = IDLE;
const struct Chord chord_235 PROGMEM = {H_TOP1, STENO, &state_235, NULL, STN_FN, 0, single_dance};
uint8_t state_236 = IDLE;
const struct Chord chord_236 PROGMEM = {H_TOP2, STENO, &state_236, NULL, STN_S1, 0, single_dance};
uint8_t state_237 = IDLE;
const struct Chord chord_237 PROGMEM = {H_TOP3, STENO, &state_237, NULL, STN_TL, 0, single_dance};
uint8_t state_238 = IDLE;
const struct Chord chord_238 PROGMEM = {H_TOP4, STENO, &state_238, NULL, STN_PL, 0, single_dance};
uint8_t state_239 = IDLE;
const struct Chord chord_239 PROGMEM = {H_TOP5, STENO, &state_239, NULL, STN_HL, 0, single_dance};
uint8_t state_240 = IDLE;
const struct Chord chord_240 PROGMEM = {H_TOP6, STENO, &state_240, NULL, STN_ST1, 0, single_dance};
uint8_t state_241 = IDLE;
const struct Chord chord_241 PROGMEM = {H_TOP7, STENO, &state_241, NULL, STN_ST3, 0, single_dance};
uint8_t state_242 = IDLE;
const struct Chord chord_242 PROGMEM = {H_TOP8, STENO, &state_242, NULL, STN_FR, 0, single_dance};
uint8_t state_243 = IDLE;
const struct Chord chord_243 PROGMEM = {H_TOP9, STENO, &state_243, NULL, STN_PR, 0, single_dance};
uint8_t state_244 = IDLE;
const struct Chord chord_244 PROGMEM = {H_TOP10, STENO, &state_244, NULL, STN_LR, 0, single_dance};
uint8_t state_245 = IDLE;
const struct Chord chord_245 PROGMEM = {H_TOP11, STENO, &state_245, NULL, STN_TR, 0, single_dance};
uint8_t state_246 = IDLE;
const struct Chord chord_246 PROGMEM = {H_TOP12, STENO, &state_246, NULL, STN_DR, 0, single_dance};
uint8_t state_247 = IDLE;
const struct Chord chord_247 PROGMEM = {H_BOT1, STENO, &state_247, NULL, STN_PWR, 0, single_dance};
uint8_t state_248 = IDLE;
const struct Chord chord_248 PROGMEM = {H_BOT2, STENO, &state_248, NULL, STN_S2, 0, single_dance};
uint8_t state_249 = IDLE;
const struct Chord chord_249 PROGMEM = {H_BOT3, STENO, &state_249, NULL, STN_KL, 0, single_dance};
uint8_t state_250 = IDLE;
const struct Chord chord_250 PROGMEM = {H_BOT4, STENO, &state_250, NULL, STN_WL, 0, single_dance};
uint8_t state_251 = IDLE;
const struct Chord chord_251 PROGMEM = {H_BOT5, STENO, &state_251, NULL, STN_RL, 0, single_dance};
uint8_t state_252 = IDLE;
const struct Chord chord_252 PROGMEM = {H_BOT6, STENO, &state_252, NULL, STN_ST2, 0, single_dance};
uint8_t state_253 = IDLE;
const struct Chord chord_253 PROGMEM = {H_BOT7, STENO, &state_253, NULL, STN_ST4, 0, single_dance};
uint8_t state_254 = IDLE;
const struct Chord chord_254 PROGMEM = {H_BOT8, STENO, &state_254, NULL, STN_RR, 0, single_dance};
uint8_t state_255 = IDLE;
const struct Chord chord_255 PROGMEM = {H_BOT9, STENO, &state_255, NULL, STN_BR, 0, single_dance};
uint8_t state_256 = IDLE;
const struct Chord chord_256 PROGMEM = {H_BOT10, STENO, &state_256, NULL, STN_GR, 0, single_dance};
uint8_t state_257 = IDLE;
const struct Chord chord_257 PROGMEM = {H_BOT11, STENO, &state_257, NULL, STN_SR, 0, single_dance};
uint8_t state_258 = IDLE;
const struct Chord chord_258 PROGMEM = {H_BOT12, STENO, &state_258, NULL, STN_ZR, 0, single_dance};
uint8_t state_259 = IDLE;
const struct Chord chord_259 PROGMEM = {H_THU1, STENO, &state_259, NULL, STN_N1, 0, single_dance};
uint8_t state_260 = IDLE;
const struct Chord chord_260 PROGMEM = {H_THU2, STENO, &state_260, NULL, STN_A, 0, single_dance};
uint8_t state_261 = IDLE;
const struct Chord chord_261 PROGMEM = {H_THU3, STENO, &state_261, NULL, STN_O, 0, single_dance};
uint8_t state_262 = IDLE;
const struct Chord chord_262 PROGMEM = {H_THU4, STENO, &state_262, NULL, STN_E, 0, single_dance};
uint8_t state_263 = IDLE;
const struct Chord chord_263 PROGMEM = {H_THU5, STENO, &state_263, NULL, STN_U, 0, single_dance};
uint8_t state_264 = IDLE;
const struct Chord chord_264 PROGMEM = {H_THU6, STENO, &state_264, NULL, STN_N7, 0, single_dance};
uint8_t state_265 = IDLE;
const struct Chord chord_265 PROGMEM = {H_TOP1 + H_BOT1, STENO, &state_265, NULL, BEAKL, 0, perm_pseudolayer};
uint8_t state_266 = IDLE;
const struct Chord chord_266 PROGMEM = {H_TOP1, PLOVER, &state_266, NULL, KC_NO, 0, single_dance};
uint8_t state_267 = IDLE;
const struct Chord chord_267 PROGMEM = {H_TOP2, PLOVER, &state_267, NULL, KC_Q, 0, single_dance};
uint8_t state_268 = IDLE;
const struct Chord chord_268 PROGMEM = {H_TOP3, PLOVER, &state_268, NULL, KC_W, 0, single_dance};
uint8_t state_269 = IDLE;
const struct Chord chord_269 PROGMEM = {H_TOP4, PLOVER, &state_269, NULL, KC_E, 0, single_dance};
uint8_t state_270 = IDLE;
const struct Chord chord_270 PROGMEM = {H_TOP5, PLOVER, &state_270, NULL, KC_R, 0, single_dance};
uint8_t state_271 = IDLE;
const struct Chord chord_271 PROGMEM = {H_TOP6, PLOVER, &state_271, NULL, KC_T, 0, single_dance};
uint8_t state_272 = IDLE;
const struct Chord chord_272 PROGMEM = {H_TOP7, PLOVER, &state_272, NULL, KC_Y, 0, single_dance};
uint8_t state_273 = IDLE;
const struct Chord chord_273 PROGMEM = {H_TOP8, PLOVER, &state_273, NULL, KC_U, 0, single_dance};
uint8_t state_274 = IDLE;
const struct Chord chord_274 PROGMEM = {H_TOP9, PLOVER, &state_274, NULL, KC_I, 0, single_dance};
uint8_t state_275 = IDLE;
const struct Chord chord_275 PROGMEM = {H_TOP10, PLOVER, &state_275, NULL, KC_O, 0, single_dance};
uint8_t state_276 = IDLE;
const struct Chord chord_276 PROGMEM = {H_TOP11, PLOVER, &state_276, NULL, KC_P, 0, single_dance};
uint8_t state_277 = IDLE;
const struct Chord chord_277 PROGMEM = {H_TOP12, PLOVER, &state_277, NULL, KC_LBRACKET, 0, single_dance};
uint8_t state_278 = IDLE;
const struct Chord chord_278 PROGMEM = {H_BOT1, PLOVER, &state_278, NULL, KC_NO, 0, single_dance};
uint8_t state_279 = IDLE;
const struct Chord chord_279 PROGMEM = {H_BOT2, PLOVER, &state_279, NULL, KC_A, 0, single_dance};
uint8_t state_280 = IDLE;
const struct Chord chord_280 PROGMEM = {H_BOT3, PLOVER, &state_280, NULL, KC_S, 0, single_dance};
uint8_t state_281 = IDLE;
const struct Chord chord_281 PROGMEM = {H_BOT4, PLOVER, &state_281, NULL, KC_D, 0, single_dance};
uint8_t state_282 = IDLE;
const struct Chord chord_282 PROGMEM = {H_BOT5, PLOVER, &state_282, NULL, KC_F, 0, single_dance};
uint8_t state_283 = IDLE;
const struct Chord chord_283 PROGMEM = {H_BOT6, PLOVER, &state_283, NULL, KC_G, 0, single_dance};
uint8_t state_284 = IDLE;
const struct Chord chord_284 PROGMEM = {H_BOT7, PLOVER, &state_284, NULL, KC_H, 0, single_dance};
uint8_t state_285 = IDLE;
const struct Chord chord_285 PROGMEM = {H_BOT8, PLOVER, &state_285, NULL, KC_J, 0, single_dance};
uint8_t state_286 = IDLE;
const struct Chord chord_286 PROGMEM = {H_BOT9, PLOVER, &state_286, NULL, KC_K, 0, single_dance};
uint8_t state_287 = IDLE;
const struct Chord chord_287 PROGMEM = {H_BOT10, PLOVER, &state_287, NULL, KC_L, 0, single_dance};
uint8_t state_288 = IDLE;
const struct Chord chord_288 PROGMEM = {H_BOT11, PLOVER, &state_288, NULL, KC_SCOLON, 0, single_dance};
uint8_t state_289 = IDLE;
const struct Chord chord_289 PROGMEM = {H_BOT12, PLOVER, &state_289, NULL, KC_QUOTE, 0, single_dance};
uint8_t state_290 = IDLE;
const struct Chord chord_290 PROGMEM = {H_THU1, PLOVER, &state_290, NULL, KC_1, 0, single_dance};
uint8_t state_291 = IDLE;
const struct Chord chord_291 PROGMEM = {H_THU2, PLOVER, &state_291, NULL, KC_C, 0, single_dance};
uint8_t state_292 = IDLE;
const struct Chord chord_292 PROGMEM = {H_THU3, PLOVER, &state_292, NULL, KC_V, 0, single_dance};
uint8_t state_293 = IDLE;
const struct Chord chord_293 PROGMEM = {H_THU4, PLOVER, &state_293, NULL, KC_N, 0, single_dance};
uint8_t state_294 = IDLE;
const struct Chord chord_294 PROGMEM = {H_THU5, PLOVER, &state_294, NULL, KC_M, 0, single_dance};
uint8_t state_295 = IDLE;
const struct Chord chord_295 PROGMEM = {H_THU6, PLOVER, &state_295, NULL, KC_1, 0, single_dance};
uint8_t state_296 = IDLE;
const struct Chord chord_296 PROGMEM = {H_TOP1 + H_BOT1, PLOVER, &state_296, NULL, BEAKL, 0, perm_pseudolayer};

const struct Chord* const list_of_chords[] PROGMEM = {
    &chord_0, &chord_1, &chord_2, &chord_3, &chord_4, &chord_5, &chord_6, &chord_7, &chord_8, &chord_9, &chord_10, &chord_11, &chord_12, &chord_13, &chord_14, &chord_15, &chord_16, &chord_17, &chord_18, &chord_19, &chord_20, &chord_21, &chord_22, &chord_23, &chord_24, &chord_25, &chord_26, &chord_27, &chord_28, &chord_29, &chord_30, &chord_31, &chord_32, &chord_33, &chord_34, &chord_35, &chord_36, &chord_37, &chord_38, &chord_39, &chord_40, &chord_41, &chord_42, &chord_43, &chord_44, &chord_45, &chord_46, &chord_47, &chord_48, &chord_49, &chord_50, &chord_51, &chord_52, &chord_53, &chord_54, &chord_55, &chord_56, &chord_57, &chord_58, &chord_59, &chord_60, &chord_61, &chord_62, &chord_63, &chord_64, &chord_65, &chord_66, &chord_67, &chord_68, &chord_69, &chord_70, &chord_71, &chord_72, &chord_73, &chord_74, &chord_75, &chord_76, &chord_77, &chord_78, &chord_79, &chord_80, &chord_81, &chord_82, &chord_83, &chord_84, &chord_85, &chord_86, &chord_87, &chord_88, &chord_89, &chord_90, &chord_91, &chord_92, &chord_93, &chord_94, &chord_95, &chord_96, &chord_97, &chord_98, &chord_99, &chord_100, &chord_101, &chord_102, &chord_103, &chord_104, &chord_105, &chord_106, &chord_107, &chord_108, &chord_109, &chord_110, &chord_111, &chord_112, &chord_113, &chord_114, &chord_115, &chord_116, &chord_117, &chord_118, &chord_119, &chord_120, &chord_121, &chord_122, &chord_123, &chord_124, &chord_125, &chord_126, &chord_127, &chord_128, &chord_129, &chord_130, &chord_131, &chord_132, &chord_133, &chord_134, &chord_135, &chord_136, &chord_137, &chord_138, &chord_139, &chord_140, &chord_141, &chord_142, &chord_143, &chord_144, &chord_145, &chord_146, &chord_147, &chord_148, &chord_149, &chord_150, &chord_151, &chord_152, &chord_153, &chord_154, &chord_155, &chord_156, &chord_157, &chord_158, &chord_159, &chord_160, &chord_161, &chord_162, &chord_163, &chord_164, &chord_165, &chord_166, &chord_167, &chord_168, &chord_169, &chord_170, &chord_171, &chord_172, &chord_173, &chord_174, &chord_175, &chord_176, &chord_177, &chord_178, &chord_179, &chord_180, &chord_181, &chord_182, &chord_183, &chord_184, &chord_185, &chord_186, &chord_187, &chord_188, &chord_189, &chord_190, &chord_191, &chord_192, &chord_193, &chord_194, &chord_195, &chord_196, &chord_197, &chord_198, &chord_199, &chord_200, &chord_201, &chord_202, &chord_203, &chord_204, &chord_205, &chord_206, &chord_207, &chord_208, &chord_209, &chord_210, &chord_211, &chord_212, &chord_213, &chord_214, &chord_215, &chord_216, &chord_217, &chord_218, &chord_219, &chord_220, &chord_221, &chord_222, &chord_223, &chord_224, &chord_225, &chord_226, &chord_227, &chord_228, &chord_229, &chord_230, &chord_231, &chord_232, &chord_233, &chord_234, &chord_235, &chord_236, &chord_237, &chord_238, &chord_239, &chord_240, &chord_241, &chord_242, &chord_243, &chord_244, &chord_245, &chord_246, &chord_247, &chord_248, &chord_249, &chord_250, &chord_251, &chord_252, &chord_253, &chord_254, &chord_255, &chord_256, &chord_257, &chord_258, &chord_259, &chord_260, &chord_261, &chord_262, &chord_263, &chord_264, &chord_265, &chord_266, &chord_267, &chord_268, &chord_269, &chord_270, &chord_271, &chord_272, &chord_273, &chord_274, &chord_275, &chord_276, &chord_277, &chord_278, &chord_279, &chord_280, &chord_281, &chord_282, &chord_283, &chord_284, &chord_285, &chord_286, &chord_287, &chord_288, &chord_289, &chord_290, &chord_291, &chord_292, &chord_293, &chord_294, &chord_295, &chord_296
};

const uint16_t** const leader_triggers PROGMEM = NULL;
void (*leader_functions[]) (void) = {};

#define NUMBER_OF_CHORDS 297
#define NUMBER_OF_LEADER_COMBOS 0

bool are_hashed_keycodes_in_sound(HASH_TYPE keycodes_hash, HASH_TYPE sound) {
    return (keycodes_hash & sound) == keycodes_hash;
}

uint8_t keycode_to_index(uint16_t keycode) {
    return keycode - FIRST_INTERNAL_KEYCODE;
}

void sound_keycode_array(uint16_t keycode) {
    uint8_t index = keycode_to_index(keycode);
    keycode_index++;
    keycodes_buffer_array[index] = keycode_index;
}

void silence_keycode_hash_array(HASH_TYPE keycode_hash) {
    for (int i = 0; i < NUMBER_OF_KEYS; i++) {
        bool index_in_hash = ((HASH_TYPE) 1 << i) & keycode_hash;
        if (index_in_hash) {
            uint8_t current_val = keycodes_buffer_array[i];
            keycodes_buffer_array[i] = 0;
            for (int j = 0; j < NUMBER_OF_KEYS; j++) {
                if (keycodes_buffer_array[j] > current_val) {
                    keycodes_buffer_array[j]--;
                }
            }
            keycode_index--;
        }
    }
}

bool are_hashed_keycodes_in_array(HASH_TYPE keycode_hash) {
    for (int i = 0; i < NUMBER_OF_KEYS; i++) {
        bool index_in_hash = ((HASH_TYPE) 1 << i) & keycode_hash;
        bool index_in_array = (bool) keycodes_buffer_array[i];
        if (index_in_hash && !index_in_array) {
            return false;
        }
    }
    return true;
}

void kill_one_shots(void) {
    struct Chord chord_storage;
    struct Chord* chord_ptr;
    struct Chord* chord;
    
    for (int i = 0; i < NUMBER_OF_CHORDS; i++) {
        chord_ptr = (struct Chord*) pgm_read_word (&list_of_chords[i]);
        memcpy_P(&chord_storage, chord_ptr, sizeof(struct Chord));
        chord = &chord_storage;
        
        if (*chord->state == IN_ONE_SHOT) {
            *chord->state = RESTART;
            chord->function(chord);
            if (*chord->state == RESTART) {
                *chord->state = IDLE;
            }
        }
    }
}

void process_finished_dances(void) {
    struct Chord chord_storage;
    struct Chord* chord_ptr;
    struct Chord* chord;
    
    for (int i = 0; i < NUMBER_OF_CHORDS; i++) {
        chord_ptr = (struct Chord*) pgm_read_word (&list_of_chords[i]);
        memcpy_P(&chord_storage, chord_ptr, sizeof(struct Chord));
        chord = &chord_storage;
        
        if (*chord->state == ACTIVATED) {
            *chord->state = PRESS_FROM_ACTIVE;
            chord->function(chord);
            if (a_key_went_through) {
                kill_one_shots();
            }
            dance_timer = timer_read();
        } else if (*chord->state == IDLE_IN_DANCE) {
            *chord->state = FINISHED;
            chord->function(chord);
            if (*chord->state == FINISHED) {
                *chord->state = RESTART;
                if (*chord->state == RESTART) {
                    *chord->state = IDLE;
                }
            }
        } else if (*chord->state == PRESS_FROM_ACTIVE) {
            *chord->state = FINISHED_FROM_ACTIVE;
            chord->function(chord);
            if (a_key_went_through) {
                kill_one_shots();
            }
            dance_timer = timer_read();
        }
    }
}

uint8_t keycodes_buffer_array_min(uint8_t* first_keycode_index) {
    for (int i = 0; i < NUMBER_OF_KEYS; i++) {
        if (keycodes_buffer_array[i] == 1) {
            if (first_keycode_index != NULL) {
                *first_keycode_index = (uint8_t) i;
            }
            return 1;
        }
    }
    return 0;
}

void remove_subchords(void) {
    struct Chord chord_storage;
    struct Chord* chord_ptr;
    struct Chord* chord;
    
    for (int i = 0; i < NUMBER_OF_CHORDS; i++) {
        chord_ptr = (struct Chord*) pgm_read_word (&list_of_chords[i]);
        memcpy_P(&chord_storage, chord_ptr, sizeof(struct Chord));
        chord = &chord_storage;
        
        if (!(*chord->state == READY || *chord->state == READY_IN_DANCE || *chord->state == READY_LOCKED)) {
            continue;
        }
        
        struct Chord chord_storage_2;
        struct Chord* chord_ptr_2;
        struct Chord* chord_2;
        for (int j = 0; j < NUMBER_OF_CHORDS; j++) {
            if (i == j) {continue;}
            
            chord_ptr_2 = (struct Chord*) pgm_read_word (&list_of_chords[j]);
            memcpy_P(&chord_storage_2, chord_ptr_2, sizeof(struct Chord));
            chord_2 = &chord_storage_2;
            
            if (are_hashed_keycodes_in_sound(chord_2->keycodes_hash, chord->keycodes_hash)) {
                if (*chord_2->state == READY) {
                    *chord_2->state = IDLE;
                }
                if (*chord_2->state == READY_IN_DANCE) {
                    *chord_2->state = IDLE_IN_DANCE;
                }
                if (*chord_2->state == READY_LOCKED) {
                    *chord_2->state = LOCKED;
                }
            }
        }
    }
}

void process_ready_chords(void) {
    uint8_t first_keycode_index = 0;
    while (keycodes_buffer_array_min(&first_keycode_index)) {
        // find ready chords
        struct Chord chord_storage;
        struct Chord* chord_ptr;
        struct Chord* chord;
        
        for (int i = 0; i < NUMBER_OF_CHORDS; i++) {
            chord_ptr = (struct Chord*) pgm_read_word (&list_of_chords[i]);
            memcpy_P(&chord_storage, chord_ptr, sizeof(struct Chord));
            chord = &chord_storage;
            
            // if the chord does not contain the first keycode
            bool contains_first_keycode = ((uint32_t) 1 << first_keycode_index) & chord->keycodes_hash;
            if (!contains_first_keycode) {
                continue;
            }
            
            if (!are_hashed_keycodes_in_array(chord->keycodes_hash)){
                continue;
            }
            
            if (*chord->state == LOCKED) {
                *chord->state = READY_LOCKED;
                continue;
            }
            
            if (!(chord->pseudolayer == current_pseudolayer || chord->pseudolayer == ALWAYS_ON)) {
                continue;
            }
            
            if (*chord->state == IDLE) {
                *chord->state = READY;
                continue;
            }
            
            if (*chord->state == IDLE_IN_DANCE) {
                *chord->state = READY_IN_DANCE;
            }
        }
        
        // remove subchords
        remove_subchords();
        
        // execute logic
        // this should be only one chord
        for (int i = 0; i < NUMBER_OF_CHORDS; i++) {
            chord_ptr = (struct Chord*) pgm_read_word (&list_of_chords[i]);
            memcpy_P(&chord_storage, chord_ptr, sizeof(struct Chord));
            chord = &chord_storage;
            
            if (*chord->state == READY_LOCKED) {
                *chord->state = RESTART;
                chord->function(chord);
                if (*chord->state == RESTART) {
                    *chord->state = IDLE;
                }
                break;
            }
            
            if (*chord->state == READY || *chord->state == READY_IN_DANCE) {
                if (last_chord && last_chord != chord) {
                    process_finished_dances();
                }
                
                bool lock_next_prev_state = lock_next;
                
                *chord->state = ACTIVATED;
                chord->function(chord);
                dance_timer = timer_read();
                
                if (lock_next && lock_next == lock_next_prev_state) {
                    lock_next = false;
                    *chord->state = PRESS_FROM_ACTIVE;
                    chord->function(chord);
                    if (*chord->state == PRESS_FROM_ACTIVE) {
                        *chord->state = LOCKED;
                    }
                    if (a_key_went_through) {
                        kill_one_shots();
                    }
                }
                break;
            }
        }
        
        // silence notes
        silence_keycode_hash_array(chord->keycodes_hash);
    }
}

void deactivate_active_chords(uint16_t keycode) {
    HASH_TYPE hash = (HASH_TYPE)1 << (keycode - SAFE_RANGE);
    bool broken;
    struct Chord chord_storage;
    struct Chord* chord_ptr;
    struct Chord* chord;
    
    for (int i = 0; i < NUMBER_OF_CHORDS; i++) {
        chord_ptr = (struct Chord*) pgm_read_word (&list_of_chords[i]);
        memcpy_P(&chord_storage, chord_ptr, sizeof(struct Chord));
        chord = &chord_storage;
        
        broken = are_hashed_keycodes_in_sound(hash, chord->keycodes_hash);
        if (!broken) {
            continue;
        }
        
        switch (*chord->state) {
            case ACTIVATED:
                *chord->state = DEACTIVATED;
                chord->function(chord);
                
                if (*chord->state == DEACTIVATED) {
                    dance_timer = timer_read();
                    *chord->state = IDLE_IN_DANCE;
                }
                if (*chord->state != IN_ONE_SHOT) {
                    kill_one_shots();
                }
                break;
            case PRESS_FROM_ACTIVE:
            case FINISHED_FROM_ACTIVE:
                *chord->state = RESTART;
                chord->function(chord);
                if (*chord->state == RESTART) {
                    *chord->state = IDLE;
                }
                kill_one_shots();
                break;
            default:
                break;
        }
    }
    
}

void process_command(void) {
    command_mode = 0;
    for (int i = 0; i < COMMAND_MAX_LENGTH; i++) {
        if (command_buffer[i]) {
            register_code(command_buffer[i]);
        }
        send_keyboard_report();
    }
    wait_ms(TAP_TIMEOUT);
    for (int i = 0; i < COMMAND_MAX_LENGTH; i++) {
        if (command_buffer[i]) {
            unregister_code(command_buffer[i]);
        }
        send_keyboard_report();
    }
    for (int i = 0; i < COMMAND_MAX_LENGTH; i++) {
        command_buffer[i] = 0;
    }
    command_ind = 0;
}

void process_leader(void) {
    in_leader_mode = false;
    for (int i = 0; i < NUMBER_OF_LEADER_COMBOS; i++) {
        uint16_t trigger[LEADER_MAX_LENGTH];
        memcpy_P(trigger, leader_triggers[i], LEADER_MAX_LENGTH * sizeof(uint16_t));
        
        if (identical(leader_buffer, trigger)) {
            (*leader_functions[i])();
            break;
        }
    }
    for (int i = 0; i < LEADER_MAX_LENGTH; i++) {
        leader_buffer[i] = 0;
    }
}

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
    if (keycode < FIRST_INTERNAL_KEYCODE || keycode > LAST_INTERNAL_KEYCODE) {
        return true;
    }
    
    if (record->event.pressed) {
        sound_keycode_array(keycode);
    } else {
        process_ready_chords();
        deactivate_active_chords(keycode);
    }
    chord_timer = timer_read();
    leader_timer = timer_read();
    
    return false;
}

void matrix_scan_user(void) {
    bool chord_timer_expired = timer_elapsed(chord_timer) > CHORD_TIMEOUT;
    if (chord_timer_expired && keycodes_buffer_array_min(NULL)) {
        process_ready_chords();
    }
    
    bool dance_timer_expired = timer_elapsed(dance_timer) > DANCE_TIMEOUT;
    if (dance_timer_expired) { // would love to have && in_dance but not sure how
        process_finished_dances();
    }
    
    bool in_command_mode = command_mode == 2;
    if (in_command_mode) {
        process_command();
    }
    
    bool leader_timer_expired = timer_elapsed(leader_timer) > LEADER_TIMEOUT;
    if (leader_timer_expired && in_leader_mode) {
        process_leader();
    }
    
}

void clear(const struct Chord* self) {
    if (*self->state == ACTIVATED) {
        // kill all chords
        struct Chord chord_storage;
        struct Chord* chord_ptr;
        struct Chord* chord;
        
        for (int i = 0; i < NUMBER_OF_CHORDS; i++) {
            chord_ptr = (struct Chord*) pgm_read_word (&list_of_chords[i]);
            memcpy_P(&chord_storage, chord_ptr, sizeof(struct Chord));
            chord = &chord_storage;
            
            *chord->state = IDLE;
            
            if (chord->counter) {
                *chord->counter = 0;
            }
        }
        
        // clear keyboard
        clear_keyboard();
        send_keyboard_report();
        
        // switch to default pseudolayer
        current_pseudolayer = DEFAULT_PSEUDOLAYER;
        
        // clear all keyboard states
        lock_next = false;
        autoshift_mode = true;
        command_mode = 0;
        in_leader_mode = false;
        leader_ind = 0;
        dynamic_macro_mode = false;
        a_key_went_through = false;
        
        for (int i = 0; i < DYNAMIC_MACRO_MAX_LENGTH; i++) {
            dynamic_macro_buffer[i] = 0;
        }
    }
}
