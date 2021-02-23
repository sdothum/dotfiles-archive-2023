// sdothum - 2016 (c) wtfpl

#include "config.h"  // for ale linter
#include "keycode_functions.h"

// ...................................................................... Global

static uint8_t i = 0;           // inline for loop counter

// .............................................................. Keyboard State

#define CLR_1SHOT clear_oneshot_layer_state(ONESHOT_PRESSED)

#define LEFT      1             // keyboard hand side
#define RIGHT     2             // for (LEFT | RIGHT) bit test

#define UPPER     1             // case
#define LOWER     0

#define ONDOWN    0             // see raise_layer()
#define INVERT    1

// ................................................................ Tapping Term

static uint16_t key_timer = 0;  // global event timer

#define START_TIMER  key_timer = timer_read()
#define CLEAR_TIMER  key_timer = 0

#ifdef ROLLING
#ifndef ROLLING_TERM
// longer TAPPING_TERM to prevent false rolling GUI CTL ALT (workflow) triggering, see mod_roll()
#define ROLLING_TERM TAPPING_TERM + TAPPING_TERM / 3
#endif

uint16_t tapping_term(uint16_t keycode)
{
  switch (keycode) {
  case HOME_Q:
  case HOME_H:
  case HOME_E:
  case HOME_R:
  case HOME_S:
  case PINKY2:  return ROLLING_TERM;
  case HOME_A:
  case HOME_T:
  default:      return TAPPING_TERM;
  }
}

// ............................................................... Keycode State

static uint16_t keycode = 0;  // default keycode for when tapping_term() macro substitution has none!

#define KEY_TAPPED(t) (timer_elapsed(t) < tapping_term(keycode))
#else
#define KEY_TAPPED(t) (timer_elapsed(t) < TAPPING_TERM)
#endif
#define KEY_TAP       KEY_TAPPED(key_timer)

#define KEY_DOWN      record->event.pressed
#define KEY_UP        !KEY_DOWN

// Keycodes
// ═════════════════════════════════════════════════════════════════════════════

// .......................................................... Keycode Primitives

#define TAP(k)         register_code(k); unregister_code(k)
#define TAP_SHIFT(k)   register_code(KC_LSFT); TAP(k); unregister_code(KC_LSFT)
#define TAP_CASE(u, k) if (u) { TAP_SHIFT(k); } else { TAP(k); }

void type(RECORD, bool upcase, uint16_t keycode)
{
  if (KEY_DOWN) {
    if (upcase) { register_code(KC_LSFT); }
    register_code(keycode);
  } else {
    unregister_code(keycode);
    if (upcase) { unregister_code(KC_LSFT); }
  }
}

#ifdef SPLITOGRAPHY
void press(RECORD, bool upcase, uint16_t keycode)
{
  if (key_press(record)) { TAP_CASE(upcase, keycode); }
}
#endif

#ifdef PINKIE_STAGGER
void toggle(RECORD, uint16_t modifier, uint16_t keycode)
{
  if (KEY_DOWN) { START_TIMER; register_code(modifier); }
  else          { unregister_code(modifier); if (KEY_TAP) { TAP(keycode); }; CLEAR_TIMER; }
}
#endif

// ................................................................... Key event

static uint16_t tt_keycode = 0;  // current TT state (keycode)

// alternate escape for TT layers, see process_record_user()
void tt_escape(RECORD, uint16_t keycode)
{
  if (tt_keycode && tt_keycode != keycode) { base_layer(0); }  // if different TT layer selected
  if (key_press(record))                   { tt_keycode = keycode; }
}

// tapped or not?
bool key_press(RECORD)
{
  if (KEY_DOWN)     { START_TIMER; }
  else if (KEY_TAP) { CLEAR_TIMER; return true; }
  else              { CLEAR_TIMER; }
  return false;
}

// Modifiers
// ═════════════════════════════════════════════════════════════════════════════

// ................................................................... Mod Masks

// tap dance persistant mods, see process_record_user()
// keyboard_report->mods (?) appears to be cleared by tap dance
static uint8_t mods = 0;

#define MOD_DOWN(k) (mods & MOD_BIT(k))
#define MOD_BITS(k) if (KEY_DOWN) { mods |= MOD_BIT(k); } else { mods &= ~(MOD_BIT(k)); }

// ......................................................... Modifier Primitives

// mods only needed for map_shift() and map_shifted()
#define MOD(k)     register_code  (k); MOD_BITS(k)
#define UNMOD(k)   unregister_code(k); MOD_BITS(k)

// smart chording (0) none (KC_*) modifier keycode (MOD_* | ..) compound modifier bitcode
#define CHORD(k)   if (k) { if (IS_MOD(k)) { MOD  (k); } else { register_mods  ((uint8_t) k); } }
#define UNCHORD(k) if (k) { if (IS_MOD(k)) { UNMOD(k); } else { unregister_mods((uint8_t) k); } }

// ALT_T, CTL_T, GUI_T, SFT_T for shifted keycodes
void mod_tap(RECORD, uint16_t modifier, bool upcase, uint16_t keycode)
{
  if (KEY_DOWN) {
    START_TIMER;
    CHORD(modifier);
  } else {
    UNCHORD(modifier);
    if (KEY_TAP) { TAP_CASE(upcase, keycode); }
    CLEAR_TIMER;
  }
}

// Key Sequence Handling
// ═════════════════════════════════════════════════════════════════════════════

// ....................................................... Leader Capitalization

static bool leadercap  = 0;  // substitute (0) keycode (1) leader + oneshot_SHIFT, see leader_cap()

// LT (LAYER, KEY) -> <leader><SHIFT>, see process_record_user() and TD_TILD, KC_EXLM, KC_QUES
bool leader_cap(RECORD, uint8_t layer, uint16_t keycode)
{
  if (leadercap) {
    if (KEY_DOWN) { START_TIMER; return false; }
    else if (KEY_TAP) {
      TAP(keycode);
      oneshot_shift(layer);
      CLEAR_TIMER;
      return true;
    }
    CLEAR_TIMER;
  }
  return false;
}

// ................................................................ Rolling Keys

#ifdef ROLLING
uint16_t modifier = 0;           // absent default for SET_EVENT()

#define SET_EVENT(c) e[c].START_TIMER;           \
                     e[c].keycode   = keycode;   \
                     e[c].shift     = (modifier == KC_LSFT || modifier == KC_RSFT); \
                     e[c].side      = (column <= 4) ? LEFT : RIGHT;                 \
                     e[c].leadercap = leadercap; \
                     prev_key       = next_key;  \
                     next_key       = c

// ortholinear column numbering ignores toggle layout key columns
// column 0 1 2 3 4 <- left, right -> 5 6 7 8 9
static struct   column_event {
       uint16_t key_timer;       // event priority
       uint16_t keycode;
       bool     shift;
       uint8_t  side;
       bool     leadercap;
} e[12];                         // leader -> 10 11, see process_record_user(), mod_roll()

void clear_events(void)
{
  for (i = 0; i < 12; i++) { e[i].CLEAR_TIMER; e[i].leadercap = 0; }
}

#define LEADER 10                // and 11 are leader columns (assigned to SPC and ENT)
#define LSHIFT 3                 // left shift column
#define RSHIFT 6                 // right shift column

static uint8_t leaderlayer = 0;  // thumb key's toggle layer, see process_record_user()
static uint8_t next_key    = 0;  // by column reference
static uint8_t prev_key    = 0;

#define ONSHIFT(c)   (e[c].shift && e[c].key_timer < e[column].key_timer && e[column].side == ((c == LSHIFT) ? RIGHT : LEFT))
#define ROLL         if (upcase || ONSHIFT(LSHIFT) || ONSHIFT(RSHIFT)) { TAP_SHIFT(keycode); } else { TAP(keycode); }
#define RESTORE(k)   if (MOD_DOWN(k)) { register_code(k); }

#define SHIFT_KEY(c) (c == LSHIFT || c == RSHIFT)
// apply rolling shift to opposite hand (0) for all keys (1) opposite shift key only
#define SHIFT_KEYS   (!ROLLING || (ROLLING && SHIFT_KEY(column) && SHIFT_KEY(next_key)))

void roll_key(bool upcase, uint16_t keycode, uint8_t column)
{
  if (e[column].key_timer < e[next_key].key_timer) {                            // rolling sequence in progress
    clear_mods();                                                               // disable modifier chord finger rolls
    if (e[column].shift && e[column].side != e[next_key].side && SHIFT_KEYS) {  // shift only opposite side of rolling sequence
      TAP_SHIFT(e[next_key].keycode);                                           // shift opposite home row key
      e[next_key].CLEAR_TIMER;                                                  // don't echo this shift key
    } else { ROLL; }                                                            // tap (shifted?) key
    RESTORE(KC_RSFT);                                                           // restore shift for map_shift(), see UNIX
    RESTORE(KC_LSFT);                                                           // .. for completeness
  } else   { ROLL; e[prev_key].CLEAR_TIMER; e[column].leadercap = 0; }          // don't echo preceeding modifier key
}

#define CLEAR_EVENT e[column].key_timer   = 0; \
                    e[column].shift       = 0; \
                    e[prev_key].leadercap = 0; \
                    leaderlayer           = 0

// handle rolling keys as shift keycode, a sequence of unmodified keycodes, or keycode leader oneshot_SHIFT
bool mod_roll(RECORD, uint16_t modifier, bool upcase, uint16_t keycode, uint8_t column)
{
  if (KEY_DOWN) {
    SET_EVENT(column);
    CHORD(modifier);
  } else {
    UNCHORD(modifier);
    if (KEY_TAPPED(e[column].key_timer)) {
      roll_key(upcase, keycode, column);
      if (e[prev_key].leadercap && column >= LEADER) {  // punctuation leader capitalization chord?
        oneshot_shift(leaderlayer);
        CLEAR_EVENT;
        return true;
      }
    }
    CLEAR_EVENT;
  }
  return false;
}

// Key Mapping
// ═════════════════════════════════════════════════════════════════════════════

// ................................................................. Map Keycode

// handle map_shift() rolling keys (and dot chords)
void set_leadercap(RECORD, uint16_t keycode, uint8_t column)
{
  if (KEY_DOWN) { SET_EVENT(column); }
  else          { e[column].leadercap = 0; }  // clear leader capitalization, see mod_roll()
}

bool map_shift_event(RECORD, uint16_t sftcode, bool upcase, uint16_t keycode, uint8_t column)
{
  set_leadercap(record, keycode, column);
  return map_shift(record, sftcode, upcase, keycode);
}
#endif

static uint8_t map = 0;  // map state

// remap keycode via shift for base and caps layers
bool map_shift(RECORD, uint16_t sftcode, bool upcase, uint16_t keycode)
{
  if (map || MOD_DOWN(sftcode)) {
    if (KEY_DOWN) {
      if (!upcase) { unregister_code(sftcode); }  // in event of unshifted keycode
      register_code(keycode);
      map = 1;                // in case shift key is released first
#ifdef ROLLING
      e[RSHIFT].CLEAR_TIMER;  // clear punctuation modifier (key tap), see mod_roll()
#endif
    } else {
      unregister_code(keycode);
      if (!upcase) { register_code(sftcode); }    // restore shift
      map = 0;
    }
    CLEAR_TIMER;              // clear home row shift, see process_record_user()
#ifdef ROLLING
    e[LSHIFT].CLEAR_TIMER;    // clear left handed separator modifier (key tap)
#endif
    return true;
  }
  return false;
}

#ifdef ROLLING
// remap keycode with rolling timing (special case for same hand remap loses autorepeat)
bool roll_shift(RECORD, uint16_t sftcode, bool upcase, uint16_t keycode, uint8_t column)
{
  if (map || MOD_DOWN(sftcode)) {
    if (KEY_DOWN) { SET_EVENT(column); }
    else if (KEY_TAPPED(e[column].key_timer)) {
      if (!upcase) { unregister_code(sftcode); }  // in event of unshifted keycode
      roll_key(upcase, keycode, column);
      if (!upcase) { register_code(sftcode); }    // restore shift
    }
    return true;
  }
  return false;
}
#endif

// Layers
// ═════════════════════════════════════════════════════════════════════════════

// ............................................................ Layer Primitives

// void persistant_default_layer_set(uint16_t default_layer)
// {
//   eeconfig_update_default_layer(default_layer);
//   default_layer_set            (default_layer);
// }

void clear_layers(void)
{
  for (i = 0; i < _END_LAYERS; i++) { layer_off(i); }
  mods       = 0;
  tt_keycode = 0;
  CLEAR_TIMER;
#ifdef ROLLING
  clear_events();
#endif
}

void base_layer(uint8_t defer)
{
  if (defer) { return; }  // see process_record_user() reset keys
#ifdef AUDIO_ENABLE
  plover ? PLAY_SONG(song_plover_gb) : PLAY_SONG(song_qwerty);
#endif
  clear_layers();
  set_single_persistent_default_layer(_BASE);
#ifdef STENO_ENABLE
  toggle_plover(0);
#endif
}

// LT macro for map_shifted(), see process_record_user()
void layer_toggle(RECORD, uint8_t layer, bool upcase, uint16_t keycode)
{
  if (KEY_DOWN) { START_TIMER; layer_on(layer); }
  else {
    layer_off(layer);
    if (KEY_TAP) { TAP_CASE(upcase, keycode); }
    // clear_mods();
    CLEAR_TIMER;
  }
}

void oneshot_shift(uint8_t layer)
{
  if (layer) { layer_off(layer); }           // disable key's assigned toggle layer
  layer_on         (_SHIFT);                 // sentence/paragraph capitalization
  set_oneshot_layer(_SHIFT, ONESHOT_START);  // see process_record_user() -> clear_oneshot_layer_state(ONESHOT_PRESSED)
}

// ............................................................ Double Key Layer

static uint8_t double_key = 0;

// dual key to raise layer (layer 0 to trap dual key state :-)
bool raise_layer(RECORD, uint8_t layer, uint8_t side, bool invert)
{
  if (KEY_DOWN) {
    double_key |= side;
    if (double_key == (LEFT | RIGHT)) {
      if (layer) { invert ? layer_invert(layer) : layer_on(layer); }
      return true;
    }
  } else {
    double_key &= ~side;
    if (!(double_key || invert)) { layer_off(layer); }  // allow single key to continue on layer :-)
  }
  return false;
}

// Steno
// ═════════════════════════════════════════════════════════════════════════════

#ifdef SPLITOGRAPHY
// .............................................................. Rolling Layers

// rolling thumb combinations, see process_record_user()
// up,   up   -> _BASE
// up,   down -> _SYMGUI
// down, up   -> _REGEX
// down, down -> _MOUSE, see layer keycodes that raise mouse layer

static uint8_t leftside  = 0;
static uint8_t rightside = 0;

#define SWITCH_LAYER(x, y) layer_off(x); x = 0; if (y && y == _MOUSE) { layer_on(facing); y = facing; }

// seamlessly switch left / right thumb layer combinations
void rolling_layer(RECORD, uint8_t side, uint8_t layer, uint8_t facing)
{
  if (KEY_DOWN) {
    layer_on(layer);
    if (side == LEFT) { leftside  = layer; }
    else              { rightside = layer; }
  } else {
    layer_off(_MOUSE);
    if (side == LEFT) { SWITCH_LAYER(leftside, rightside); }
    else              { SWITCH_LAYER(rightside, leftside); }
  }
}
#endif

// ...................................................................... Plover

#ifdef STENO_ENABLE
void steno(RECORD)
{
  if (KEY_DOWN) {
#ifdef AUDIO_ENABLE
    PLAY_SONG(song_plover);
#endif
    clear_layers();
    layer_on(_PLOVER);
#ifndef STENO_ENABLE
    if (!eeconfig_is_enabled()) { eeconfig_init(); }
    keymap_config.raw  = eeconfig_read_keymap();
    keymap_config.nkro = 1;
    eeconfig_update_keymap(keymap_config.raw);
#endif
    toggle_plover(1);
  }
}

static uint8_t plover = 0;  // plover application run state (0) off (1) on, see wm keybinds

void toggle_plover(uint8_t state)
{
  if (plover != state) {
#ifdef PLOVER_KEYBIND
#include "plover_keybind.h"
#endif
    plover = state;
  }
}
#endif
