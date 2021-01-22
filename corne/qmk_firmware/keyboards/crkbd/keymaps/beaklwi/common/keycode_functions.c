// sdothum - 2016 (c) wtfpl

#include "config.h"  // for ale linter
#include "keycode_functions.h"

// ................................................................ Global Scope

static bool     leadercap  = 0;  // substitute (0) keycode (1) leader + oneshot_SHIFT, see leader_cap()
static bool     reshifted  = 0;  // SFT_T timing trap, see map_shift(), process_record_user()
static uint16_t tt_keycode = 0;  // current TT state (keycode)

#define CLR_1SHOT clear_oneshot_layer_state(ONESHOT_PRESSED)
#define KEY_DOWN  record->event.pressed

#define LEFT      1              // keyboard hand side
#define RIGHT     2              // for (LEFT | RIGHT) bit test

#define UPPER     1              // case
#define LOWER     0

#define ONDOWN    0              // see raise_layer()
#define INVERT    1

// ................................................................. Local Scope

static uint8_t  i         = 0;   // inline for loop counter
static uint16_t key_timer = 0;   // global event timer

#define START_TIMER   key_timer = timer_read()
#define CLEAR_TIMER   key_timer = 0
#define KEY_TAPPED(t) (timer_elapsed(t) < TAPPING_TERM)
#define KEY_TAP       KEY_TAPPED(key_timer)

// Keycodes
// ═════════════════════════════════════════════════════════════════════════════

// .......................................................... Keycode Primitives

#define TAP(k)       register_code(k); unregister_code(k)
#define TAP_SHIFT(k) register_code(KC_LSFT); TAP(k); unregister_code(KC_LSFT)

#ifdef PINKIE_STAGGER
void type(RECORD, bool shift, uint16_t keycode)
{
  if (KEY_DOWN) {
    if (shift) { register_code(KC_LSFT); }
    register_code(keycode);
  } else {
    unregister_code(keycode);
    if (shift) { unregister_code(KC_LSFT); }
  }
}

void toggle(RECORD, uint16_t modifier, uint16_t keycode)
{
  if (KEY_DOWN) { START_TIMER; register_code(modifier); }
  else          { unregister_code(modifier); if (KEY_TAP) { TAP(keycode); } }
}
#endif

// ................................................................... Key event

// alternate escape for TT layers, see process_record_user()
void tt_escape(RECORD, uint16_t keycode)
{
  if (tt_keycode && tt_keycode != keycode) { base_layer(0); }  // if different TT layer selected
  if (KEY_DOWN) { START_TIMER; }
  else          { if (KEY_TAP) { tt_keycode = keycode; } CLEAR_TIMER; }
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

#ifdef SPLITOGRAPHY
#define MOD_DOWN(k) (mods & MOD_BIT(k))   // regardless of other home row modifiers
#else
#define MOD_DOWN(k) (mods == MOD_BIT(k))  // on home row modifier only
#endif
#define MOD_BITS(k) if (KEY_DOWN) { mods |= MOD_BIT(k); } else { mods &= ~(MOD_BIT(k)); }

// ......................................................... Modifier Primitives

#define MOD(m)         if (m) { register_code  (m); MOD_BITS(m); }
#define UNMOD(m)       if (m) { unregister_code(m); MOD_BITS(m); }
#define CHORD(m, m2)   MOD(m);    MOD(m2)
#define UNCHORD(m, m2) UNMOD(m2); UNMOD(m)
#define TAP_CASE(u, k) if (u) { TAP_SHIFT(k); } else { TAP(k); }

#ifndef ROLLOVER
// ALT_T, CTL_T, GUI_T, SFT_T for shifted keycodes
void mod_tap(RECORD, uint16_t modifier, uint16_t modifier2, bool shift, uint16_t keycode)
{
  if (KEY_DOWN) {
    START_TIMER;
    CHORD(modifier, modifier2);
  } else {
    UNCHORD(modifier, modifier2);
    if (KEY_TAP) { TAP_CASE(shift, keycode); }
    CLEAR_TIMER;
  }
}
#endif

// Key Sequence Handling
// ═════════════════════════════════════════════════════════════════════════════

// ....................................................... Leader Capitalization

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

#ifdef ROLLOVER
#define SET_EVENT(c) e[c].key_timer = timer_read(); \
                     e[c].keycode   = keycode;      \
                     e[c].shift     = (modifier == KC_LSFT || modifier == KC_RSFT); \
                     e[c].side      = (column <= 4) ? LEFT : RIGHT;                 \
                     e[c].leadercap = leadercap;    \
                     prev_key       = next_key;     \
                     next_key       = c

// column 0 1 2 3 4 <- left, right -> 5 6 7 8 9
static struct column_event {
  uint16_t key_timer;            // event priority
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
#define ROLL         if (shift || ONSHIFT(LSHIFT) || ONSHIFT(RSHIFT)) { TAP_SHIFT(keycode); } else { TAP(keycode); }
#define SHIFT_KEY(c) (c == LSHIFT || c == RSHIFT)
// apply rolling shift to opposite hand (0) for all keys (1) opposite shift key only
#define SHIFT_KEYS   (!ROLLOVER || (ROLLOVER && SHIFT_KEY(column) && SHIFT_KEY(next_key)))

void roll_key(bool shift, uint16_t keycode, uint8_t column)
{
  if (e[column].key_timer < e[next_key].key_timer) {                            // rolling sequence in progress
    clear_mods();                                                               // disable modifier chord finger rolls
    if (e[column].shift && e[column].side != e[next_key].side && SHIFT_KEYS) {  // shift only opposite side of rolling sequence
      TAP_SHIFT(e[next_key].keycode);                                           // shift opposite home row key
      e[next_key].CLEAR_TIMER;                                                  // don't echo this shift key
    } else { ROLL; }                                                            // tap (shifted?) key
  } else   { ROLL; e[prev_key].CLEAR_TIMER; e[column].leadercap = 0; }          // don't echo preceeding modifier key
}

#define CLEAR_EVENT e[column].key_timer   = 0; \
                    e[column].shift       = 0; \
                    e[prev_key].leadercap = 0; \
                    leaderlayer           = 0

// handle rolling keys as shift keycode, a sequence of unmodified keycodes, or keycode leader oneshot_SHIFT
bool mod_roll(RECORD, uint16_t modifier, uint16_t modifier2, bool shift, uint16_t keycode, uint8_t column)
{
  if (KEY_DOWN) {
    SET_EVENT(column);
    CHORD(modifier, modifier2);
  } else {
    UNCHORD(modifier, modifier2);
    if (KEY_TAPPED(e[column].key_timer)) {
      roll_key(shift, keycode, column);
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
void set_leader(RECORD, uint16_t keycode, uint8_t column)
{
  uint16_t modifier = 0;  // for SET_EVENT()

  if (KEY_DOWN) { SET_EVENT(column); }
  else          { e[column].leadercap = 0; }  // clear leader capitalization, see mod_roll()
}

bool map_leader(RECORD, uint16_t shift_key, bool shift, uint16_t keycode, uint8_t column)
{
  set_leader(record, keycode, column);
  return map_shift(record, shift_key, shift, keycode);
}
#endif

static uint8_t map = 0;  // map state

// remap keycode via shift for base and caps layers
bool map_shift(RECORD, uint16_t shift_key, bool shift, uint16_t keycode)
{
  if (map || MOD_DOWN(shift_key)) {
    if (KEY_DOWN) {
      if (!shift) { unregister_code(shift_key); }  // in event of unshifted keycode
      register_code(keycode);
      map = 1;                  // in case shift key is released first
#ifdef ROLLOVER
      e[RSHIFT].CLEAR_TIMER;  // clear punctuation modifier (key tap), see mod_roll()
#endif
    } else {
      unregister_code(keycode);
      if (!shift) { register_code(shift_key); reshifted = 1; }  // set SFT_T timing trap, process_record_user()
      map = 0;
    }
    CLEAR_TIMER;              // clear home row shift, see process_record_user()
#ifdef ROLLOVER
    e[LSHIFT].CLEAR_TIMER;    // clear left handed separator modifier (key tap)
#endif
    return true;
  }
  return false;
}

// conditional map_shift pass through on keycode down to complete layer_toggle(), see process_record_user()
bool map_shifted(RECORD, uint16_t shift_key, bool shift, uint16_t keycode, uint8_t layer)
{
  if (MOD_DOWN(shift_key)) {
    if (KEY_DOWN) {
      START_TIMER;
#ifdef ROLLOVER
      e[RSHIFT].CLEAR_TIMER;  // clear punctuation modifier (key tap), see mod_roll()
#endif
    } else {
      if (KEY_TAP) {
        if (!shift) { unregister_code(shift_key); }               // in event of unshifted keycode
        TAP(keycode);
        if (!shift) { register_code(shift_key); reshifted = 1; }  // set SFT_T timing trap, process_record_user()
      }
      CLEAR_TIMER;            // clear home row shift, see process_record_user() and sft_home()
#ifdef ROLLOVER
      e[LSHIFT].CLEAR_TIMER;  // clear left handed separator modifier (key tap)
#endif
      if (layer) { layer_off(layer); }  // disable MO layer (base layer == 0)
      return true;
    }
  }
  return false;
}

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
#ifdef ROLLOVER
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
void layer_toggle(RECORD, uint8_t layer, bool shift, uint16_t keycode)
{
  if (KEY_DOWN) { START_TIMER; layer_on(layer); }
  else {
    layer_off(layer);
    if (KEY_TAP) { TAP_CASE(shift, keycode); }
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
void rolling_layer(RECORD, uint8_t side, bool shift, uint16_t keycode, uint8_t layer, uint8_t facing)
{
  if (KEY_DOWN) {
    layer_on(layer);
    if (side == LEFT) { leftside = layer; }
    else              { rightside = layer; }
    START_TIMER;
  } else {
    layer_off(_MOUSE);
    if (keycode && KEY_TAP) { TAP_CASE(shift, keycode); }
    if (side == LEFT) { SWITCH_LAYER(leftside, rightside); }
    else              { SWITCH_LAYER(rightside, leftside); }
    // clear_mods();
    CLEAR_TIMER;
  }
}

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
