#define STENO !defined(CHIMERA) && !defined(CORNE)

#include "config.h"  // for ale linter
#include "keycode_functions.h"

// ................................................................ Global Scope


static bool     leadercap  = 0;  // substitute (0) keycode (1) leader + oneshot_SHIFT, see leader_cap()
static bool     reshifted  = 0;  // SFT_T timing trap, see map_shift(), process_record_user()
static uint16_t tt_keycode = 0;  // current TT state (keycode)

#define CLR_1SHOT clear_oneshot_layer_state(ONESHOT_PRESSED)
#define KEY_DOWN  record->event.pressed

#define LEFT      1  // keyboard hand side
#define RIGHT     2  // for (LEFT | RIGHT) bit test

#define SHIFT     1  // shift option or shiftable state (function dependent)
#define NOSHIFT   0

#define ONDOWN    0  // see raise_layer()
#define TOGGLE    1

// ................................................................. Local Scope

static uint8_t  i          = 0;  // inline for loop counter
static uint16_t key_timer  = 0;  // global event timer

#define KEY_TIMER key_timer = timer_read()
#define KEY_TAP   timer_elapsed(key_timer) < TAPPING_TERM

// Keycodes
// ═════════════════════════════════════════════════════════════════════════════

// .......................................................... Keycode Primitives

void register_shift(uint16_t keycode)
{
  register_code(KC_LSFT);
  register_code(keycode);
}

void unregister_shift(uint16_t keycode)
{
  unregister_code(keycode);
  unregister_code(KC_LSFT);
}

void tap_key(uint16_t keycode)
{
  register_code  (keycode);
  unregister_code(keycode);
}

void tap_shift(uint16_t keycode)
{
  register_code  (KC_LSFT);
  tap_key        (keycode);
  unregister_code(KC_LSFT);
}

#ifdef INITIAL_STAGGER
void send(RECORD, bool shift, uint16_t keycode)
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
  if (KEY_DOWN) { KEY_TIMER; register_code(modifier); }
  else          { unregister_code(modifier); if (KEY_TAP) { tap_key(keycode); } }
}
#endif

// ................................................................... Key event

// alternate escape for TT layers, see process_record_user()
void tt_escape(RECORD, uint16_t keycode)
{
  if (tt_keycode && tt_keycode != keycode) { base_layer(0); }  // if different TT layer selected
  if (KEY_DOWN)                            { KEY_TIMER; }
  else                                     { if (KEY_TAP) { tt_keycode = keycode; } key_timer = 0; }
}

#ifdef SPLITOGRAPHY
// tapped or not?
bool key_press(RECORD)
{
  if (KEY_DOWN)     { KEY_TIMER; }
  else if (KEY_TAP) { key_timer = 0; return true; }
  else              { key_timer = 0; }
  return false;
}
#endif

// Modifiers
// ═════════════════════════════════════════════════════════════════════════════

// ................................................................... Mod Masks

// tap dance persistant mods, see process_record_user()
// keyboard_report->mods (?) appears to be cleared by tap dance
static uint8_t mods = 0;

void register_modifier(uint16_t keycode)
{
  register_code(keycode);
  mods |= MOD_BIT(keycode);
}

void unregister_modifier(uint16_t keycode)
{
  unregister_code(keycode);
  mods &= ~(MOD_BIT(keycode));
}

#define MOD_KEY(k) if (mods & MOD_BIT(k)) { f(k); }

// (un)register modifiers
void mod_all(void (*f)(uint8_t), uint8_t mask)
{
  if (!mods) { return; }
  MOD_KEY(KC_LGUI);
  MOD_KEY(KC_LCTL);
  MOD_KEY(KC_LALT);
  MOD_KEY(KC_LSFT);
  MOD_KEY(KC_RSFT);           // note: qmk macros all use left modifiers
  MOD_KEY(KC_RALT);
  MOD_KEY(KC_RCTL);
  MOD_KEY(KC_RGUI);
  mods &= (mask ? 0xFF : 0);  // 0 -> discard, otherwise -> retain state
}

void mod_bits(RECORD, uint16_t keycode)
{
  if (KEY_DOWN) { mods |=   MOD_BIT(keycode); }
  else          { mods &= ~(MOD_BIT(keycode)); }
}

// base layer modifier
bool mod_down(uint16_t key_code)
{
#ifdef SPLITOGRAPHY
  return mods & MOD_BIT(key_code);   // regardless of other home row modifiers
#else
  return mods == MOD_BIT(key_code);  // on home row modifier only
#endif
}

// ......................................................... Modifier Primitives

void mod_key(uint16_t modifier, uint16_t keycode)
{
  switch (modifier) {
  case NOSHIFT:
    tap_key(keycode);   break;
  case SHIFT:
    tap_shift(keycode); break;
  default:
    register_modifier  (modifier);
    tap_key            (keycode);
    unregister_modifier(modifier);
  }
}

// ALT_T, CTL_T, GUI_T, SFT_T for shifted keycodes
void mt_shift(RECORD, uint16_t modifier, uint16_t modifier2, uint16_t keycode)
{
  if (KEY_DOWN) {
    KEY_TIMER;
    if (modifier2) { register_modifier(modifier2); }
    register_modifier(modifier);
  } else {
    unregister_modifier(modifier);
    if (modifier2) { unregister_modifier(modifier2); }
    if (KEY_TAP)   { tap_shift(keycode); }
    key_timer = 0;
  }
}

// Key Sequence Handling
// ═════════════════════════════════════════════════════════════════════════════

// ....................................................... Leader Capitalization

// LT (LAYER, KEY) -> <leader><SHIFT>, see process_record_user() and TD_TILD, KC_EXLM, KC_QUES
bool leader_cap(RECORD, uint8_t layer, uint16_t keycode)
{
  if (leadercap) {
    if (KEY_DOWN) { KEY_TIMER; return false; }
    else if (KEY_TAP) {
      tap_key      (keycode);
      oneshot_shift(layer);
      key_timer = 0;
      return true; 
    }
    key_timer = 0;
  }
  return false;
}

// ................................................................ Rolling Keys
#ifdef ROLLOVER
#define SET_EVENT(c) e[c].key_timer = timer_read(); \
                     e[c].keycode   = keycode;      \
                     e[c].shift     = modifier == KC_LSFT || modifier == KC_RSFT; \
                     e[c].side      = column <= 4 ? LEFT : RIGHT;                 \
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
  for (i = 0; i < 12; i++) { e[i].key_timer = 0; e[i].leadercap = 0; }
}

#define LEADER 10                // and 11 are leader columns (assigned to SPC and ENT)
#define LSHIFT 3                 // left shift column
#define RSHIFT 6                 // right shift column

static uint8_t leaderlayer = 0;  // thumb key's toggle layer, see process_record_user()
static uint8_t next_key    = 0;  // by column reference
static uint8_t prev_key    = 0;

#define ONSHIFT(s, c) (e[c].shift && e[c].key_timer < e[column].key_timer && s == (c == LSHIFT ? RIGHT : LEFT))
#define ROLL(s, k)    ONSHIFT(s, LSHIFT) || ONSHIFT(s, RSHIFT) ? tap_shift(k) : tap_key(k)
#define SHIFT_KEY(c)  (c == LSHIFT || c == RSHIFT)
// apply rolling shift to opposite hand (0) for all keys (1) opposite shift key only
#define SHIFT_KEYS    (!ROLLOVER || (ROLLOVER && SHIFT_KEY(column) && SHIFT_KEY(next_key)))

void roll_key(uint16_t keycode, uint8_t column)
{
  if (e[column].key_timer < e[next_key].key_timer) {  // rolling sequence in progress
    mod_all(unregister_code, 0);                      // disable modifier chord finger rolls
    if (e[column].shift && e[column].side != e[next_key].side && SHIFT_KEYS) {           // shift only opposite side of rolling sequence
      tap_shift(e[next_key].keycode);                 // shift opposite home row key
      e[next_key].key_timer = 0;                      // don't echo this shift key
    } else { ROLL(e[column].side, keycode); }                   // tap (shifted?) key
  } else   { ROLL(e[column].side, keycode); e[prev_key].key_timer = 0; e[column].leadercap = 0; }  // don't echo preceeding modifier key
}

#define CLEAR_EVENT e[column].key_timer   = 0; \
                    e[column].shift       = 0; \
                    e[prev_key].leadercap = 0; \
                    leaderlayer           = 0

// handle rolling keys as shift keycode, a sequence of unmodified keycodes, or keycode leader oneshot_SHIFT
bool mod_roll(RECORD, uint16_t modifier, uint16_t keycode, uint8_t column)
{
  if (KEY_DOWN) {
    SET_EVENT(column);
    if (modifier) { register_modifier(modifier); }
  } else {
    if (modifier) { unregister_modifier(modifier); }
    if (timer_elapsed(e[column].key_timer) < TAPPING_TERM) {
      roll_key(keycode, column);
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
  if (map || mod_down(shift_key)) {
    if (KEY_DOWN) {
      if (!shift) { unregister_code(shift_key); }  // in event of unshifted keycode
      register_code(keycode);
      map = 1;                  // in case shift key is released first
#ifdef ROLLOVER
      e[RSHIFT].key_timer = 0;  // clear punctuation modifier (key tap), see mod_roll()
#endif
    } else {
      unregister_code(keycode);
      if (!shift) { register_code(shift_key); reshifted = 1; }  // set SFT_T timing trap, process_record_user()
      map = 0;
    }
    key_timer = 0;              // clear home row shift, see process_record_user()
#ifdef ROLLOVER
    e[LSHIFT].key_timer = 0;    // clear left handed separator modifier (key tap)
#endif
    return true;
  }
  return false;
}

// conditional map_shift pass through on keycode down to complete lt(), see process_record_user()
bool map_shifted(RECORD, uint16_t shift_key, bool shift, uint16_t keycode, uint8_t layer)
{
  if (mod_down(shift_key)) {
    if (KEY_DOWN) {
      KEY_TIMER;
#ifdef ROLLOVER
      e[RSHIFT].key_timer = 0;          // clear punctuation modifier (key tap), see mod_roll()
#endif
    } else {
      if (KEY_TAP) {
        if (!shift) { unregister_code(shift_key); }               // in event of unshifted keycode
        tap_key(keycode);
        if (!shift) { register_code(shift_key); reshifted = 1; }  // set SFT_T timing trap, process_record_user()
      }
      key_timer = 0;                    // clear home row shift, see process_record_user() and sft_home()
#ifdef ROLLOVER
      e[LSHIFT].key_timer = 0;          // clear left handed separator modifier (key tap)
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
  key_timer  = 0;
  tt_keycode = 0;
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
#if STENO
  toggle_plover(0);
#endif
}

// LT macro for map_shifted(), see process_record_user()
void lt(RECORD, uint8_t layer, bool shift, uint16_t keycode)
{
  if (KEY_DOWN) { KEY_TIMER; layer_on(layer); }
  else {
    layer_off(layer);
    if (KEY_TAP) { mod_key(shift, keycode); }
    // clear_mods();
    key_timer = 0;
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
bool raise_layer(RECORD, uint8_t layer, uint8_t side, bool toggle)
{
  if (KEY_DOWN) {
    double_key |= side;
    if (double_key == (LEFT | RIGHT)) { 
      if (layer) { toggle ? layer_invert(layer) : layer_on(layer); }
      return true;
    }
  } else {
    double_key &= ~side;
    if (!(double_key || toggle)) { layer_off(layer); }  // allow single key to continue on layer :-)
  }
  return false;
}

// Steno
// ═════════════════════════════════════════════════════════════════════════════
#if STENO
// .............................................................. Rolling Layers

// rolling thumb combinations, see process_record_user()
// up,   up   -> _BASE
// up,   down -> _SYMGUI
// down, up   -> _REGEX
// down, down -> _MOUSE, see layer keycodes that raise mouse layer

static uint8_t leftside  = 0;
static uint8_t rightside = 0;

#define SWITCH_LAYER(x, y) layer_off(x); x = 0; \
                           if (y && y == _MOUSE) { layer_on(facing); y = facing; }

// seamlessly switch left / right thumb layer combinations
void rolling_layer(RECORD, uint8_t side, bool shift, uint16_t keycode, uint8_t layer, uint8_t facing)
{
  if (KEY_DOWN) {
    layer_on(layer);
    if (side == LEFT)       { leftside = layer; }
    else                    { rightside = layer; }
    KEY_TIMER;
  } else {
    layer_off(_MOUSE);
    if (keycode && KEY_TAP) { mod_key(shift, keycode); }
    if (side == LEFT)       { SWITCH_LAYER(leftside, rightside); }
    else                    { SWITCH_LAYER(rightside, leftside); }
    // clear_mods();
    key_timer = 0;
  }
}

// ...................................................................... Plover

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
