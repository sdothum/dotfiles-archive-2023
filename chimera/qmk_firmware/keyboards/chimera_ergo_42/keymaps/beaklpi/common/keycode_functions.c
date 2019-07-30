
#include "config.h"  // for ale linter
#include "keycode_functions.h"

// ................................................................ Global Scope

static uint8_t  reshifted  = 0;  // SFT_T timing trap, see map_shift(), process_record_user()
static uint16_t tt_keycode = 0;  // current TT keycode

// ................................................................. Local Scope

static uint8_t  i          = 0;  // inline for loop counter
static uint16_t key_timer  = 0;  // global event timer

#define KEY_DOWN  record->event.pressed
#define KEY_TIMER key_timer = timer_read()
#define KEY_TAP   timer_elapsed(key_timer) < TAPPING_TERM

// Keycodes
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

#define MOD_KEY(x) mods & MOD_BIT(x)

// (un)register modifiers
void mod_all(void (*f)(uint8_t), uint8_t mask)
{
  if (!mods)            { return; }
  if (MOD_KEY(KC_LGUI)) { f(KC_LGUI); }
  if (MOD_KEY(KC_LCTL)) { f(KC_LCTL); }
  if (MOD_KEY(KC_LALT)) { f(KC_LALT); }
  if (MOD_KEY(KC_LSFT)) { f(KC_LSFT); }
  if (MOD_KEY(KC_RSFT)) { f(KC_RSFT); }  // note: qmk macros all use left modifiers
  if (MOD_KEY(KC_RALT)) { f(KC_RALT); }
  if (MOD_KEY(KC_RCTL)) { f(KC_RCTL); }
  if (MOD_KEY(KC_RGUI)) { f(KC_RGUI); }
  mods &= (mask ? 0xFF : 0);             // 0 -> discard, otherwise -> retain state
}

// two or more active modifier keys (down) only, see mod_roll()
bool chained_modifier()
{
  uint8_t bits = 0;
  uint8_t i    = mods;
  while(i) { bits += i % 2; i >>= 1; }
  return bits > 1;
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

// .................................................................. Key event

// alternate escape for TT layers, see process_record_user()
void tt_escape(RECORD, uint16_t keycode)
{
  if (tt_keycode && tt_keycode != keycode) { base_layer(0); }  // if different TT layer selected
  if (KEY_DOWN)                            { KEY_TIMER; }
  else                                     { if (KEY_TAP) { tt_keycode = keycode; } key_timer = 0; }
}

// tapped or not?
bool key_press(RECORD)
{
  if (KEY_DOWN)     { KEY_TIMER; }
  else if (KEY_TAP) { key_timer = 0; return true; }
  else              { key_timer = 0; }
  return false;
}

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

void double_tap(uint8_t count, uint8_t shift, uint16_t keycode)
{
  shift ? tap_shift(keycode) : tap_key(keycode);
  if (count > 1) { shift ? tap_shift(keycode) : tap_key(keycode); }
}

// ............................................................ Keycode Modifier

#define SHIFT   1
#define NOSHIFT 0

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

#define SET_EVENT(c) e[c].key_timer = timer_read(); \
                     e[c].keycode   = keycode;      \
                     e[c].shift     = shift;        \
                     e[c].side      = side;         \
                     e[c].leadercap = leadercap;    \
                     prev_key       = next_key;     \
                     next_key       = c

// column 0 1 2 3 4 <- left, right -> 5 6 7 8 9
static struct column_event {
  uint16_t key_timer;            // event priority
  uint16_t keycode;
  uint8_t  shift;
  uint8_t  side;
  uint8_t  leadercap;
} e[12];                         // leader -> 10 11, see process_record_user(), mod_roll()

void clear_events(void)
{
  for (i = 0; i < 12; i++) { e[i].key_timer = 0; e[i].leadercap = 0; }
}

#define LEADER 10                // ,11 leader columns
#define LSHIFT 3                 // left shift column
#define RSHIFT 6                 // right shift column

#define LEFT   1                 // also see raise_layer(), rolling_layer()
#define RIGHT  2                 // for binary (LEFT | RIGHT) test

#define ROLL(s, k) (e[RSHIFT].shift && s == LEFT) || (e[LSHIFT].shift && s == RIGHT) ? tap_shift(k) : tap_key(k)

static uint8_t leadercap   = 0;  // substitute (0) keycode (1) leader + oneshot_SHIFT, see cap_lt()
static uint8_t togglelayer = 0;  // key's toggle layer, see process_record_user()
static uint8_t next_key    = 0;  // by column reference
static uint8_t prev_key    = 0;

// handle rolling keys as shift keycode, a sequence of unmodified keycodes, or keycode leader oneshot_SHIFT
bool mod_roll(RECORD, uint8_t side, uint8_t shift, uint16_t modifier, uint16_t keycode, uint8_t column)
{
  if (KEY_DOWN) {
    SET_EVENT(column);
    if (modifier) { register_modifier(modifier); }
  } else {
    if (modifier) { unregister_modifier(modifier); }
    if (timer_elapsed(e[column].key_timer) < TAPPING_TERM) {
      if (e[column].key_timer < e[next_key].key_timer) {              // rolling sequence in progress
        mod_all(unregister_code, 0);                                  // disable modifier chord finger rolls
        if (e[column].shift && e[column].side != e[next_key].side) {  // shift only opposite side of rolling sequence
          tap_shift(e[next_key].keycode);                             // shift opposite home row key
          e[next_key].key_timer = 0;                                  // don't re-echo this key
        } else { ROLL(side, keycode); }                               // tap (shifted?) key
      } else   { ROLL(side, keycode); e[prev_key].key_timer = 0; e[column].leadercap = 0; }  // don't echo preceeding modifier key
    }
    if (e[prev_key].leadercap && column >= LEADER) {                  // trigger leader capitalization only on leader key
      if (togglelayer) { layer_off(togglelayer); togglelayer = 0; }   // disable key's toggle layer, see process_record_user()
      layer_on         (_SHIFT);                                      // sentence/paragraph capitalization
      set_oneshot_layer(_SHIFT, ONESHOT_START);                       // see process_record_user() -> clear_oneshot_layer_state(ONESHOT_PRESSED)
      e[prev_key].leadercap = 0;
      return true; 
    }
    e[column].key_timer = 0;
    e[column].shift     = 0;  // clear shift state, see ROLL()
  }
  return false;
}

// down -> always shift (versus SFT_t auto repeat), 
void mod_t(RECORD, uint16_t modifier, uint16_t keycode)
{
  if (KEY_DOWN) { KEY_TIMER; register_modifier(modifier); }
  else {
    unregister_modifier(modifier);
    if (KEY_TAP) { tap_key(keycode); }
    key_timer = 0;
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

// ................................................................. Map Keycode

// handle map_shift() rolling keys (and dot chords)
void set_leader(RECORD, uint8_t side, uint16_t shift_key, uint8_t shift, uint16_t keycode, uint8_t column)
{
  if (KEY_DOWN) { SET_EVENT(column); }
  else          { e[column].leadercap = 0; }  // clear leader capitalization, see mod_roll()
}

bool map_leader(RECORD, uint8_t side, uint16_t shift_key, uint8_t shift, uint16_t keycode, uint8_t column)
{
  set_leader(record, side, shift_key, shift, keycode, column);
  return map_shift(record, shift_key, shift, keycode);
}

static uint8_t map = 0;  // map state

// remap keycode via shift for base and caps layers
bool map_shift(RECORD, uint16_t shift_key, uint8_t shift, uint16_t keycode)
{
  if (map || mod_down(shift_key)) {
    if (KEY_DOWN) {
      if (!shift) { unregister_code(shift_key); }  // in event of unshifted keycode
      register_code(keycode);
      map = 1;                                     // in case shift key is released first
      e[RSHIFT].key_timer = 0;                     // don't bounce the punctuation modifier, see mod_roll()
    } else {
      unregister_code(keycode);
      if (!shift) { register_code(shift_key); reshifted = 1; }  // set SFT_T timing trap, process_record_user()
      map = 0;
    }
    key_timer = 0;  // clear home row shift, see process_record_user()
    return true;
  }
  return false;
}

#ifndef CHIMERA
// conditional map_shift pass through on keycode down to complete lt(), see process_record_user()
bool mapc_shift(RECORD, uint16_t shift_key, uint8_t shift, uint16_t keycode)
{
  if (mod_down(shift_key)) {
    if (KEY_DOWN) { KEY_TIMER; }
    else {
      if (KEY_TAP) {
        if (!shift) { unregister_code(shift_key); }               // in event of unshifted keycode
        tap_key(keycode);
        if (!shift) { register_code(shift_key); reshifted = 1; }  // set SFT_T timing trap, process_record_user()
      }
      key_timer = 0;  // clear home row shift, see process_record_user() and sft_home()
      return true;
    }
  }
  return false;
}
#endif

// LT (LAYER, KEY) -> <leader><SHIFT>, see process_record_user() and TD_TILD, KC_EXLM, KC_QUES
bool leader_cap(RECORD, uint8_t layer, uint8_t leadercap, uint16_t keycode)
{
  if (leadercap) {
    if (KEY_DOWN) { KEY_TIMER; return false; }
    else if (KEY_TAP) {
      tap_key(keycode);
      if (layer) { layer_off(layer); }           // disable key's toggle layer
      layer_on         (_SHIFT);                 // sentence/paragraph capitalization
      set_oneshot_layer(_SHIFT, ONESHOT_START);  // see process_record_user() -> clear_oneshot_layer_state(ONESHOT_PRESSED)
      key_timer = 0;
      return true; 
    }
    key_timer = 0;
  }
  return false;
}

// Tap Dance
// ═════════════════════════════════════════════════════════════════════════════

// .......................................................... Tap Dance Keycodes

qk_tap_dance_action_t tap_dance_actions[] = {
  [_ASTR]   = ACTION_TAP_DANCE_FN              (asterisk)
 ,[_COMM]   = ACTION_TAP_DANCE_FN              (comma)
 ,[_DOT]    = ACTION_TAP_DANCE_FN              (dot)
 ,[_EQL]    = ACTION_TAP_DANCE_FN_ADVANCED_TIME(NULL, equal, equal_reset, HASKELL_TERM)
 ,[_PASTE]  = ACTION_TAP_DANCE_FN_ADVANCED     (NULL, paste, paste_reset)
 ,[_PERC]   = ACTION_TAP_DANCE_FN_ADVANCED     (NULL, percent, percent_reset)
 ,[_PRIV]   = ACTION_TAP_DANCE_FN              (private)
 ,[_SEND]   = ACTION_TAP_DANCE_FN              (send)
 ,[_TILD]   = ACTION_TAP_DANCE_FN_ADVANCED     (NULL, tilde, tilde_reset)
 ,[_X]      = ACTION_TAP_DANCE_FN              (pound)
 ,[_XPASTE] = ACTION_TAP_DANCE_FN_ADVANCED     (NULL, xpaste, xpaste_reset)
#ifdef HASKELL
 ,[_COLN]   = ACTION_TAP_DANCE_FN_ADVANCED_TIME(NULL, colon, colon_reset, HASKELL_TERM)
 ,[_LT]     = ACTION_TAP_DANCE_FN_ADVANCED_TIME(NULL, lesser, lesser_reset, HASKELL_TERM)
 ,[_GT]     = ACTION_TAP_DANCE_FN_ADVANCED_TIME(NULL, greater, greater_reset, HASKELL_TERM)
#endif
};

// ........................................................... Context Multi Tap

#define TAP      state->count
#define TAPS     TAP > 1
#define TAP_DOWN state->pressed

#define DOUBLE_TAP(k, s) if (TAP_DOWN)                  { register_code(k); } \
                         else if (TAP == 2)             { send_string(s); }   \
                         else for (i = 0; i < TAP; i++) { tap_key(k); }

void colon(STATE, void *user_data)
{
  if (mod_down(KC_RSFT)) {  // handle like map_shift()
    if (TAPS)                      { DOUBLE_TAP(KC_SCLN, ":-"); }
    else                           { TAP_DOWN ? register_code(KC_SCLN) : double_tap(TAP, NOSHIFT, KC_SCLN); }
  } else if (TAPS) {
    if (TAP_DOWN)                  { register_shift(KC_SCLN); }
#ifdef HASKELL
    else if (TAP == 2)             { send_string(" :: "); }
#endif
    else for (i = 0; i < TAP; i++) { tap_shift(KC_SCLN); }
  } else                           { TAP_DOWN ? register_shift(KC_SCLN) : double_tap(TAP, SHIFT, KC_SCLN); }
  reset_tap_dance(state);
}

void colon_reset(STATE, void *user_data)
{
  unregister_shift(KC_SCLN);
  if (mod_down(KC_RSFT)) { register_code(KC_RSFT); }  // restore HOME_T, see process_record_user() TD_COLN
}

void equal(STATE, void *user_data)
{
  if (TAPS) { DOUBLE_TAP(KC_EQL, "!="); }
#ifdef CHIMERA
  else      { TAP_DOWN ? register_code(KC_EQL) : double_tap(TAP, NOSHIFT, KC_EQL); }
#else
  else      { TAP_DOWN ? layer_on(_MOUSE) : double_tap(TAP, NOSHIFT, KC_EQL); }
#endif
  reset_tap_dance(state);
}

void equal_reset(STATE, void *user_data)
{
  unregister_code(KC_EQL);
  layer_off      (_MOUSE);
}

#define DOUBLE_SHIFT(k, s) if (TAP_DOWN)                  { register_shift(k); } \
                           else if (TAP == 2)             { send_string(s); }    \
                           else for (i = 0; i < TAP; i++) { tap_shift(k); }

#ifdef HASKELL
void lesser(STATE, void *user_data)
{
  if (TAPS) { DOUBLE_SHIFT(KC_COMM, " <- "); }
  else      { TAP_DOWN ? register_code(KC_LCTL) : double_tap(TAP, SHIFT, KC_COMM); }
  reset_tap_dance(state);
}

void lesser_reset(STATE, void *user_data)
{
  unregister_shift(KC_COMM);
  unregister_code (KC_LCTL);
}

void greater(STATE, void *user_data)
{
  if (TAPS) { DOUBLE_SHIFT(KC_DOT, " -> "); }
  else      { TAP_DOWN ? register_code(KC_LSFT) : double_tap(TAP, SHIFT, KC_DOT); }
  reset_tap_dance(state);
}

void greater_reset(STATE, void *user_data)
{
  unregister_shift(KC_DOT);
  unregister_code (KC_LSFT);
}
#endif

void tilde(STATE, void *user_data)
{
  if (TAPS) { DOUBLE_SHIFT(KC_GRV, "~/"); }
  else      { TAP_DOWN ? register_shift(KC_GRV) : tap_shift(KC_GRV); }
  reset_tap_dance(state);
}

void tilde_reset(STATE, void *user_data)
{
  unregister_shift(KC_GRV);
  unregister_code (KC_DOT);
  if (mod_down(KC_RSFT)) { register_code(KC_RSFT); }  // restore HOME_T, see process_record_user() TD_TILD
}

// ........................................................... Simple Double Tap

void asterisk(STATE, void *user_data)
{
  if (TAPS) { tap_key(KC_DOT); }
  tap_shift(KC_8);
  reset_tap_dance(state);
}

void comma(STATE, void *user_data)
{
  tap_key(KC_COMM);
  if (TAPS) { tap_key(KC_SPC); }
  reset_tap_dance(state);
}

void dot(STATE, void *user_data)
{
  if (biton32(layer_state) == _NUMBER) { TAPS ? tap_shift(KC_COLN) : tap_key(KC_DOT); }
  else                                 { TAPS ? send_string("./") : tap_key(KC_DOT); }  // see symbol layer
  reset_tap_dance(state);
}

#define IRC_ENTER _delay_ms(10); \
                  tap_key  (KC_ENT)

void paste(STATE, void *user_data)
{
  if (TAPS)          { mod_key(KC_LCTL, KC_V); IRC_ENTER; }
  else if (TAP_DOWN) { register_code(KC_LCTL); register_code(KC_V); }
  else               { mod_key(KC_LCTL, KC_V); }
  reset_tap_dance(state);
}

void paste_reset(STATE, void *user_data)
{
  unregister_code(KC_V);
  unregister_code(KC_LCTL);
}

void percent(STATE, void *user_data)
{
  if (TAPS && TAP_DOWN) { register_shift(KC_5); }
  else                  { TAP_DOWN ? register_code(KC_LALT) : double_tap(TAP, SHIFT, KC_5); }
  reset_tap_dance(state);
}

void percent_reset(STATE, void *user_data)
{
  unregister_shift(KC_5);
  unregister_code (KC_LALT);
}

void pound(STATE, void *user_data)
{
  TAPS ? tap_shift(KC_3) : tap_key(KC_X);
  reset_tap_dance(state);
}

// compile time macro string, see functions/hardware <keyboard> script
void private(STATE, void *user_data)
{
  if (TAPS) { SEND_STRING(PRIVATE_STRING); }
  reset_tap_dance(state);
}

// config.h defined string
void send(STATE, void *user_data)
{
  if (TAPS) { SEND_STRING(PUBLIC_STRING); }
  reset_tap_dance(state);
}

#define CTL_SFT_V register_code  (KC_LCTL); \
                  tap_shift      (KC_V);    \
                  unregister_code(KC_LCTL)

void xpaste(STATE, void *user_data)
{
  if (TAPS)          { CTL_SFT_V; IRC_ENTER; }
  else if (TAP_DOWN) { register_code(KC_LCTL); register_shift(KC_V); }
  else               { CTL_SFT_V; }
  reset_tap_dance(state);
}

void xpaste_reset(STATE, void *user_data)
{
  unregister_shift(KC_V);
  unregister_code (KC_LCTL);
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
  clear_events();
}

void base_layer(uint8_t defer)
{
  if (defer) { return; }  // see process_record_user() reset keys
#ifdef AUDIO_ENABLE
  plover ? PLAY_SONG(song_plover_gb) : PLAY_SONG(song_qwerty);
#endif
  clear_layers();
  set_single_persistent_default_layer(_BASE);
#ifndef CHIMERA
  toggle_plover(0);
#endif
}

// set layer asap to overcome macro latency errors, notably tap dance, LT usage and..
// inexplicably sets layer_on() faster than can be done in rolling_layer()
void tap_layer(RECORD, uint8_t layer)
{
  KEY_DOWN ? layer_on(layer) : layer_off(layer);
}

// LT macro for mapc_shift(), see process_record_user()
void lt(RECORD, uint8_t layer, uint8_t shift, uint16_t keycode)
{
  if (KEY_DOWN) { KEY_TIMER; layer_on(layer); }
  else {
    layer_off(layer);
    if (KEY_TAP) { mod_key(shift, keycode); }
    // clear_mods();
    key_timer = 0;
  }
}

// ............................................................ Double Key Layer

#define ONDOWN 0
#define TOGGLE 1

static uint8_t double_key = 0;

// dual key to raise layer (layer 0 to trap dual key state :-)
bool raise_layer(RECORD, uint8_t layer, uint8_t side, uint8_t toggle)
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

#ifndef CHIMERA
// .............................................................. Rolling Layers

// rolling thumb combinations, see process_record_user()
// up,   up   -> _BASE
// up,   down -> _SYMGUI
// down, up   -> _REGEX
// down, down -> _MOUSE, see layer keycodes that raise mouse layer

static uint8_t leftside  = 0;
static uint8_t rightside = 0;

#define SWITCH_LAYER(x, y) layer_off(x); \
                           x = 0;        \
                           if (y && y == _MOUSE) { layer_on(facing); y = facing; }

// seamlessly switch left / right thumb layer combinations
void rolling_layer(RECORD, uint8_t side, uint8_t shift, uint16_t keycode, uint8_t layer, uint8_t facing)
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

// ....................................................................... Steno

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
