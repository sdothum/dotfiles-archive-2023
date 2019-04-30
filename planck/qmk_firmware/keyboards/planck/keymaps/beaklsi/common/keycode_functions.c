
#include "keycode_functions.h"

static uint8_t  i         = 0; // inline for loop counter
static uint16_t key_timer = 0; // global event timer
static uint8_t reshifted  = 0; // SFT_T timing trap, see map_shift(), process_record_user()

// Keycodes
// ═════════════════════════════════════════════════════════════════════════════

// ................................................................... Mod Masks

// tap dance persistant mods, see process_record_user()
// keyboard_report->mods (?) appears to be cleared by tap dance
static uint8_t mods = 0;

void tap_mods(keyrecord_t *record, uint16_t keycode)
{
  if (record->event.pressed) { mods |= MOD_BIT  (keycode); }
  else                       { mods &= ~(MOD_BIT(keycode)); }
}

// (un)register modifiers
void modifier(void (*f)(uint8_t))
{
  if (mods & MOD_BIT(KC_LCTL)) { f(KC_LCTL); }
  if (mods & MOD_BIT(KC_LGUI)) { f(KC_LGUI); }
  if (mods & MOD_BIT(KC_LALT)) { f(KC_LALT); }
  if (mods & MOD_BIT(KC_LSFT)) { f(KC_LSFT); }
  if (mods & MOD_BIT(KC_RSFT)) { f(KC_RSFT); } // note: qmk macros all use left modifiers
}

// base layer modifier
bool mod_down(uint16_t key_code)
{
  // return (mods && ((mods & MOD_BIT(key_code)) == mods) && (biton32(layer_state) == _BASE || biton32(layer_state) == _TTCAPS));
  // return (mods && ((mods & MOD_BIT(key_code)) == mods));
  return mods & MOD_BIT(key_code); // relax timing on home row modifiers
}

// .................................................................. Key event

static uint16_t tt_keycode = 0; // current TT keycode

// alternate escape for TT layers, see process_record_user()
void tt_escape(keyrecord_t *record, uint16_t keycode)
{
  if (tt_keycode != keycode && tt_keycode) { base_layer(0); } // if different TT layer selected
  if (record->event.pressed)               { key_timer = timer_read(); }
  else {
    if (timer_elapsed(key_timer) < TAPPING_TERM) { tt_keycode = keycode; }
    key_timer = 0;
  }
}

// .......................................................... Keycode Primitives

// register shift keycode
void register_shift(uint16_t keycode)
{
  register_code(KC_LSFT);
  register_code(keycode);
}

// unregister shift keycode
void unregister_shift(uint16_t keycode)
{
  unregister_code(keycode);
  unregister_code(KC_LSFT);
}

// register simple key press
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
    tap_key(keycode);
    break;
  case SHIFT:
    tap_shift(keycode);
    break;
  default:
    register_code  (modifier);
    tap_key        (keycode);
    unregister_code(modifier);
  }
}

// down -> always shift (versus SFT auto repeat), 
void mod_t(keyrecord_t *record, uint16_t modifier, uint16_t keycode)
{
  if (record->event.pressed) {
    key_timer = timer_read();
    register_code(modifier);
  }
  else {
    unregister_code(modifier);
    if (timer_elapsed(key_timer) < TAPPING_TERM) { tap_key(keycode); }
    key_timer = 0;
  }
}

// ALT_T, CTL_T, GUI_T, SFT_T for shifted keycodes
void mt_shift(keyrecord_t *record, uint16_t modifier, uint16_t modifier2, uint16_t keycode)
{
  if (record->event.pressed) {
    key_timer = timer_read();
    if (modifier2) { register_code(modifier2); }
    register_code(modifier);
  }
  else {
    unregister_code(modifier);
    if (modifier2)                               { unregister_code(modifier2); }
    if (timer_elapsed(key_timer) < TAPPING_TERM) { tap_shift(keycode); }
    key_timer = 0;
  }
}

// ................................................................. Map Keycode

// remap keycode via shift for base and caps layers
bool map_shift(keyrecord_t *record, uint16_t shift_key, uint8_t shift, uint16_t keycode)
{
  if (mod_down(shift_key)) {
    if (record->event.pressed) {
      if (!shift) { unregister_code(shift_key); }              // in event of unshifted keycode
      register_code(keycode);
    }
    else {
      unregister_code(keycode);
      if (!shift) { register_code(shift_key); reshifted = 1; } // set SFT_T timing trap, process_record_user()
    }
    key_timer = 0; // clear home row shift, see process_record_user() and mod_t()
    return true;
  }
  return false;
}

#ifndef CHIMERA
// conditional map_shift pass through on keycode down to complete lt(), see process_record_user()
bool mapc_shift(keyrecord_t *record, uint16_t shift_key, uint8_t shift, uint16_t keycode)
{
  if (mod_down(shift_key)) {
    if (record->event.pressed) {
      key_timer = timer_read();
    }
    else {
      if (timer_elapsed(key_timer) < TAPPING_TERM) {
        if (!shift) { unregister_code(shift_key); }              // in event of unshifted keycode
        tap_key(keycode);
        if (!shift) { register_code(shift_key); reshifted = 1; } // set SFT_T timing trap, process_record_user()
      }
      key_timer = 0; // clear home row shift, see process_record_user() and mod_t()
      return true;
    }
  }
  return false;
}
#endif

// ....................................................... Leader Capitalization

// LT (LAYER, KEY) -> <leader><SHIFT>, see process_record_user() and TD_TILD, KC_EXLM, KC_QUES
bool leader_cap(keyrecord_t *record, uint8_t layer, uint8_t autocap, uint16_t keycode)
{
  if (autocap) {
    if (!record->event.pressed) { 
      tap_key(keycode);
      if (layer) { layer_off(layer); }
      layer_on                 (_SHIFT); // sentence/paragraph capitalization
      set_oneshot_layer        (_SHIFT, ONESHOT_START);
      clear_oneshot_layer_state(ONESHOT_PRESSED);
    }
    return true; 
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
 ,[_XPASTE] = ACTION_TAP_DANCE_FN_ADVANCED     (NULL, xpaste, xpaste_reset)
#ifdef HASKELL
 ,[_COLN]   = ACTION_TAP_DANCE_FN_ADVANCED_TIME(NULL, colon, colon_reset, HASKELL_TERM)
 ,[_LT]     = ACTION_TAP_DANCE_FN_ADVANCED_TIME(NULL, lesser, lesser_reset, HASKELL_TERM)
 ,[_GT]     = ACTION_TAP_DANCE_FN_ADVANCED_TIME(NULL, greater, greater_reset, HASKELL_TERM)
#endif
};

// ........................................................... Context Multi Tap

void colon(qk_tap_dance_state_t *state, void *user_data)
{
  if (mod_down(KC_RSFT)) { // shift -> semicolon
    if (state->count > 1) {
      if (state->pressed)                     { register_code(KC_SCLN); }
      else if (state->count == 2)             { send_string(":-"); }
      else for (i = 0; i < state->count; i++) { tap_key(KC_SCLN); }
    }
    else { state->pressed ? register_code(KC_SCLN) : double_tap(state->count, NOSHIFT, KC_SCLN); }
  }
  else if (state->count > 1) {
    if (state->pressed)                     { register_shift(KC_SCLN); }
#ifdef HASKELL
    else if (state->count == 2)             { send_string(" :: "); }
#endif
    else for (i = 0; i < state->count; i++) { tap_shift(KC_SCLN); }
  }
  else { state->pressed ? register_shift(KC_SCLN) : double_tap(state->count, SHIFT, KC_SCLN); }
  reset_tap_dance(state);
}

void colon_reset(qk_tap_dance_state_t *state, void *user_data)
{
  unregister_shift(KC_SCLN);
  if (mod_down(KC_RSFT)) { register_code(KC_RSFT); } // restore HOME_T, see process_record_user() TD_COLN
}

void equal(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1) {
    if (state->pressed)                     { register_code(KC_EQL); }
    else if (state->count == 2)             { send_string("=~"); } 
    else for (i = 0; i < state->count; i++) { tap_key(KC_EQL); }
  }
#ifdef CHIMERA
  else { state->pressed ? register_code(KC_EQL) : double_tap(state->count, NOSHIFT, KC_EQL); }
#else
  else { state->pressed ? layer_on(_MOUSE) : double_tap(state->count, NOSHIFT, KC_EQL); }
#endif
  reset_tap_dance(state);
}

void equal_reset(qk_tap_dance_state_t *state, void *user_data)
{
  unregister_code(KC_EQL);
  layer_off      (_MOUSE);
}

#ifdef HASKELL
void lesser(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1) {
    if (state->pressed)                     { register_shift(KC_COMM); }
    else if (state->count == 2)             { send_string(" <- "); }
    else for (i = 0; i < state->count; i++) { tap_shift(KC_COMM); }
  }
  else { state->pressed ? register_code(KC_LCTL) : double_tap(state->count, SHIFT, KC_COMM); }
  reset_tap_dance(state);
}

void lesser_reset(qk_tap_dance_state_t *state, void *user_data)
{
  unregister_shift(KC_COMM);
  unregister_code (KC_LCTL);
}

void greater(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1) {
    if (state->pressed)                     { register_shift(KC_DOT); }
    else if (state->count == 2)             { send_string(" -> "); }
    else for (i = 0; i < state->count; i++) { tap_shift(KC_DOT); }
  }
  else { state->pressed ? register_code(KC_LSFT) : double_tap(state->count, SHIFT, KC_DOT); }
  reset_tap_dance(state);
}

void greater_reset(qk_tap_dance_state_t *state, void *user_data)
{
  unregister_shift(KC_DOT);
  unregister_code (KC_LSFT);
}
#endif

void tilde(qk_tap_dance_state_t *state, void *user_data)
{
  if (mod_down(KC_RSFT)) { // dot, shift -> tilde
    if (state->count > 1) {
      if (state->pressed)                     { register_shift(KC_GRV); }
      else if (state->count == 2)             { send_string("~/"); } 
      else for (i = 0; i < state->count; i++) { tap_shift(KC_GRV); }
    }
    else { state->pressed ? register_shift(KC_GRV) : tap_shift(KC_GRV); }
  }
  else if (state->pressed)                { register_code(KC_DOT); }
  else for (i = 0; i < state->count; i++) { tap_key(KC_DOT); }
  reset_tap_dance(state);
}

void tilde_reset(qk_tap_dance_state_t *state, void *user_data)
{
  unregister_shift(KC_GRV);
  unregister_code (KC_DOT);
  if (mod_down(KC_RSFT)) { register_code(KC_RSFT); } // restore HOME_T, see process_record_user() TD_TILD
}

// ........................................................... Simple Double Tap

void asterisk(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1) { tap_key(KC_DOT); }
  tap_shift(KC_8);
  reset_tap_dance(state);
}

void comma(qk_tap_dance_state_t *state, void *user_data)
{
  tap_key(KC_COMM);
  if (state->count > 1) { tap_key(KC_SPC); }
  reset_tap_dance(state);
}

void dot(qk_tap_dance_state_t *state, void *user_data)
{
  state->count > 1 ? tap_shift(KC_COLN) : tap_key(KC_DOT);
  reset_tap_dance(state);
}

#define IRC_ENTER _delay_ms(10); tap_key(KC_ENT);

void paste(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1)    { mod_key(KC_LCTL, KC_V); IRC_ENTER; }
  else if (state->pressed) { register_code(KC_LCTL); register_code(KC_V); }
  else                     { mod_key(KC_LCTL, KC_V); } 
  reset_tap_dance(state);
}

void paste_reset(qk_tap_dance_state_t *state, void *user_data)
{
  unregister_code(KC_V);
  unregister_code(KC_LCTL);
}

void percent(qk_tap_dance_state_t *state, void *user_data)
{
  if ((state->count > 1) && state->pressed) { register_shift(KC_5); }
  else { state->pressed                     ? register_code(KC_LALT) : double_tap(state->count, SHIFT, KC_5); }
  reset_tap_dance(state);
}

void percent_reset(qk_tap_dance_state_t *state, void *user_data)
{
  unregister_shift(KC_5);
  unregister_code (KC_LALT);
}

#define CTL_SFT_V register_code(KC_LCTL); tap_shift(KC_V); unregister_code(KC_LCTL) 

void xpaste(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1)    { CTL_SFT_V; IRC_ENTER; }
  else if (state->pressed) { register_code(KC_LCTL); register_shift(KC_V); }
  else                     { CTL_SFT_V; }
  reset_tap_dance(state);
}

void xpaste_reset(qk_tap_dance_state_t *state, void *user_data)
{
  unregister_shift(KC_V);
  unregister_code (KC_LCTL);
}

// compile time macro string, see functions/hardware <keyboard> script
void private(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1) { SEND_STRING(PRIVATE_STRING); }
  reset_tap_dance(state);
}

// config.h defined string
void send(qk_tap_dance_state_t *state, void *user_data)
{
  if (state->count > 1) { SEND_STRING(PUBLIC_STRING); }
  reset_tap_dance(state);
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
}

void base_layer(uint8_t defer)
{
  if (defer) { return; } // see process_record_user() reset keys
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
void tap_layer(keyrecord_t *record, uint8_t layer)
{
  record->event.pressed ? layer_on(layer) : layer_off(layer);
}

#ifndef CHIMERA
// LT macro for mapc_shift(), see process_record_user()
void lt(keyrecord_t *record, uint8_t layer, uint16_t keycode)
{
  if (record->event.pressed) {
    key_timer = timer_read();
    layer_on(layer);
  }
  else {
    layer_off(layer);
    if (timer_elapsed(key_timer) < TAPPING_TERM) { tap_key(keycode); }
    // clear_mods();
    key_timer = 0;
  }
}
#endif

// ............................................................ Double Key Layer

#define LEFT   1
#define RIGHT  2
#define ONDOWN 0
#define TOGGLE 1

static uint8_t double_key = 0;

// dual key to raise layer (layer 0 to trap dual key state :-)
bool raise_layer(keyrecord_t *record, uint8_t layer, uint8_t side, uint8_t toggle)
{
  if (record->event.pressed) {
    double_key |= side;
    if (double_key == (LEFT | RIGHT)) { 
      if (layer) { toggle ? layer_invert(layer) : layer_on(layer); }
      return true;
    }
  }
  else {
    double_key &= ~side;
    if (!(double_key || toggle)) { layer_off(layer); } // allow single key to continue on layer :-)
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

#define SWITCH_LAYER(x, y) layer_off(x); x = 0; if (y && (y == _MOUSE)) { layer_on(facing); y = facing; }

// seamlessly switch left / right thumb layer combinations
void rolling_layer(keyrecord_t *record, uint8_t side, uint8_t shift, uint16_t keycode, uint8_t layer, uint8_t facing)
{
  if (record->event.pressed) {
    layer_on(layer);
    if (side == LEFT) { leftside = layer; }
    else              { rightside = layer; }
    key_timer  = timer_read();
  }
  else {
    layer_off(_MOUSE);
    if (keycode && (timer_elapsed(key_timer) < TAPPING_TERM)) { mod_key(shift, keycode); }
    if (side == LEFT)                                         { SWITCH_LAYER(leftside, rightside); }
    else                                                      { SWITCH_LAYER(rightside, leftside); }
    // clear_mods();
    key_timer = 0;
  }
}

// ....................................................................... Steno

void steno(keyrecord_t *record)
{
  if (record->event.pressed) {
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

static uint8_t plover = 0; // plover application run state (0) off (1) on, see wm keybinds

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
