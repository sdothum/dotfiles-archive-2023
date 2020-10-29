#include "config.h"  // for ale linter
#include "keycode_functions.h"

// Tap Dance
// ═════════════════════════════════════════════════════════════════════════════

// .......................................................... Tap Dance Keycodes

qk_tap_dance_action_t tap_dance_actions[] = {
  [_ASTR]   = ACTION_TAP_DANCE_FN              (asterisk)
 ,[_COLN]   = ACTION_TAP_DANCE_FN_ADVANCED_TIME(NULL, colon, colon_reset, HASKELL_TERM)
 ,[_COMM]   = ACTION_TAP_DANCE_FN              (comma)
 ,[_DOT]    = ACTION_TAP_DANCE_FN              (dot)
 ,[_EQL]    = ACTION_TAP_DANCE_FN_ADVANCED_TIME(NULL, equal, equal_reset, HASKELL_TERM)
#ifdef HASKELL
 ,[_GT]     = ACTION_TAP_DANCE_FN_ADVANCED_TIME(NULL, greater, greater_reset, HASKELL_TERM)
 ,[_LT]     = ACTION_TAP_DANCE_FN_ADVANCED_TIME(NULL, lesser, lesser_reset, HASKELL_TERM)
#endif
 ,[_PASTE]  = ACTION_TAP_DANCE_FN_ADVANCED     (NULL, paste, paste_reset)
 ,[_PERC]   = ACTION_TAP_DANCE_FN_ADVANCED     (NULL, percent, percent_reset)
 ,[_PRIV]   = ACTION_TAP_DANCE_FN              (private)
 ,[_SEND]   = ACTION_TAP_DANCE_FN              (send)
 ,[_TILD]   = ACTION_TAP_DANCE_FN_ADVANCED     (NULL, tilde, tilde_reset)
 ,[_XPASTE] = ACTION_TAP_DANCE_FN_ADVANCED     (NULL, xpaste, xpaste_reset)
};

// ........................................................... Context Multi Tap

#define TAP      state->count
#define TAPS     TAP > 1
#define TAP_DOWN state->pressed

#define REPEAT(f, k) for (i = 0; i < TAP; i++) { f(k); }

#define DOUBLE_TAP(k, s) if (TAP_DOWN)      { register_code(k); } \
                         else if (TAP == 2) { send_string(s); }   \
                         else REPEAT(tap_key, k);

void colon(STATE, void *user_data)
{
  if (mod_down(KC_RSFT)) {  // handle like map_shift()
#ifdef EMOJI
    if (TAPS)          { DOUBLE_TAP(KC_SCLN, " :-"); }
#else
    if (TAPS)          { REPEAT(tap_key, KC_SCLN); }
#endif
    else               { TAP_DOWN ? register_code(KC_SCLN) : double_tap(TAP, NOSHIFT, KC_SCLN); }
  } else if (TAPS) {
    if (TAP_DOWN)      { register_shift(KC_SCLN); }
#ifdef HASKELL
    else if (TAP == 2) { send_string(" :: "); }
#endif
    else REPEAT(tap_shift, KC_SCLN);
  } else               { TAP_DOWN ? register_shift(KC_SCLN) : double_tap(TAP, SHIFT, KC_SCLN); }
  reset_tap_dance(state);
}

void colon_reset(STATE, void *user_data)
{
  unregister_shift(KC_SCLN);
  if (mod_down(KC_RSFT)) { register_code(KC_RSFT); }  // restore HOME_T, see process_record_user() TD_COLN
}

void equal(STATE, void *user_data)
{
  if (TAPS) { DOUBLE_TAP(KC_EQL, EQLEQL); }
#if NONSTENO
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

#define DOUBLE_SHIFT(k, s) if (TAP_DOWN)          { register_shift(k); } \
                           else if (TAP == 2)     { send_string(s); }    \
                           else REPEAT(tap_shift, k);

#ifdef HASKELL
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
  if (TAPS) { send_string(COMMACOMMA); }
  else      { tap_key(KC_COMM); }
  reset_tap_dance(state);
}

void dot(STATE, void *user_data)
{
  if (biton32(layer_state) == _NUMBER) { TAPS ? tap_shift(KC_COLN) : tap_key(KC_DOT); }
  else                                 { TAPS ? send_string("./") : tap_key(KC_DOT); }  // see symbol layer
  reset_tap_dance(state);
}

#define IRC_ENTER wait_ms(10); \
                  tap_key(KC_ENT)

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

// compile time macro strings, see functions/hardware qmk script
void private(STATE, void *user_data)
{
  if (TAPS) { SEND_STRING(PRIVATE_STRING); }
  reset_tap_dance(state);
}

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
