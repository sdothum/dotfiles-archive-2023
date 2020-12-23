#include "config.h"  // for ale linter
#include "keycode_functions.h"

// Tap Dance
// ═════════════════════════════════════════════════════════════════════════════

// .......................................................... Tap Dance Keycodes

qk_tap_dance_action_t tap_dance_actions[] = {
  [_ASTR]   = ACTION_TAP_DANCE_FN_ADVANCED     (NULL, asterisk, asterisk_reset)
 ,[_COMM]   = ACTION_TAP_DANCE_FN_ADVANCED     (NULL, comma, comma_reset)
 ,[_DOT]    = ACTION_TAP_DANCE_FN_ADVANCED     (NULL, dot, dot_reset)
#ifndef EQLEQL
 ,[_EQL]    = ACTION_TAP_DANCE_FN_ADVANCED_TIME(NULL, equal, equal_reset, HASKELL_TERM)
#endif
#ifdef HASKELL
 ,[_COLN]   = ACTION_TAP_DANCE_FN_ADVANCED_TIME(NULL, colon, colon_reset, HASKELL_TERM)
 ,[_GT]     = ACTION_TAP_DANCE_FN_ADVANCED_TIME(NULL, greater, greater_reset, HASKELL_TERM)
 ,[_LT]     = ACTION_TAP_DANCE_FN_ADVANCED_TIME(NULL, lesser, lesser_reset, HASKELL_TERM)
#endif
#ifdef UNIX
 ,[_TILD]   = ACTION_TAP_DANCE_FN_ADVANCED     (NULL, tilde, tilde_reset)
#endif
 ,[_PASTE]  = ACTION_TAP_DANCE_FN_ADVANCED     (NULL, paste, paste_reset)
 ,[_PRIV]   = ACTION_TAP_DANCE_FN              (private)
 ,[_PUB]    = ACTION_TAP_DANCE_FN              (public)
 ,[_XPASTE] = ACTION_TAP_DANCE_FN_ADVANCED     (NULL, xpaste, xpaste_reset)
};

// .......................................................... Double Tap Strings

#define TAP      state->count
#define TAPS     TAP > 1
#define TAP_DOWN state->pressed

#define ON_TAPS_OR_SHIFT(s, k) TAPS ? send_string (s) : register_shift(k); reset_tap_dance(state)

void asterisk(STATE, void *user_data)
{
  ON_TAPS_OR_SHIFT(".*", KC_8);
}

void asterisk_reset(STATE, void *user_data)
{
  unregister_shift(KC_8);
}

void colon(STATE, void *user_data)
{
  if (mod_down(KC_RSFT)) {  // handle like map_shift()
#ifdef EMOJI
    TAPS ? send_string(" :-") : register_code(KC_SCLN);
#else
    register_code(KC_SCLN);
#endif
  } else {
#ifdef HASKELL
    if (TAPS) { send_string   (" :: "); }
    else      { register_shift(KC_SCLN); }
#else
    register_shift(KC_SCLN); 
#endif
  }
  reset_tap_dance(state);
}

void colon_reset(STATE, void *user_data)
{
  unregister_code (KC_SCLN);
  unregister_shift(KC_SCLN);
  if (mod_down(KC_RSFT)) { register_code(KC_RSFT); }  // restore HOME_T, see process_record_user() TD_COLN
}

#ifndef EQLEQL
void equal(STATE, void *user_data)
{
  if (TAPS) { send_string  ("=~"); }
  else      { register_code(KC_EQL); }
  reset_tap_dance(state);
}

void equal_reset(STATE, void *user_data)
{
  unregister_code(KC_EQL);
}
#endif

#ifdef HASKELL
void greater(STATE, void *user_data)
{
  ON_TAPS_OR_SHIFT(" -> ", KC_DOT);
}

void greater_reset(STATE, void *user_data)
{
  unregister_shift(KC_DOT);
}

void lesser(STATE, void *user_data)
{
  ON_TAPS_OR_SHIFT(" <- ", KC_COMM);
}

void lesser_reset(STATE, void *user_data)
{
  unregister_shift(KC_COMM);
}
#endif

#ifdef UNIX
void tilde(STATE, void *user_data)
{
  ON_TAPS_OR_SHIFT("~/", KC_GRV);
}

void tilde_reset(STATE, void *user_data)
{
  unregister_shift(KC_GRV);
  if (mod_down(KC_RSFT)) { register_code(KC_RSFT); }  // restore HOME_T, see process_record_user() TD_TILD
}
#endif

// ........................................................ Double Tap Character

void comma(STATE, void *user_data)
{
#ifdef COMMASPACE
  if (TAPS) { send_string  (", "); }
#else
  if (TAPS) { register_code(KC_SCLN); }
#endif
  else      { register_code(KC_COMM); }
  reset_tap_dance(state);
}

void comma_reset(STATE, void *user_data)
{
  unregister_code (KC_COMM);
  unregister_code (KC_SCLN);
}

void dot(STATE, void *user_data)
{
  TAPS ? register_shift(KC_SCLN) : register_code (KC_DOT);
  reset_tap_dance(state);
}

void dot_reset(STATE, void *user_data)
{
  unregister_code (KC_DOT);
  unregister_shift(KC_SCLN);
}

// ............................................................... Paste Actions

#define IRC_ENTER _delay_ms(10); tap_key(KC_ENT)

void paste(STATE, void *user_data)
{
  if (TAPS)          { mod_key      (KC_LCTL, KC_V); IRC_ENTER; }
  else if (TAP_DOWN) { register_code(KC_LCTL); register_code(KC_V); }
  else               { mod_key      (KC_LCTL, KC_V); }
  reset_tap_dance(state);
}

void paste_reset(STATE, void *user_data)
{
  unregister_code(KC_V);
  unregister_code(KC_LCTL);
}

// compile time macro strings, see functions/hardware qmk script
void private(STATE, void *user_data)
{
  if (TAPS) { SEND_STRING(PRIVATE_STRING); }
  reset_tap_dance(state);
}

void public(STATE, void *user_data)
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
