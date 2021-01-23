// sdothum - 2016 (c) wtfpl

// bool process_record_user(uint16_t keycode, keyrecord_t *record)
// {
//   switch (keycode) {

// ............................................................. Left Thumb Keys

  case TT_ESC:
    numerating = KEY_DOWN ? 1 : 0;
    if (map_shift(record, KC_LSFT, UPPER, KC_TAB))  { return false; }
    if (map_shift(record, KC_RSFT, LOWER, KC_TAB))  { return false; }
    if (key_press(record))                          { base_layer(0); return false; }  // exit TT layer
    break;
  case LT_ESC:
    numerating = KEY_DOWN ? 1 : 0;
    if (raise_layer(record, _FNCKEY, LEFT, ONDOWN)) { return false; }
    if (map_shift(record, KC_LSFT, UPPER, KC_TAB))  { return false; }
    if (map_shift(record, KC_RSFT, LOWER, KC_TAB))  { return false; }
    if (tt_keycode)                                 { base_layer(0); return false; }
    break;

  case LT_I:
    if (raise_layer(record, _FNCKEY, RIGHT, ONDOWN)) { return false; }
#ifdef LEFT_SPC_ENT
    if (map_shift(record, KC_LSFT, LOWER, KC_SPC))   { return false; }
#endif
#ifdef ROLLOVER
    if (mod_roll(record, LEFT, LOWER, 0, KC_I, 4))   { return false; }  // MO(_REGEX) -> layer_toggle(_REGEX, KC_I)
#endif
    rolling_layer(record, LEFT, 0, 0, _REGEX, _SYMGUI);
    break;
  case TT_I:
    layer_toggle(record, _REGEX, UPPER, KC_I);
    break;

  case TD_EQL:
    if (tt_keycode) { break; }  // no thumb mouse layer on toggle layer
#ifdef LEFT_SPC_ENT
    if (map_shift(record, KC_LSFT, LOWER, KC_ENT)) { return false; }
#endif
    rolling_layer(record, LEFT, 0, 0, _MOUSE, _SYMGUI);        // equal handled by tapdance
    break;

// ............................................................ Right Thumb Keys

  case ML_BSLS:
    rolling_layer(record, RIGHT, 0, KC_BSLS, _MOUSE, _REGEX);  // see TD_EQL
    break;

  case LT_SPC:
#ifdef THUMB_CAPS
    if (raise_layer(record, _TTCAPS, LEFT, INVERT))           { return false; }
#endif
    if (map_shifted(record, KC_LSFT, LOWER, KC_ENT, _SYMGUI)) { return false; }  // rolling cursor to enter
    if (map_shift(record, KC_RSFT, LOWER, KC_ENT))            { return false; }
#ifdef ROLLOVER
    leaderlayer = _SYMGUI;                                                       // see mod_roll()
    if (mod_roll(record, RIGHT, LOWER, 0, KC_SPC, 11))        { return false; }  // KC_SPC -> space shift
#else
    if (leader_cap (record, _SYMGUI, KC_SPC))                 { return false; }  // KC_SPC -> space shift
#endif
    rolling_layer(record, RIGHT, 0, 0, _SYMGUI, _REGEX);
    break;
  case TT_SPC:
#ifdef THUMB_CAPS
    if (raise_layer(record, _TTCAPS, LEFT, INVERT))           { return false; }
#endif
    if (map_shifted(record, KC_LSFT, LOWER, KC_ENT, _SYMGUI)) { return false; }  // rolling cursor to enter
    if (map_shift(record, KC_RSFT, LOWER, KC_ENT))            { return false; }
    layer_toggle(record, _SYMGUI, LOWER, KC_SPC);  // because layer_toggle() issues <spc> before <enter> on map_shifted()
    break;
  case KC_SPC:
    if (!KEY_DOWN) { CLR_1SHOT; }  // see leader_cap()
    break;

  case LT_BSPC:
    if (!KEY_DOWN) { CLR_1SHOT; }  // see leader_cap()
#ifdef THUMB_CAPS
    if (raise_layer(record, _TTCAPS, RIGHT, INVERT))  { return false; }
#endif
    if (map_shift(record, KC_LSFT, LOWER, KC_DEL))    { layer_off(_SYMGUI); return false; }  // rolling cursor to del
    if (map_shift(record, KC_RSFT, LOWER, KC_DEL))    { return false; }
    if (leader_cap (record, _EDIT, KC_ENT))           { return false; }  // see KC_BSPC for multi-tap
    break;
  case KC_BSPC:
    if (!KEY_DOWN) { CLR_1SHOT; }  // see leader_cap()
#ifdef THUMB_CAPS
    if (raise_layer(record, _TTCAPS, RIGHT, INVERT))  { return false; }
#endif
    if (map_shift(record, KC_LSFT, LOWER, KC_DEL))    { layer_off(_SYMGUI); return false; }  // rolling cursor to del
    if (map_shift(record, KC_RSFT, LOWER, KC_DEL))    { return false; }
    if (leader_cap (record, 0, KC_ENT))               { return false; }  // KC_BSPC from LT_BSPC -> (enter)* enter shift
#ifdef THUMB_CAPS
    if (KEY_DOWN) { key_timer = timer_read(); }
    else if (timer_elapsed(key_timer) < TAPPING_TERM) { TAP(KC_BSPC); }
    return false;  // capslock toggling trap, use shift bspc -> del for auto repeat
#else
    break;
#endif
