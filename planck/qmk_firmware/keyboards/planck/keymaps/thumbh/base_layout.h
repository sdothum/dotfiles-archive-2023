
// const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

// ..................................................................... BEAKL H
#ifdef DEFAULT
  // ,-----------------------------------------------------------------------------------.
  // |   ;  |   Y  |   O  |   U  |   Z  |  Fn  | Caps |   G  |   D  |   N  |   M  |   X  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   Q  |   I  |   E  |   A  |   .  |Cursor| Mouse|   C  |   T  |   R  |   S  |   W  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   J  |   -  |   '  |   K  |   ,  |  Num | Regex|   B  |   P  |   L  |   F  |   V  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt |  Esc |   H  |  Ins | Left | Space| Bksp | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'

  [_BASE] = {
    {KC_SCLN, KC_Y,    KC_O,    KC_U,    KC_Z,    CNTR_TL, CNTR_TR, KC_G,    KC_D,    KC_N,    KC_M,    KC_X   },
    {HOME_Q,  HOME_I,  HOME_E,  HOME_A,  KC_DOT,  CNTR_HL, CNTR_HR, KC_C,    HOME_T,  HOME_R,  HOME_S,  HOME_W },
    {KC_J,    KC_MINS, KC_QUOT, KC_K,    KC_COMM, CNTR_BL, CNTR_BR, KC_B,    KC_P,    KC_L,    KC_F,    KC_V   },
    {OS_CTL,  OS_GUI,  OS_ALT,  LT_ESC,  LT_H,    LT_INS,  LT_LEFT, TD_SPC,  LT_BSPC, AT_DOWN, GT_UP,   CT_RGHT},
  },

  [_SHIFT] = {
    {KC_COLN, S(KC_Y), S(KC_O), S(KC_U), S(KC_Z), CNTR_TL, CNTR_TR, S(KC_G), S(KC_D), S(KC_N), S(KC_M), S(KC_X)},
    {S(KC_Q), S(KC_I), S(KC_E), S(KC_A), KC_QUES, CNTR_HL, CNTR_HR, S(KC_C), S(KC_T), S(KC_R), S(KC_S), S(KC_W)},
    {S(KC_J), KC_UNDS, KC_DQT,  S(KC_K), KC_EXLM, CNTR_BL, CNTR_BR, S(KC_B), S(KC_P), S(KC_L), S(KC_F), S(KC_V)},
    {OS_CTL,  OS_GUI,  OS_ALT,  KC_ESC,  S(KC_H), LT_INS,  LT_LEFT, KC_SPC,  KC_BSPC, AT_DOWN, GT_UP,   CT_RGHT},
  },

  // ,-----------------------------------------------------------------------------------.
  // |   ;  |   Y  |   O  |   U  |   Z  |  Fn  | Caps |   G  |   D  |   N  |   M  |   X  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   Q  |   I  |   E  |   A  |   ~  |Cursor| Mouse|   C  |   T  |   R  |   S  |   W  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   J  |   -  |   '  |   K  |   `  |  Num | Regex|   B  |   P  |   L  |   F  |   V  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt |  Tab |  f() |  Ins | Left | Enter|  Del | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'
  // thumb side lower case to ovoid triggering same hand upper case

  [_LSHIFT] = {
    {KC_SCLN, KC_Y,    KC_O,    KC_U,    KC_Z,    CNTR_TL, CNTR_TR, S(KC_G), S(KC_D), S(KC_N), S(KC_M), S(KC_X)},
    {KC_Q,    KC_I,    KC_E,    KC_A,    TD_TILD, CNTR_HL, CNTR_HR, S(KC_C), S(KC_T), S(KC_R), S(KC_S), S(KC_W)},
    {KC_J,    KC_MINS, KC_QUOT, KC_K,    KC_GRV,  CNTR_BL, CNTR_BR, S(KC_B), S(KC_P), S(KC_L), S(KC_F), S(KC_V)},
    {OS_CTL,  OS_GUI,  OS_ALT,  KC_TAB,  ___fn__, LT_INS,  S_LEFT,  TD_ENT,  LT_DEL,  S_DOWN,  S_UP,    S_RGHT },
  },

  // ,-----------------------------------------------------------------------------------.
  // |   :  |   Y  |   O  |   U  |   Z  |  Fn  | Caps |   G  |   D  |   N  |   M  |   X  |
  // |------+------+------+------+------+-------------+------+------+------+------+------|
  // |   Q  |   I  |   E  |   A  |   ?  |Cursor| Mouse|   C  |   T  |   R  |   S  |   W  |
  // |------+------+------+------+------+------|------+------+------+------+------+------|
  // |   J  |   _  |   "  |   K  |   /  |  Num | Regex|   B  |   P  |   L  |   F  |   V  |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // | Ctrl |  GUI |  Alt |  Tab |   H  |  Ins | Left |  f() |  Del | Down |  Up  | Right|
  // `-----------------------------------------------------------------------------------'
  // thumb side lower case to ovoid triggering same hand upper case

  [_RSHIFT] = {
    {KC_COLN, S(KC_Y), S(KC_O), S(KC_U), S(KC_Z), CNTR_TL, CNTR_TR, KC_G,    KC_D,    KC_N,    KC_M,    KC_X   },
    {S(KC_Q), S(KC_I), S(KC_E), S(KC_A), KC_QUES, CNTR_HL, CNTR_HR, KC_C,    KC_T,    KC_R,    KC_S,    KC_W   },
    {S(KC_J), KC_UNDS, KC_DQT,  S(KC_K), KC_SLSH, CNTR_BL, CNTR_BR, KC_B,    KC_P,    KC_L,    KC_F,    KC_V   },
    {OS_CTL,  OS_GUI,  OS_ALT,  KC_TAB,  S(KC_H), LT_INS,  S_LEFT,  ___fn__, KC_DEL,  S_DOWN,  S_UP,    S_RGHT },
  },
#ifdef HOME_MODS
  // ,-----------------------------------------------------------------------------------.
  // |      |      |      |      |      |      |      |   *  |   [  |   ^  |   ]  |      |
  // |--------------------+---------------------------+------+------+------+------+------|
  // |      |      |      |      |      |      |      |   |  |   (  |   $  |   )  |      |
  // |------|-------------+------|------|-------------+------+------+------+------+------|
  // |      |      |      |      |      |      |      |   /  |   {  |   #  |   }  |      |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // |      |      |      |  f() |      |      |      |   \  |  Del |      |      |      |
  // `-----------------------------------------------------------------------------------'

  [_LSYMBOL] = {
    {_______, _______, _______, _______, _______, ___x___, ___x___, TD_ASTR, KC_LBRC, KC_CIRC, KC_RBRC, _______},
    {_______, _______, _______, _______, _______, ___x___, ___x___, KC_PIPE, KC_LPRN, KC_DLR,  KC_RPRN, _______},
    {_______, _______, _______, _______, _______, ___x___, ___x___, KC_SLSH, KC_LCBR, KC_HASH, KC_RCBR, _______},
    {_______, _______, _______, ___fn__, ___x___, _______, _______, KC_BSLS, SL_DEL,  _______, _______, _______},
  },

  // ,-----------------------------------------------------------------------------------.
  // |   :  |   .  |   *  |   &  |      |      |      |      | Home |  Up  |  End | PgUp |
  // |------+------+------+------+------+------------------------------------------------|
  // |   ~  |   <  |   %  |   >  |   ?  |      |      |      | Left | Down | Right| PgDn |
  // |------+------+------+------+------+------|-----------------------------------------|
  // |      |   +  |   @  |   !  |   /  |      |      |      |      |      |      |      |
  // |------+------+------+------+------+------+-----------------------------------------|
  // |      |      |      | ↑Tab |   =  |      |      |      |  f() |      |      |      |
  // `-----------------------------------------------------------------------------------'

  [_RSYMBOL] = {
    {HS_COLN, KC_DOT,  KC_ASTR, KC_AMPR, _______, ___x___, ___x___, _______, KC_HOME, KC_UP,   KC_END,  KC_PGUP},
    {SG_TILD, HS_LT,   SA_PERC, HS_GT,   KC_QUES, ___x___, ___x___, _______, KC_LEFT, KC_DOWN, KC_RGHT, KC_PGDN},
    {_______, KC_PLUS, KC_AT,   KC_EXLM, KC_SLSH, ___x___, ___x___, _______, _______, _______, _______, _______},
    {_______, _______, _______, SL_TAB,  HS_EQL,  _______, _______, ___x___, ___fn__, _______, _______, _______},
  },
#endif
#endif
