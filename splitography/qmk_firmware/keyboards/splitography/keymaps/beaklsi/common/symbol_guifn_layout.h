
// const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

// ............................................. Symbol Pairs / Navigation Layer

  // ,-----------------------------------------------------------------------------------.
  // |   :  |   .  |   *  |   &  |      |      |      |      | Home |  Up  |  End | PgUp |
  // |------+------+------+------+------+------------------------------------------------|
  // |   ~  |   <  |   %  |   >  |   ?  |      |      |      | Left | Down | Right| PgDn |
  // |------+------+------+------+------+------|-----------------------------------------|
  // |      |   +  |   @  |   !  |   /  |      |      |      |      |      |      |      |
  // |------+------+------+------+------+------+-----------------------------------------|
  // |                           |      |   =  |  f() |      |                           |
  // |      |      |      |      |   =  |      |      |  f() |      |      |      |      |
  // `-----------------------------------------------------------------------------------'

  [_GUIFN] = {
    {HS_COLN, KC_DOT,  KC_ASTR, KC_AMPR, _______, ___x___, ___x___, _______, KC_HOME, KC_UP,   KC_END,  KC_PGUP},
    {SG_TILD, HS_LT,   TD_PERC, HS_GT,   KC_QUES, ___x___, ___x___, _______, KC_LEFT, KC_DOWN, KC_RGHT, KC_PGDN},
    {_______, KC_PLUS, KC_AT,   KC_EXLM, KC_SLSH, ___x___, ___x___, _______, _______, _______, _______, _______},
#ifdef SPLITOGRAPHY
    {_______, _______, _______, _______, _______, ML_EQL,  ___fn__, ___x___, _______, _______, _______, _______},
#else
    {_______, _______, _______, _______, ML_EQL,  _______, _______, ___fn__, ___x___, _______, _______, _______},
#endif
  },

// ................................................................ Symbol Layer

  // ,-----------------------------------------------------------------------------------.
  // |      |      |      |      |      |      |      |   *  |   [  |   ^  |   ]  |      |
  // |--------------------+---------------------------+------+------+------+------+------|
  // |      |      |      |      |      |      |      |   |  |   (  |   $  |   )  |      |
  // |------|-------------+------|------|-------------+------+------+------+------+------|
  // |      |      |      |      |      |      |      |   /  |   {  |   #  |   }  |      |
  // |------+------+------+------+------+------+------+------+------+------+------+------|
  // |                           |      |  f() |   \  |  Del |                           |
  // |      |      |      |      |  f() |      |      |   \  |  Del |      |      |      |
  // `-----------------------------------------------------------------------------------'

  [_SYMBOL] = {
    {_______, _______, _______, _______, _______, ___x___, ___x___, TD_ASTR, KC_LBRC, KC_CIRC, KC_RBRC, _______},
    {_______, _______, _______, _______, _______, ___x___, ___x___, KC_PIPE, KC_LPRN, KC_DLR,  KC_RPRN, _______},
    {_______, _______, _______, _______, _______, ___x___, ___x___, KC_SLSH, KC_LCBR, KC_HASH, KC_RCBR, _______},
#ifdef SPLITOGRAPHY
    {_______, _______, _______, _______, ___x___, ___fn__, ML_BSLS, KC_DEL,  _______, _______, _______, _______},
#else
    {_______, _______, _______, ___x___, ___fn__, _______, _______, ML_BSLS, LT_DEL,  _______, _______, _______},
#endif
  },

// ......................................................... Mouse Pointer Layer

  // .-----------------------------------------------------------------------------------.
  // |      |      |      |      |      |      |      |      | Left |  Up  | Right|  Up  |
  // |-----------------------------------------------------------------------------------|
  // |      | Btn3 | Btn2 | Btn1 |      |      |      |      | Left | Down | Right| Down |
  // |-----------------------------------------------------------------------------------|
  // |      |      |      |      |      |      |      |      |      |      |      |      |
  // |-----------------------------------------------------------------------------------|
  // |                           |      |  f() |  f() |      |                           |
  // |      |      |      |      |  f() |      |      |  f() |      |      |      |      |
  // '-----------------------------------------------------------------------------------'

  [_MOUSE] = {
    {_______, _______, _______, _______, _______, ___x___, ___x___, _______, KC_WH_L, KC_MS_U, KC_WH_R, KC_WH_U},
    {_______, KC_BTN3, KC_BTN2, KC_BTN1, _______, ___x___, ___x___, _______, KC_MS_L, KC_MS_D, KC_MS_R, KC_WH_D},
    {_______, _______, _______, _______, _______, ___x___, ___x___, _______, _______, _______, _______, _______},
#ifdef SPLITOGRAPHY
    {_______, _______, _______, _______, ___x___, ___fn__, ___fn__, ___x___, _______, _______, _______, _______},
#else
    {_______, _______, _______, _______, ___fn__, _______, _______, ___fn__, _______, _______, _______, _______},
#endif
  },
