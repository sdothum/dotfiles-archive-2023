// sdothum - 2016 (c) wtfpl

// const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = KEYMAP( 

// ......................................................... Mouse Pointer Layer

  [_MOUSE] = KEYMAP( 
  // ---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------
        ___   ,   ___   ,   ___   ,   ___   ,   ___   ,  __x__  ,  __x__  ,   ___   , KC_WH_L , KC_MS_U , KC_WH_R , KC_WH_U ,
  // ---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------
        ___   , KC_BTN3 , KC_BTN2 , KC_BTN1 ,   ___   ,  __x__  ,  __x__  ,   ___   , KC_MS_L , KC_MS_D , KC_MS_R , KC_WH_D ,
  // ---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------
        ___   ,   ___   ,   ___   ,   ___   ,   ___   ,  __x__  ,  __x__  ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,
  // ---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------
        ___   ,   ___   ,   ___   , MO_ADJ  ,   ___   ,   ___   ,   ___   ,   ___   ,  __x__  ,   ___   ,   ___   ,   ___   
  // ---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------
  ),

// .................................................................. Short Cuts

  [_EDIT] = KEYMAP( 
  // ---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------
        ___   ,   ___   , XCOPY   , XPASTE  , TD_PRIV ,   ___   ,   ___   ,   ___   , KC_HOME , KC_UP   , KC_END  , KC_PGUP ,
  // ---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+--------- 
      UNDO    , CUT     , COPY    , PASTE   , TD_PUB  ,   ___   ,   ___   ,   ___   , KC_LEFT , KC_DOWN , KC_RGHT , KC_PGDN ,
  // ---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+--------- 
        ___   ,   ___   , NAK     , EOT     , PLOVER  ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,
  // ---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------
        ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,  __x__  ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   
  // ---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------
  ),

// ................................................................ Adjust Layer

  [_ADJUST] = KEYMAP( 
  // ---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------
      PLOVER  ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,
  // ---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------
      AU_ON   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,
  // ---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------
      RESET   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,   ___   ,
  // ---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------
      STAGGER ,   ___   ,   ___   ,  __x__  ,   ___   ,   ___   ,   ___   ,   ___   ,  __x__  ,   ___   ,   ___   ,   ___   
  // ---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------+---------
  ),
