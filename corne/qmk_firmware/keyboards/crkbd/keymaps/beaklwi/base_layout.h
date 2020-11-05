
// const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

// .................................................................... BEAKL ZI

  [_BASE] = KEYMAP(
  // ---------+---------+---------+---------+---------+---------                      ---------+---------+---------+---------+---------+--------- 
      TGL_TL  , HS_COLN , KC_Y    , KC_O    , KC_U    , KC_MINS ,                      KC_G    , KC_D    , KC_N    , KC_M    , KC_Z    , TGL_TR  ,
  // ---------+---------+---------+---------+---------+---------                      ---------+---------+---------+---------+---------+--------- 
      TGL_HL  , HOME_Q  , HOME_H  , HOME_E  , HOME_A  , KC_W    ,                      KC_C    , HOME_T  , HOME_R  , HOME_S  , HOME_V  , TGL_HR  ,
  // ---------+---------+---------+---------+---------+---------                      ---------+---------+---------+---------+---------+--------- 
      TGL_BL  , KC_J    , KC_COMM , KC_DOT  , KC_K    , KC_QUOT ,                      KC_B    , KC_P    , KC_L    , KC_F    , KC_X    , TGL_BR  ,
  // ---------+---------+---------+---------+---------+---------                      ---------+---------+---------+---------+---------+--------- 
                                              LT_ESC  , LT_I    , LT_TAB  ,  LT_ENT  , LT_SPC  , LT_BSPC 
  //                                         ---------+---------+---------  ---------+---------+--------- 
  ),

  [_SHIFT] = KEYMAP(
  // ---------+---------+---------+---------+---------+---------                      ---------+---------+---------+---------+---------+--------- 
       __x__  , KC_SCLN , S(KC_Y) , S(KC_O) , S(KC_U) , KC_UNDS ,                      S(KC_G) , S(KC_D) , S(KC_N) , S(KC_M) , S(KC_Z) ,  __x__  ,
  // ---------+---------+---------+---------+---------+---------                      ---------+---------+---------+---------+---------+---------+
       __x__  , S(KC_Q) , S(KC_H) , S(KC_E) , S(KC_A) , S(KC_W) ,                      S(KC_C) , S(KC_T) , S(KC_R) , S(KC_S) , S(KC_V) ,  __x__  ,
  // ---------+---------+---------+---------+---------+---------                      ---------+---------+---------+---------+---------+---------+
       __x__  , S(KC_J) , KC_EXLM , KC_QUES , S(KC_K) , KC_DQT  ,                      S(KC_B) , S(KC_P) , S(KC_L) , S(KC_F) , S(KC_X) ,  __x__  ,
  // ---------+---------+---------+---------+---------+---------                      ---------+---------+---------+---------+---------+--------- 
                                              KC_ESC  , S(KC_I) , KC_TAB  ,  KC_ENT  , KC_SPC  , KC_BSPC 
  //                                         ---------+---------+---------  ---------+---------+--------- 
  ),
