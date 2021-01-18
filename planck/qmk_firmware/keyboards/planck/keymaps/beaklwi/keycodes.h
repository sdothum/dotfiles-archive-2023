// sdothum - 2016 (c) wtfpl

#define STENO_LAYER defined(PLANCK) || defined(STENO_ENABLE)

enum keyboard_keycodes {
  BASE = SAFE_RANGE
#if STENO_LAYER
 ,BASE1
 ,BASE2
#endif
#ifdef STENO_ENABLE
 ,PLOVER
#endif
#ifdef ROLLOVER
 ,HOME_Q   // pseudo GUI_T(KC_Q)
 ,HOME_H   // pseudo CTL_T(KC_H)
 ,HOME_E   // pseudo ALT_T(KC_E)
 ,HOME_A   // pseudo SFT_T(KC_A)
 ,HOME_T   // pseudo SFT_T(KC_T)
 ,HOME_R   // pseudo ALT_T(KC_R)
 ,HOME_S   // pseudo CTL_T(KC_S)
#endif
 ,STAGGER  // cycle pinkie home row stagger 0 -> 1 -> 2
 ,PINKY3   // <pinkie>
 ,PINKY2   // pseudo GUI_T(<pinkie>)
 ,PINKY1   // <pinkie>
 ,KEY3     // <pinkie>
 ,KEY2     // <pinkie>
 ,KEY1     // <pinkie>
 ,SHIFT3   // SFT(<pinkie>)
 ,SHIFT2   // SFT(<pinkie>)
 ,SHIFT1   // SFT(<pinkie>)
#ifdef HASKELL
 ,HS_GT    // pseudo SFT_T(S(KC_DOT))
 ,HS_LT    // pseudo CTL_T(S(KC_COMM))
#endif
 ,HEXCASE  // capslock hex
 ,HEX_A
 ,HEX_B    // pseudo MT(MOD_LALT | MOD_LCTL, S(KC_B))
 ,HEX_C
 ,HEX_D    // pseudo CTL_T(S(KC_D))
 ,HEX_E    // pseudo ALT_T(S(KC_E))
 ,HEX_F    // pseudo SFT_T(S(KC_F))
 ,BRKTYPE  // cycle brkts [] -> () -> {}
 ,L_BRKT
 ,R_BRKT   // pseudo MT(MOD_LALT | MOD_LSFT, R_BRKT)
 ,SMART
 ,DELIM
#ifdef SPLITOGRAPHY
 ,LT_SPC
 ,ML_BSLS
#else
 ,TT_ESC
#endif
 ,TT_A     // pseudo LT(_TTBASEL, S(KC_A))
 ,TT_I     // pseudo LT(_REGEX,   S(KC_I))
 ,TT_T     // pseudo LT(_TTBASER, S(KC_T))
 ,TT_SPC   // pseudo LT(_SYMGUI,  KC_SPC)
};

#ifndef ROLLOVER
#define HOME_Q  GUI_T(KC_Q)
#define HOME_H  CTL_T(KC_H)
#define HOME_E  ALT_T(KC_E)
#define HOME_A  SFT_T(KC_A)
#define HOME_T  SFT_T(KC_T)
#define HOME_R  ALT_T(KC_R)
#define HOME_S  CTL_T(KC_S)
#endif

// pass through keycodes
#define __x__   KC_TRNS
#define ___     KC_NO
#ifndef STENO_ENABLE
#define PLOVER  KC_NO
#endif

// tap dance macros
#ifndef EQLEQL
#define HS_EQL  TD_EQL
#else
#define HS_EQL  KC_EQL
#endif
#ifdef HASKELL
#define HS_COLN TD_COLN
#define HS_GT   TD_GT
#define HS_LT   TD_LT
#else
#define HS_COLN KC_COLN
#define HS_GT   KC_GT
#define HS_LT   KC_LT
#endif
#ifdef UNIX
#define HS_TILD TD_TILD
#else
#define HS_TILD KC_TILD
#endif

// editing macros
#define COPY    LCTL(KC_C)
#define CUT     LCTL(KC_X)
#define EOT     LCTL(KC_D)
#define NAK     LCTL(KC_U)
#define PASTE   TD_PASTE
#define UNDO    LCTL(KC_Z)
#define XCOPY   LCTL(LSFT(KC_C))
#define XPASTE  TD_XPASTE

// thumb keys
#ifdef SPLITOGRAPHY
#define LT_BSPC LT(_EDIT, KC_BSPC)
#define LT_ESC  LT(_NUMBER, KC_ESC)
#define TT_ESC  MO(_NUMBER)
#ifdef ROLLOVER
#define LT_I    MO(_REGEX)   // plus mod_roll() -> LT(_REGEX, KC_I)
#else
#define LT_I    LT(_REGEX, KC_I)
#endif
#else
#define BKTAB   S (KC_TAB)
#define LT_BSPC LT(_MOUSE, KC_BSPC)
#define LT_ESC  LT(_FNCKEY, KC_ESC)
#ifdef ROLLOVER
#define LT_I    MO(_REGEX)   // plus mod_roll() -> LT(_REGEX, KC_I)
#define LT_ENT  MO(_EDIT)    // plus mod_roll() -> LT(_EDIT, KC_ENT)
#define LT_SPC  MO(_SYMGUI)  // plus mod_roll() -> LT(_SYMGUI, KC_SPC)
#else
#define LT_I    LT(_REGEX, KC_I)
#define LT_ENT  LT(_EDIT, KC_ENT)
#define LT_SPC  LT(_SYMGUI, KC_SPC)
#endif
#endif
#define LT_TAB  LT(_NUMBER, KC_TAB)

// fnkey layer macros
#define OS_ALT  OSM(MOD_LALT)
#define OS_CTL  OSM(MOD_LCTL)
#define OS_GUI  OSM(MOD_LGUI)
#define OS_SFT  OSM(MOD_LSFT)

// layer toggle macros
#define TGL_TL  TT(_TTFNCKEY)
#define TGL_HL  TT(_TTCAPS)
#define TGL_BL  TT(_TTMOUSE)
#define TGL_TR  TT(_TTREGEX)
#define TGL_HR  TT(_TTNUMBER)
#define TGL_BR  TT(_TTCURSOR)
#ifdef PLANCK
#define MO_ADJ  MO(_ADJUST)
#endif

#ifdef TEST
#define FLASH   TG(_TEST)
#else
#ifdef CORNE
#define FLASH   RESET
#else
#define FLASH   KC_NO
#endif
#endif
