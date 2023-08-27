
// tap dance keys
enum tap_dance {
  _ASTR = 0
 ,_BSPC
 ,_COLM
 ,_COMM
 ,_DEL
 ,_DOT
 ,_PERC
 ,_PRIV
 ,_SEND
 ,_SPC
 ,_TILD
#ifdef HASKELL
 ,_COLN
 ,_LT
 ,_GT
 ,_EQL
#endif
};

#define TD_ASTR TD(_ASTR)
#define TD_BSPC TD(_BSPC)
#define TD_COLM TD(_COLM)
#define TD_COMM TD(_COMM)
#define TD_DEL  TD(_DEL)
#define TD_DOT  TD(_DOT)
#define TD_PERC TD(_PERC)
#define TD_PRIV TD(_PRIV)                   // compile time macro string, provided in private_string.h
#define TD_SEND TD(_SEND)                   // config.h defined macro string
#define TD_SPC  TD(_SPC)                    // see process_record_user() for extended handling of Spc
#define TD_TILD TD(_TILD)
#ifdef HASKELL
#define TD_COLN TD(_COLN)
#define TD_LT   TD(_LT)
#define TD_GT   TD(_GT)
#define TD_EQL  TD(_EQL)
#endif
