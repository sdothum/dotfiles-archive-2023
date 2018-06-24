
// bool process_record_user(uint16_t keycode, keyrecord_t *record)
// {
//   switch (keycode) {

    case PS_STNA:
      stn_layer(record, STN_A, _TXBNUMBER);
      break;
    case PS_STNO:
      stn_layer(record, STN_O, _TXBFKEY);
      break;
    case PS_STNE:
      stn_layer(record, STN_E, _TXBEDIT);
      break;
    case PS_STNU:
      stn_layer(record, STN_U, _TXBGUIFN);
      break;
    case PS_SYM:
      stn_layer(record, 0, _TXBSYMBOL);
      break;
    case PS_MOUS:
      stn_layer(record, 0, _TXBMOUSE);
      break;
    case PS_QENT:
      stn_layer(record, STN_U, _TXBRSHIFT);
      break;
    case PS_QSPC:
      stn_layer(record, STN_U, _TXBLSHIFT);
      break;
    case PS_BASE:
      if (record->event.pressed) {
#ifdef AUDIO_ENABLE
        PLAY_SONG(song_qwerty);
#endif
        layer_on(_TXBBASE);
      }
      break;

//   }
//   return true;
// }
