// sdothum - 2016 (c) wtfpl

// ....................................................................... Corne
 
#ifdef CORNE
void persistent_default_layer_set(uint16_t default_layer) {
  eeconfig_update_default_layer(default_layer);
  default_layer_set(default_layer);
}

void matrix_init_user(void) {
#ifdef RGBLIGHT_ENABLE
  RGB_current_mode = rgblight_config.mode;
#endif
// SSD1306 OLED init, make sure to add #define SSD1306OLED in config.h
#ifdef SSD1306OLED
  iota_gfx_init(!has_usb());  // turns on the display
#endif
  base_layer(0);
}

// SSD1306 OLED update loop, make sure to add #define SSD1306OLED in config.h
#ifdef SSD1306OLED
// When add source files to SRC in rules.mk, you can use functions.
const char *read_layer_state(void);
const char *read_logo(void);
void set_keylog(uint16_t keycode, keyrecord_t *record);
const char *read_keylog(void);
const char *read_keylogs(void);
// const char *read_mode_icon(bool swap);
// const char *read_host_led_state(void);
// void set_timelog(void);
// const char *read_timelog(void);

void matrix_scan_user(void) {
  iota_gfx_task();
}

void matrix_render_user(struct CharacterMatrix *matrix) {
  if (is_master) {
    // If you want to change the display of OLED, you need to change here
    matrix_write_ln(matrix, read_layer_state());
    matrix_write_ln(matrix, read_keylog());
    // matrix_write_ln(matrix, read_keylogs());
    // matrix_write_ln(matrix, read_mode_icon(keymap_config.swap_lalt_lgui));
    // matrix_write_ln(matrix, read_host_led_state());
    // matrix_write_ln(matrix, read_timelog());
  } else {
    matrix_write(matrix, read_logo());
  }
}

void matrix_update(struct CharacterMatrix *dest, const struct CharacterMatrix *source) {
  if (memcmp(dest->display, source->display, sizeof(dest->display))) {
    memcpy(dest->display, source->display, sizeof(dest->display));
    dest->dirty = true;
  }
}

void iota_gfx_task_user(void) {
  struct CharacterMatrix matrix;
  matrix_clear(&matrix);
  matrix_render_user(&matrix);
  matrix_update(&display, &matrix);
}
#endif
#endif

// ...................................................................... Planck

#ifdef PLANCK
void matrix_init_user(void)
{
#ifdef ROLLING
  clear_events();
#endif
#ifdef STENO_ENABLE
  steno_set_mode(STENO_MODE_BOLT);  // or STENO_MODE_GEMINI
#endif
#ifdef AUDIO_ENABLE
  startup_user();
#endif
}

#include "audio.h"
#endif

// ................................................................ Splitography

#ifdef SPLITOGRAPHY
void matrix_init_user(void)
{
#ifdef ROLLING
  clear_events();
#endif
#ifdef STENO_ENABLE
  steno_set_mode(STENO_MODE_BOLT);  // or STENO_MODE_GEMINI
#endif
}
#endif

// ..................................................................... Chimera

#ifdef CHIMERA
void matrix_init_user(void)
{
  base_layer(0);
}

void matrix_scan_user(void) {
  uint8_t layer = biton32(layer_state);
  
  switch (layer) {
  case _BASE:
    set_led_blue;    break;
  case _SHIFT:
  case _TTCAPS:
    set_led_cyan;    break;
  case _NUMBER:
  case _TTNUMBER:
    set_led_green;   break;
  case _REGEX:
  case _SYMGUI:
  case _TTREGEX:
    set_led_red;     break;
  case _MOUSE:
  case _TTCURSOR:
  case _TTMOUSE:
    set_led_magenta; break;
  case _FNCKEY:
  case _TTFNCKEY:
    set_led_green;   break;
  case _EDIT:
  default:
    set_led_yellow;  break;
  }
}
#endif
