// sdothum - 2016 (c) wtfpl

#define RECORD keyrecord_t *record

bool key_press(RECORD);
void layer_toggle(RECORD, uint8_t layer, bool upcase, uint16_t keycode);
bool leader_cap(RECORD, uint8_t layer, uint16_t keycode);
bool map_shift(RECORD, uint16_t sftcode, bool upcase, uint16_t keycode);
bool map_shift_event(RECORD, uint16_t sftcode, bool upcase, uint16_t keycode, uint8_t column);
bool mod_roll(RECORD, uint16_t modifier, bool upcase, uint16_t keycode, uint8_t column);
void mod_tap(RECORD, uint16_t modifier, bool upcase, uint16_t keycode);
void press(RECORD, bool upcase, uint16_t keycode);
bool raise_layer(RECORD, uint8_t layer, uint8_t side, bool invert);
void rolling_layer(RECORD, uint8_t side, uint8_t layer, uint8_t facing);
void set_leadercap(RECORD, uint16_t keycode, uint8_t column);
void steno(RECORD);
void toggle(RECORD, uint16_t modifier, uint16_t keycode);
void tt_escape(RECORD, uint16_t keycode);
void type(RECORD, bool upcase, uint16_t keycode);

void base_layer(uint8_t defer);
void clear_events(void);
void clear_layers(void);
void oneshot_shift(uint8_t layer);
void roll_key(bool upcase, uint16_t keycode, uint8_t column);
void toggle_plover(uint8_t state);
