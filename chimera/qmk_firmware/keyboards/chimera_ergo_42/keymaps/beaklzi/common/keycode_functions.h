void asterisk(qk_tap_dance_state_t *state, void *user_data);
void base_layer(uint8_t defer);
bool is_chained_modifier(void);
void clear_layers(void);
void colon(qk_tap_dance_state_t *state, void *user_data);
void colon_reset(qk_tap_dance_state_t *state, void *user_data);
void comma(qk_tap_dance_state_t *state, void *user_data);
void dot(qk_tap_dance_state_t *state, void *user_data);
void double_tap(uint8_t count, uint8_t shift, uint16_t keycode);
void equal(qk_tap_dance_state_t *state, void *user_data);
void equal_reset(qk_tap_dance_state_t *state, void *user_data);
void greater(qk_tap_dance_state_t *state, void *user_data);
void greater_reset(qk_tap_dance_state_t *state, void *user_data);
bool key_press(keyrecord_t *record);
bool leader_cap(keyrecord_t *record, uint8_t layer, uint8_t autocap, uint16_t keycode);
void lesser(qk_tap_dance_state_t *state, void *user_data);
void lesser_reset(qk_tap_dance_state_t *state, void *user_data);
void lt(keyrecord_t *record, uint8_t layer, uint8_t shift, uint16_t keycode);
bool map_shift(keyrecord_t *record, uint16_t shift_key, uint8_t shift, uint16_t keycode);
bool mapc_shift(keyrecord_t *record, uint16_t shift_key, uint8_t shift, uint16_t keycode);
void mod_bits(keyrecord_t *record, uint16_t keycode);
bool mod_down(uint16_t keycode);
void mod_home(keyrecord_t *record, uint8_t side, uint8_t shift, uint16_t modifier, uint16_t keycode, uint16_t* key_timer);
void mod_key(uint16_t modifier, uint16_t keycode);
void modifier(void (*f)(uint8_t));
void mt_shift(keyrecord_t *record, uint16_t modifier, uint16_t modifier2, uint16_t keycode);
void paste(qk_tap_dance_state_t *state, void *user_data);
void paste_reset(qk_tap_dance_state_t *state, void *user_data);
void percent(qk_tap_dance_state_t *state, void *user_data);
void percent_reset(qk_tap_dance_state_t *state, void *user_data);
void pound(qk_tap_dance_state_t *state, void *user_data);
void private(qk_tap_dance_state_t *state, void *user_data);
bool raise_layer(keyrecord_t *record, uint8_t layer, uint8_t side, uint8_t toggle);
void register_shift(uint16_t keycode);
void rolling_layer(keyrecord_t *record, uint8_t side, uint8_t shift, uint16_t keycode, uint8_t layer, uint8_t facing);
void send(qk_tap_dance_state_t *state, void *user_data);
void space(qk_tap_dance_state_t *state, void *user_data);
void space_reset(qk_tap_dance_state_t *state, void *user_data);
void steno(keyrecord_t *record);
void tap_key(uint16_t keycode);
void tap_layer(keyrecord_t *record, uint8_t layer);
void tap_shift(uint16_t keycode);
void tilde(qk_tap_dance_state_t *state, void *user_data);
void tilde_reset(qk_tap_dance_state_t *state, void *user_data);
void toggle_plover(uint8_t state);
void tt_escape(keyrecord_t *record, uint16_t keycode);
void unregister_shift(uint16_t keycode);
void xpaste(qk_tap_dance_state_t *state, void *user_data);
void xpaste_reset(qk_tap_dance_state_t *state, void *user_data);
