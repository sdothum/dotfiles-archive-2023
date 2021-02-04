// sdothum - 2016 (c) wtfpl

#define STATE qk_tap_dance_state_t *state

void asterisk(STATE, void *user_data);
void asterisk_reset(STATE, void *user_data);
void colon(STATE, void *user_data);
void colon_reset(STATE, void *user_data);
void comma(STATE, void *user_data);
void comma_reset(STATE, void *user_data);
void dot(STATE, void *user_data);
void dot_reset(STATE, void *user_data);
void greater(STATE, void *user_data);
void greater_reset(STATE, void *user_data);
void lesser(STATE, void *user_data);
void lesser_reset(STATE, void *user_data);
void paste(STATE, void *user_data);
void paste_reset(STATE, void *user_data);
void percent(STATE, void *user_data);
void percent_reset(STATE, void *user_data);
void private(STATE, void *user_data);
void public(STATE, void *user_data);
void space(STATE, void *user_data);
void space_reset(STATE, void *user_data);
void xpaste(STATE, void *user_data);
void xpaste_reset(STATE, void *user_data);
