// sdothum - 2016 (c) wtfpl

// ....................................................................... Audio

#ifdef AUDIO_ENABLE
#ifdef BACKLIGHT_ENABLE
void led_set_user(uint8_t usb_led)
{
  static uint8_t old_usb_led = 0;
  wait_ms(10);  // gets rid of tick
  if (!is_playing_notes()) {
    // if capslock LED is turning on
    if ((usb_led & (1<<USB_LED_CAPS_LOCK)) && !(old_usb_led & (1<<USB_LED_CAPS_LOCK)))      { PLAY_SONG(song_caps_on); }
    // if capslock LED is turning off
    else if (!(usb_led & (1<<USB_LED_CAPS_LOCK)) && (old_usb_led & (1<<USB_LED_CAPS_LOCK))) { PLAY_SONG(song_caps_off); }
  }
  old_usb_led = usb_led;
}
#endif

void startup_user(void)
{
  wait_ms(20);  // gets rid of tick
  PLAY_SONG(song_startup);
}

void shutdown_user(void)
{
  PLAY_SONG(song_goodbye);
  wait_ms(150);
  stop_all_notes();
}

void music_on_user(void)
{
  music_scale_user();
}

void music_scale_user(void)
{
  PLAY_SONG(music_scale);
}
#endif
