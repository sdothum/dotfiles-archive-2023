#ifndef CHIMERA_ERGO_42_H
#define CHIMERA_ERGO_42_H

#include "quantum.h"
#include "matrix.h"
// #include "backlight.h"
#include <stddef.h>

#define red_led_off   PORTF |= (1<<5)
#define red_led_on    PORTF &= ~(1<<5)
#define blu_led_off   PORTF |= (1<<4)
#define blu_led_on    PORTF &= ~(1<<4)
#define grn_led_off   PORTD |= (1<<1)
#define grn_led_on    PORTD &= ~(1<<1)

#define set_led_off     red_led_off; grn_led_off; blu_led_off
#define set_led_red     red_led_on;  grn_led_off; blu_led_off
#define set_led_blue    red_led_off; grn_led_off; blu_led_on
#define set_led_green   red_led_off; grn_led_on;  blu_led_off
#define set_led_yellow  red_led_on;  grn_led_on;  blu_led_off
#define set_led_magenta red_led_on;  grn_led_off; blu_led_on
#define set_led_cyan    red_led_off; grn_led_on;  blu_led_on
#define set_led_white   red_led_on;  grn_led_on;  blu_led_on

/*
#define LED_B 5
#define LED_R 6
#define LED_G 7

#define all_leds_off PORTF &= ~(1<<LED_B) & ~(1<<LED_R) & ~(1<<LED_G)

#define red_led_on   PORTF |= (1<<LED_R)
#define red_led_off  PORTF &= ~(1<<LED_R)
#define grn_led_on   PORTF |= (1<<LED_G)
#define grn_led_off  PORTF &= ~(1<<LED_G)
#define blu_led_on   PORTF |= (1<<LED_B)
#define blu_led_off  PORTF &= ~(1<<LED_B)

#define set_led_off     PORTF &= ~(1<<LED_B) & ~(1<<LED_R) & ~(1<<LED_G)
#define set_led_red     PORTF = PORTF & ~(1<<LED_B) & ~(1<<LED_G) | (1<<LED_R)
#define set_led_blue    PORTF = PORTF & ~(1<<LED_G) & ~(1<<LED_R) | (1<<LED_B)
#define set_led_green   PORTF = PORTF & ~(1<<LED_B) & ~(1<<LED_R) | (1<<LED_G)
#define set_led_yellow  PORTF = PORTF & ~(1<<LED_B) | (1<<LED_R) | (1<<LED_G)
#define set_led_magenta PORTF = PORTF & ~(1<<LED_G) | (1<<LED_R) | (1<<LED_B)
#define set_led_cyan    PORTF = PORTF & ~(1<<LED_R) | (1<<LED_B) | (1<<LED_G)
#define set_led_white   PORTF |= (1<<LED_B) | (1<<LED_R) | (1<<LED_G)
*/

// This a shortcut to help you visually see your layout.
// The first section contains all of the arguements
// The second converts the arguments into a two-dimensional array
#define KC_KEYMAP( \
  k00, k01, k02, k03, k04, k05,      k06, k07, k08, k09, k10, k11, \
  k12, k13, k14, k15, k16, k17,      k18, k19, k20, k21, k22, k23, \
  k24, k25, k26, k27, k28, k29,      k30, k31, k32, k33, k34, k35, \
                 k36, k37, k38,      k39, k40, k41 \
) \
{ \
	{ KC_NO,    KC_##k28,    KC_##k14, KC_##k03, KC_##k16, KC_NO,         KC_##k39, KC_##k19, KC_##k08, KC_##k21, KC_NO,    KC_NO     }, \
	{ KC_NO,    KC_##k25,    KC_##k02, KC_##k15, KC_##k04, KC_##k24,      KC_##k35, KC_##k07, KC_##k20, KC_##k09, KC_##k34, KC_NO     }, \
	{ KC_##k00, KC_##k13,    KC_NO,    KC_##k27, KC_##k17, KC_##k37,      KC_##k40, KC_##k18, KC_##k32, KC_NO,    KC_##k22, KC_##k11  }, \
	{ KC_##k12, KC_##k01,    KC_##k26, KC_NO,    KC_##k05, KC_##k36,      KC_##k41, KC_##k06, KC_NO,    KC_##k33, KC_##k10, KC_##k23  },  \
	{ KC_##k29, KC_##k38,    KC_NO,    KC_##k28, KC_##k25, KC_NO,         KC_##k30, KC_##k31, KC_NO,    KC_NO,    KC_##k34, KC_NO     }, \
}

#define KEYMAP( \
  k00, k01, k02, k03, k04, k05,      k06, k07, k08, k09, k10, k11, \
  k12, k13, k14, k15, k16, k17,      k18, k19, k20, k21, k22, k23, \
  k24, k25, k26, k27, k28, k29,      k30, k31, k32, k33, k34, k35, \
                 k36, k37, k38,      k39, k40, k41 \
) \
{ \
	{ KC_NO, k28,   k14,   k03,   k16, KC_NO,         k39, k19, k08,   k21,   KC_NO, KC_NO  }, \
	{ KC_NO, k25,   k02,   k15,   k04, k24,           k35, k07, k20,   k09,   k34,   KC_NO  }, \
	{ k00,   k13,   KC_NO, k27,   k17, k37,           k40, k18, k32,   KC_NO, k22,   k11    }, \
	{ k12,   k01,   k26,   KC_NO, k05, k36,           k41, k06, KC_NO, k33,   k10,   k23    },  \
	{ k29,   k38,   KC_NO, k28,   k25, KC_NO,         k30, k31, KC_NO, KC_NO, k34,   KC_NO  }, \
}
#endif
