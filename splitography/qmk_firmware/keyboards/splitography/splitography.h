/* Copyright 2017 REPLACE_WITH_YOUR_NAME
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
#ifndef SPLITOGRAPHY_H
#define SPLITOGRAPHY_H

#include "quantum.h"

// This a shortcut to help you visually see your layout.
// The first section contains all of the arguments
// The second converts the arguments into a two-dimensional array

#define KEYMAP( \
	k00, k01, k02, k03, k04, k05, k06, k07, k08, k09, k0a, k0b,     \
	k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k1a, k1b,     \
	k20, k21, k22, k23, k24, k25, k26, k27, k28, k29, k2a, k2b,     \
	k30, k31, k32, k33, k34, k35, k36, k37, k38, k39, k3a, k3b      \
)                                                                 \
{                                                                 \
	{ k00, k01, k02, k03, k04, k05, k06, k07, k08, k09, k0a, k0b }, \
	{ k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k1a, k1b }, \
	{ k20, k21, k22, k23, k24, k25, k26, k27, k28, k29, k2a, k2b }, \
	{ k30, k31, k32, k33, k34, k35, k36, k37, k38, k39, k3a, k3b }  \
}

#define LAYOUT_splitography(                                                      \
	K00, K01, K02, K03, K04, K05, K06, K07, K08, K09, K0A, K0B,                     \
	K10, K11, K12, K13, K14, K15, K16, K17, K18, K19, K1A, K1B,                     \
	K20, K21, K22, K23, K24, K25, K26, K27, K28, K29, K2A, K2B,                     \
	                    K34, K35, K36, K37                                          \
)                                                                                 \
{                                                                                 \
	{ K00,   K01,   K02,   K03,   K04, K05, K06, K07, K08,   K09,   K0A,   K0B   }, \
	{ K10,   K11,   K12,   K13,   K14, K15, K16, K17, K18,   K19,   K1A,   K1B   }, \
	{ K20,   K21,   K22,   K23,   K24, K25, K26, K27, K28,   K29,   K2A,   K2B   }, \
	{ KC_NO, KC_NO, KC_NO, KC_NO, K34, K35, K36, K37, KC_NO, KC_NO, KC_NO, KC_NO }  \
}

#endif
