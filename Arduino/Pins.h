#pragma once

#include <AVR/Pins.h>

////
//
//      Pin mappings for Arduino UNO
//
////

#if defined(__AVR_ATmega32U4__) // leonardo
typedef output_t<PD, 2> D0;
typedef output_t<PD, 3> D1;
typedef output_t<PD, 1> D2;
typedef output_t<PD, 0> D3;
typedef output_t<PD, 4> D4;
typedef output_t<PC, 6> D5;
typedef output_t<PD, 7> D6;
typedef output_t<PE, 6> D7;

typedef output_t<PB, 4> D8;
typedef output_t<PB, 5> D9;
typedef output_t<PB, 6> D10;
typedef output_t<PB, 7> D11;
typedef output_t<PD, 6> D12;
typedef output_t<PC, 7> D13;

static const uint8_t A0 = 7;
static const uint8_t A1 = 6;
static const uint8_t A2 = 5;
static const uint8_t A3 = 4;
static const uint8_t A4 = 1;
static const uint8_t A5 = 0;

// analog use of digital pins D4 and up
static const uint8_t AD4 = 8;
static const uint8_t AD6 = 10;
static const uint8_t AD8 = 11;
static const uint8_t AD9 = 12;
static const uint8_t AD10 = 13;
static const uint8_t AD12 = 9;
#else                           // uno
typedef output_t<PD, 0> D0;
typedef output_t<PD, 1> D1;
typedef output_t<PD, 2> D2;
typedef output_t<PD, 3> D3;
typedef output_t<PD, 4> D4;
typedef output_t<PD, 5> D5;
typedef output_t<PD, 6> D6;
typedef output_t<PD, 7> D7;

typedef output_t<PB, 0> D8;
typedef output_t<PB, 1> D9;
typedef output_t<PB, 2> D10;
typedef output_t<PB, 3> D11;
typedef output_t<PB, 4> D12;
typedef output_t<PB, 5> D13;

static const uint8_t A0 = 0;
static const uint8_t A1 = 1;
static const uint8_t A2 = 2;
static const uint8_t A3 = 3;
static const uint8_t A4 = 4;
static const uint8_t A5 = 5;
#endif
