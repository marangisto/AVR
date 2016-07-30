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
#endif
