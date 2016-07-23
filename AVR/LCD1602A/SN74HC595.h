#pragma once

#include <AVR/LCD1602A.h>
#include <AVR/SN74HC595.h>

////
//
//      SN74HC595 interface for TC1602 LCD
//
//      parameters are DT = Data, CK = Clock, LT = Latch pins for 74595 chip
//
//      It is assumed that the 74595 is wired to the TC1602 as follows:
//
//          Q0, Q1, Q2, Q3  -> DB4, DB5, DB6, DB7
//          Q4              -> RS
//          Q5              -> E
//          Q6              -> BL (Back-Light)
//
////

template<class DT, class CK, class LT>
class sn74hc595_tc1602_t
{
public:
    static inline void setup() { shift_register::setup(); }
    static inline void write(uint8_t x) { shift_register::write(x); }

private:
	typedef sn74hc595_t<DT, CK, LT, LSB_FIRST> shift_register;
};

