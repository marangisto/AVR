#pragma once

#include <AVR/LCD1602A.h>
#include <Arduino/Pins.h>

////
//
//      DFRobot LDC Shield DFR0009 interface for TC1602 LCD
//
////

class dfr0009_tc1602_t
{
public:
    static inline void setup() { pins::setup(); }
    static inline void write(uint8_t x) { pins::write(x); }

private:
    typedef D10 BL;
    typedef D9 E;
    typedef D8 RS;
    typedef D7 DB7;
    typedef D6 DB6;
    typedef D5 DB5;
    typedef D4 DB4;

    typedef outputs_t<DB4, DB5, DB6, DB7, RS, E, BL> pins;
};

typedef tc1602_t<dfr0009_tc1602_t> dfr0009_t;

