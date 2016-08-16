#pragma once

#include <AVR/ADC.h>
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

enum button_t { btn_none, btn_release, btn_select, btn_left, btn_down, btn_up, btn_right };

class buttons_t
{
public:
    static void setup()
    {
        adc::setup<128>();
    }

    static button_t read()  // call every 1ms
    {
        static uint8_t count = 0;
        static uint8_t last_state = 0;
        uint8_t this_state = raw_read();

        count = this_state != stable_state && this_state == last_state ? count + 1 : 0;
        last_state = this_state;
        if (count == stable_count)
        {
            stable_state = this_state;
            return static_cast<button_t>(stable_state + 1);
        }
        else
            return btn_none;
    }

private:
    static const uint8_t CHANNEL = A0;
    static const uint8_t stable_count;
    static uint8_t stable_state;

    static uint8_t raw_read()
    {
        static uint16_t limits[] = { 820, 475, 340, 175, 50 };
        static uint8_t n_limits = sizeof(limits) / sizeof(*limits);
        uint16_t x = adc::read<CHANNEL>();

        for (uint8_t i = 0; i < n_limits; ++i)
            if (x > limits[i])
                return i;

        return n_limits;
    }
};

const uint8_t buttons_t::stable_count = 10;
uint8_t buttons_t::stable_state = 0;

