#pragma once

#include "Pins.h"
#include "Delay.h"
#include <stdlib.h>

template<class IFACE>
class tc1602_t
{
public:
    static void setup()
    {
        static uint8_t data[] =
            { 0x3                    // sync
            , 0x3                    // sync
            , 0x3                    // function set
            , 0x2                    // 4-bit mode
            , 0x2                    // function set
            , 0x8                    // two-line mode
            , 0x0                    // items on/off
            , 0x8 | 0x4              // 0x8, display = 0x4 | cursor = 0x2 | blink = 0x1
            , 0x0                    // entry mode
            , 0x0                    // 0x4, right direction = 0x2, shift display = 0x1
            };

        IFACE::setup();

        const uint8_t *p = data;

        delay_ms(15);                // startup delay > 15ms
        send(*p++);
        delay_ms(5);                 // wait time > 4.1ms
        send(*p++);
        delay_us(100);               // wait time > 100us

        while (p < data + sizeof(data) / sizeof(*data))
            send(*p++);

        clear();
    }

    static void write(int x, int radix = 10)
    {
        write(itoa(x, buf, radix));
    }

    static void write(unsigned x, int radix = 10)
    {
        write(utoa(x, buf, radix));
    }

    static void write(long x, int radix = 10)
    {
        write(ltoa(x, buf, radix));
    }

    static void write(unsigned long x, int radix = 10)
    {
        write(ultoa(x, buf, radix));
    }

    static void write(double x, signed char w = 8, unsigned char p = 2)
    {
        write(dtostrf(x, w, p, buf));
    }

    static void write_e(double x, unsigned char p = 2)
    {
        write(dtostre(x, buf, p, 0x01));
    }

    static void write(const char *p)
    {
        uint8_t i = 0;

        while (*p && i++ < 15)
            write_char(*p++);
    }

    static void write_char(char c)
    {
        send(rs | ((c >> 4) & 0xf));
        send(rs | (c & 0xf));
    }

    static void set_pos(uint8_t r, uint8_t c)
    {
        uint8_t w = _BV(7) | (r ? 0x40 : 0) | (c & 0x1f);    // set ddram address + address
        send((w >> 4) & 0xf);
        send(w & 0xf);
    }

    static void cursor(bool on, bool blink)
    {
        send(0x0);                   // items on/off
        send(0x8 | 0x4 | (on ? 0x2 : 0) | (blink ? 0x1 : 0));
    }

    static void clear()
    {
        send(0x0);                   // display clear
        send(0x1);
        delay_us(1700);              // wait time > 1.64ms
    }

private:
    static const uint8_t rs = _BV(4);
    static const uint8_t e  = _BV(5);
    static const uint8_t l  = _BV(6);

    static void send(uint8_t w)
    {
        IFACE::write(w | l | e);
        nop<1>();                    // minimum pulse width 140ns
        IFACE::write((w | l) & ~e);
        delay_us(40);                // min cycle time is min op time is 37us
    }

    static const int buf_size = 33;  // radix 2 on a 32-bit number plus terminator
    static char buf[buf_size];
};

template<class IFACE>
char tc1602_t<IFACE>::buf[tc1602_t<IFACE>::buf_size];

