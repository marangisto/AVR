#pragma once

#include <avr/io.h>
#include <stdio.h>

// hack around the fact that we use g++, not gcc!
#undef FDEV_SETUP_STREAM
#define FDEV_SETUP_STREAM(p, g, f) { 0, 0, f, 0, 0, p, g, 0 }

struct UART
{
    template<uint32_t BAUD>
    static void setup()
    {
        static const uint16_t ubrr = static_cast<uint16_t>(F_CPU / (16. * BAUD) - 1. + 0.5);

        UBRR0H = static_cast<uint8_t>(ubrr >> 8);
        UBRR0L = static_cast<uint8_t>(ubrr & 0xff);

        UCSR0A &= ~_BV(U2X0);                   // 1x
        //UCSR0A |= _BV(U2X0);  // 2x
 
        UCSR0C = _BV(UCSZ01) | _BV(UCSZ00);     // 8-bit data
        UCSR0B = _BV(RXEN0) | _BV(TXEN0);       // enable rx & tx

        stdout = &output;
    }

    static void putChar(char c)
    {
        loop_until_bit_is_set(UCSR0A, UDRE0);
        UDR0 = c;
    }

    static int putCharS(char c, FILE *stream)
    {
        if (c == '\n')
            putChar('\r');
        putChar(c);
        return 0;
    }

    static FILE output;
};

FILE UART::output = FDEV_SETUP_STREAM(UART::putCharS, NULL, _FDEV_SETUP_WRITE);

