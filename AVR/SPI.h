#pragma once

#include <avr/io.h>
#include <stdio.h>
#include "Pins.h"

enum spi_shift_order_t { lsb_first = 1, msb_first = 0 };

template<int CLOCK_DIV>
struct spi_clock_traits
{
    static_assert((CLOCK_DIV != 0) && !(CLOCK_DIV & (CLOCK_DIV - 1)), "clock divider must be a power of 2");
    static_assert(CLOCK_DIV > 1 && CLOCK_DIV < 255, "clock divider out of range [2..128]");

    static inline uint8_t spi2x() { return 0; }
    static inline uint8_t spr10() { return 0; }
};

#if defined(__AVR_ATmega328P__) || defined(__AVR_ATmega328__) || defined(__AVR_ATmega328PB__)
template <>
struct spi_clock_traits<4>
{
    static inline uint8_t spi2x() { return 0; }
    static inline uint8_t spr10() { return 0; }
};
template <>
struct spi_clock_traits<16>
{
    static inline uint8_t spi2x() { return 0; }
    static inline uint8_t spr10() { return _BV(SPR0); }
};
template <>
struct spi_clock_traits<64>
{
    static inline uint8_t spi2x() { return 0; }
    static inline uint8_t spr10() { return _BV(SPR1); }
};
template <>
struct spi_clock_traits<128>
{
    static inline uint8_t spi2x() { return 0; }
    static inline uint8_t spr10() { return _BV(SPR1) | _BV(SPR0); }
};
template <>
struct spi_clock_traits<2>
{
    static inline uint8_t spi2x() { return _BV(SPI2X); }
    static inline uint8_t spr10() { return 0; }
};
template <>
struct spi_clock_traits<8>
{
    static inline uint8_t spi2x() { return _BV(SPI2X); }
    static inline uint8_t spr10() { return _BV(SPR0); }
};
template <>
struct spi_clock_traits<32>
{
    static inline uint8_t spi2x() { return _BV(SPI2X); }
    static inline uint8_t spr10() { return _BV(SPR1); }
};
#endif

#if defined(__AVR_ATtiny84__)
typedef output_t<PA, 5> MOSI;   // DO
typedef output_t<PA, 4> SCK;
#elif defined(__AVR_ATmega328P__) || defined(__AVR_ATmega328__) || defined(__AVR_ATmega328PB__)
typedef output_t<PB, 3> MOSI;
typedef output_t<PB, 5> SCK;
#endif

template<int CLOCK_DIV, spi_shift_order_t SHIFT_ORD, class PORT, unsigned BIT>
struct spi_t
{
    typedef output_t<PORT, BIT> CS;
 
    static void setup()
    {
        MOSI::setup();
        SCK::setup();
        CS::setup();

#if defined(__AVR_ATtiny84__)
        USICR = _BV(USIWM0) | _BV(USICS1) | _BV(USICLK);
#elif defined(__AVR_ATmega328P__) || defined(__AVR_ATmega328__) || defined(__AVR_ATmega328PB__)
        SPCR = _BV(SPE) | _BV(MSTR) | (SHIFT_ORD<<DORD) | spi_clock_traits<CLOCK_DIV>::spr10();
        SPSR = spi_clock_traits<CLOCK_DIV>::spi2x();
#endif
    }

    static void write(uint8_t x)
    {
        CS::clear();
#if defined(__AVR_ATtiny84__)
        USIDR = x;
        USISR = _BV(USIOIF);
        while (!(USISR & _BV(USIOIF)))
            USICR |= _BV(USITC);
#elif defined(__AVR_ATmega328P__) || defined(__AVR_ATmega328__) || defined(__AVR_ATmega328PB__)
        SPDR = x;
        loop_until_bit_is_set(SPSR, SPIF);
#endif
        CS::set();
    }

    static void write(uint16_t x)
    {
        CS::clear();
#if defined(__AVR_ATtiny84__)
        USIDR = SHIFT_ORD ? (x & 0xff) : (x >> 8);
        USISR = _BV(USIOIF);
        while (!(USISR & _BV(USIOIF)))
            USICR |= _BV(USITC);
        USIDR = SHIFT_ORD ? (x >> 8) : (x & 0xff);
        USISR = _BV(USIOIF);
        while (!(USISR & _BV(USIOIF)))
            USICR |= _BV(USITC);
#elif defined(__AVR_ATmega328P__) || defined(__AVR_ATmega328__) || defined(__AVR_ATmega328PB__)
        SPDR = SHIFT_ORD ? (x & 0xff) : (x >> 8);
        loop_until_bit_is_set(SPSR, SPIF);
        SPDR = SHIFT_ORD ? (x >> 8) : (x & 0xff);
        loop_until_bit_is_set(SPSR, SPIF);
#endif
        CS::set();
    }
};

