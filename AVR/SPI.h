#pragma once

#include <avr/io.h>
#include <stdio.h>
#include "Pins.h"

enum spi_shift_order_t { lsb_first = 1, msb_first = 0 };

template<int CLOCK_DIV>
struct spi_clock_traits
{
    static_assert((CLOCK_DIV != 0) && !(CLOCK_DIV & (CLOCK_DIV - 1)), "clock divider must be a power of 2");
    static_assert(CLOCK_DIV > 0 && CLOCK_DIV < 255, "clock divider out of range [1..128]");

    static inline uint8_t spi2x() { return 0; }
    static inline uint8_t spr10() { return 0; }
};

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

typedef output_t<PB, 3> MOSI;
typedef output_t<PB, 5> SCK;

template<int CLOCK_DIV, spi_shift_order_t SHIFT_ORD, class PORT, unsigned BIT>
struct spi_t
{
    typedef output_t<PORT, BIT> CS;
 
    static void setup()
    {
        MOSI::setup();
        SCK::setup();
        CS::setup();

        SPCR = _BV(SPE) | _BV(MSTR) | (SHIFT_ORD<<DORD) | spi_clock_traits<CLOCK_DIV>::spr10();
        SPSR = spi_clock_traits<CLOCK_DIV>::spi2x();
    }

    static void write(uint8_t x)
    {
        CS::clear();
        SPDR = x;
        loop_until_bit_is_set(SPSR, SPIF);
        CS::set();
    }

    static void write(uint16_t x)
    {
        CS::clear();
        SPDR = SHIFT_ORD ? (x & 0xff) : (x >> 8);
        loop_until_bit_is_set(SPSR, SPIF);
        SPDR = SHIFT_ORD ? (x >> 8) : (x & 0xff);
        loop_until_bit_is_set(SPSR, SPIF);
        CS::set();
    }
};

