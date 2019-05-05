#pragma once

#include "Pins.h"

enum shift_order_t { LSB_FIRST = 0, MSB_FIRST = 1};

template<class DT, class CK>
static inline void clock_out_bit(uint8_t x, int i)
{
    CK::clear();
    DT::write(((x >> i) & 0x01) != 0);
    CK::set();
}

template<class DT, class CK>
static inline void clock_out_bit16(uint16_t x, int i)
{
    CK::clear();
    DT::write(((x >> i) & 0x01) != 0);
    CK::set();
}

template<class DT, class CK, shift_order_t SD>
struct shift_out_impl
{
    static void shift_out(uint8_t x)
    {
        for (int8_t i = 7; i >= 0; --i)
            clock_out_bit<DT, CK>(x, i);
    }

    static void shift_out16(uint16_t x)
    {
        for (int8_t i = 15; i >= 0; --i)
            clock_out_bit16<DT, CK>(x, i);
    }
};

template<class DT, class CK>
struct shift_out_impl<DT, CK, LSB_FIRST>
{
    static void shift_out(uint8_t x)
    {
        for (int8_t i = 0; i < 8; ++i)
            clock_out_bit<DT, CK>(x, i);
    }

    static void shift_out16(uint16_t x)
    {
        for (int8_t i = 0; i < 16; ++i)
            clock_out_bit16<DT, CK>(x, i);
    }
};

template<class DT, class CK, shift_order_t SD>
void shift_out(uint8_t x)
{
    shift_out_impl<DT, CK, SD>::shift_out(x);
}

template<class DT, class CK, shift_order_t SD>
void shift_out16(uint16_t x)
{
    shift_out_impl<DT, CK, SD>::shift_out16(x);
}

template<class DT, class CK, class LT, shift_order_t SD>
struct sn74hc595_t
{
    static void setup()
    {
        DT::setup();
        CK::setup();
        LT::setup();
    }

    static void write(uint8_t x)
    {
        LT::clear();
        shift_out<DT, CK, SD>(x);
        LT::set();
    }

    static void write(uint16_t x)
    {
        LT::clear();
        shift_out16<DT, CK, SD>(x);
        LT::set();
    }
};

