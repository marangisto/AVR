#define NO_TIMER_VECTORS 1
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <AVR/ADC.h>
#include <AVR/Pins.h>

template <class T> const T& max(const T& a, const T& b) { return (a<b) ? b : a; }
template <class T> const T& min(const T& a, const T& b) { return (a<b) ? a : b; }

typedef output_t<PB, 2> out_0;
typedef output_t<PA, 2> out_1;
typedef output_t<PB, 0> out_2;
typedef output_t<PA, 1> out_3;
typedef output_t<PA, 4> out_4;
typedef output_t<PA, 5> out_5;
typedef output_t<PA, 6> out_6;
typedef output_t<PA, 0> out_7;

typedef input_t<PA, 3> in_rst;
typedef input_t<PB, 1> in_clk;

static const uint8_t spdts = 7;

void setup()
{
    out_0::setup();
    out_1::setup();
    out_2::setup();
    out_3::setup();
    out_4::setup();
    out_5::setup();
    out_6::setup();
    out_7::setup();
    in_rst::setup();
    in_clk::setup();
    adc::setup<128>();

    // sei();
}

static bool update(uint8_t& c, uint8_t m)
{
    if (c++ >= m)
    {
        c = 0;
        return true;
    }

    return false;
}

void loop()
{
    static uint32_t clock = 0;
    static bool tick = false;
    static uint8_t i = 0;
    static uint8_t m0 = 1;
    static uint8_t m1 = 2;
    static uint8_t m2 = 3;
    static uint8_t m3 = 4;
    static uint8_t m4 = 5;
    static uint8_t m5 = 6;
    static uint8_t m6 = 6;
    static uint8_t m7 = 7;
    static uint8_t c0 = m0;
    static uint8_t c1 = m1;
    static uint8_t c2 = m2;
    static uint8_t c3 = m3;
    static uint8_t c4 = m4;
    static uint8_t c5 = m5;
    static uint8_t c6 = m6;
    static uint8_t c7 = m7;

    if (!i++)
        tick = true;

    if (tick)
    {
        if (update(c0, m0))
            out_0::toggle();
        if (update(c1, m1))
            out_1::toggle();
        if (update(c2, m2))
            out_2::toggle();
        if (update(c3, m3))
            out_3::toggle();
        if (update(c4, m4))
            out_4::toggle();
        if (update(c5, m5))
            out_5::toggle();
        if (update(c6, m6))
            out_6::toggle();
        if (update(c7, m7))
            out_7::toggle();
    }

    if (tick)
        ++clock;

    tick = false;
    delay_us(250);
}

