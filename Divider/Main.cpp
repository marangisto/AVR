#define NO_TIMER_VECTORS 1
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <AVR/ADC.h>
#include <AVR/Pins.h>

template <class T> const T& max(const T& a, const T& b) { return (a<b) ? b : a; }
template <class T> const T& min(const T& a, const T& b) { return (a<b) ? a : b; }

typedef output_t<PB, 2> out_0;
typedef output_t<PA, 2> out_4;
typedef output_t<PB, 0> out_1;
typedef output_t<PA, 1> out_5;
typedef output_t<PA, 4> out_2;
typedef output_t<PA, 5> out_6;
typedef output_t<PA, 6> out_3;
typedef output_t<PA, 0> out_7;

typedef outputs_t<out_7, out_3, out_6, out_2, out_5, out_1, out_4, out_0> output;

typedef input_t<PA, 3> in_rst;  // PCINT3
typedef input_t<PB, 1> in_clk;  // PCINT9

static const uint8_t spdts = 7;

static volatile bool clk_rising = false;
static volatile bool clk_falling = false;

ISR(PCINT0_vect)
{
    if (!in_rst::read())    // inverted input
        ; // FIXME: perform reset here
}

ISR(PCINT1_vect)
{
    if (!in_clk::read())    // inverted input
        clk_rising = true;
    else
        clk_falling = true;
}

static uint8_t read_spdts()     // 1-9 is valid state, 0 is an error condition
{
    static uint16_t midpoints[] = { 956, 846, 757, 659, 559, 459, 365, 281, 119 };

    uint16_t x = adc::read<spdts>();

    for (uint8_t i = 0; i < sizeof(midpoints) / sizeof(*midpoints); ++i)
        if (x > midpoints[i])
            return i + 1;

    return 0;
}

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

    output::setup();

    in_rst::setup();
    in_clk::setup();
    adc::setup<128>();

    PCMSK0 |= _BV(PCINT3);  // enable pin-change interrupt on rst (PCINT3)
    GIMSK |= _BV(PCIE0);    // enable channel 0 pin-change interrupts

    PCMSK1 |= _BV(PCINT9);  // enable pin-change interrupt on clk (PCINT9)
    GIMSK |= _BV(PCIE1);    // enable channel 1 pin-change interrupts

    sei();
}

/*
static bool update(uint8_t& c, uint8_t m)
{
    if (c++ >= m)
    {
        c = 0;
        return true;
    }

    return false;
}
*/

void loop()
{
    /*
    static uint32_t clock = 0;
    static bool tick = false;
    static bool b0 = false;
    static bool b1 = false;
    static bool b2 = false;
    static bool b3 = false;
    static bool b4 = false;
    static bool b5 = false;
    static bool b6 = false;
    static bool b7 = false;
    static uint8_t m1 = 2;
    static uint8_t m2 = 4;
    static uint8_t m3 = 8;
    static uint8_t m4 = 16;
    static uint8_t m5 = 32;
    static uint8_t m6 = 64;
    static uint8_t m7 = 128;
    static uint8_t c1 = m1;
    static uint8_t c2 = m2;
    static uint8_t c3 = m3;
    static uint8_t c4 = m4;
    static uint8_t c5 = m5;
    static uint8_t c6 = m6;
    static uint8_t c7 = m7;

    if (!i++)               // this should be the clock interrupt
    {
        tick = true;
        b0 = !b0;
    }

    if (tick)
    {
        b1 = update(c1, m1);
        b2 = update(c2, m2);
        b3 = update(c3, m3);
        b4 = update(c4, m4);
        b5 = update(c5, m5);
        b6 = update(c6, m6);
        b7 = update(c7, m7);
    }

    out_0::write(b0);
    out_1::write(b1);
    out_2::write(b2);
    out_3::write(b3);
    out_4::write(b4);
    out_5::write(b5);
    out_6::write(b6);
    out_7::write(b7);

    if (tick)
        ++clock;

    */
    /*
    static uint8_t i = 0;

    if (clk_rising)
    {
        out_0::set();
        clk_rising = false;
        i = (i + 1) & 0x7;
        switch (i)
        {
            case 1: out_1::set(); break;
            case 2: out_2::set(); break;
            case 3: out_3::set(); break;
            case 4: out_4::set(); break;
            case 5: out_5::set(); break;
            case 6: out_6::set(); break;
            case 7: out_7::set(); break;
        }
    }
    else if (clk_falling)
    {
        out_0::clear();
        clk_falling = false;
        out_1::clear();
        out_2::clear();
        out_3::clear();
        out_4::clear();
        out_5::clear();
        out_6::clear();
        out_7::clear();
    }
    */
    output::write(read_spdts());
    output::write(255);
}

