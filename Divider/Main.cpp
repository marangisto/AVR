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

void loop()
{
    static uint32_t clock = 0;
    static bool tick = false;
    static uint8_t i = 0;
    static bool b0 = false;
    static bool b1 = false;
    static bool b2 = false;
    static bool b3 = false;
    static bool b4 = false;
    static bool b5 = false;
    static bool b6 = false;
    static bool b7 = false;

    if (i++)
        tick = true;

    if (tick)
    {
        b0 = !b0;

        if (b0)
        {
            b1 = !b1;
            if (b1)
            {
                b2 = !b2;
                if (b2)
                {
                    b3 = !b3;
                    if (b3)
                    {
                        b4 = !b4;
                        if (b4)
                        {
                            b5 = !b5;
                            if (b5)
                            {
                                b6 = !b6;
                                if (b6)
                                    b7 = !b7;
                            }
                        }
                    }
                }
            }
        }

        out_0::write(b0);
        out_1::write(b1);
        out_2::write(b2);
        out_3::write(b3);
        out_4::write(b4);
        out_5::write(b5);
        out_6::write(b6);
        out_7::write(b7);
    }

    if (tick)
        ++clock;

    tick = false;
    delay_ms(100);
}

