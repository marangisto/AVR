#define NO_TIMER_VECTORS 1
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <AVR/ADC.h>
#include <AVR/Pins.h>
#include <stdlib.h>

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
    out_0::toggle();
    out_1::toggle();
    out_2::toggle();
    out_3::toggle();
    out_4::toggle();
    out_5::toggle();
    out_6::toggle();
    out_7::toggle();
    delay_ms(1);
}

