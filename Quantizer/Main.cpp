#define NO_TIMER_VECTORS 1
#include <AVR/Main.h>
#include <AVR/ADC.h>
#include <AVR/SPI.h>
#include <AVR/MCP48x2.h>
#include <AVR/SN74HC595.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <AVR/Buttons.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

template<class T> T max(const T& x, const T& y) { return x > y ? x : y; }

typedef output_t<PC, 3> led;
typedef spi_t<2, msb_first, PB, 2> spi;
typedef output_t<PB, 1> dac;

void setup()
{
    led::setup();
    spi::setup();
    dac::setup();
    dac::set();
}

void loop()
{
    static uint16_t i = 0;

    spi::write(mcp48x2_t::encode<chan_a, gain_x2>(i));
    spi::write(mcp48x2_t::encode<chan_b, gain_x2>(4096 - i++));

    dac::clear();
    dac::set();

    led::toggle();
}

