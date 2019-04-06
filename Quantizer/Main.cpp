#define NO_TIMER_VECTORS 1
#include <AVR/Main.h>
#include <AVR/ADC.h>
#include <AVR/SPI.h>
#include <AVR/MCP48x2.h>
#include <AVR/SN74HC595.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <AVR/Button.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

template<class T> T max(const T& x, const T& y) { return x > y ? x : y; }

typedef output_t<PC, 3> led;
typedef spi_t<2, msb_first, PB, 2> spi;
typedef output_t<PB, 1> dac;
typedef button_t<PD, 6> btn_dn;
typedef button_t<PD, 7> btn_up;

typedef timer_t<0> debounce;

ISR(TIMER0_OVF_vect)
{
    btn_up::update();
    btn_dn::update();
}

void setup()
{
    led::setup();
    spi::setup();
    dac::setup();
    dac::set();
    btn_up::setup();
    btn_dn::setup();
    debounce::template setup<normal_mode>();
    debounce::template clock_select<64>();
    debounce::enable();
    sei();
}

void loop()
{
    static uint16_t i = 0;

    if (btn_up::read())
        i = (i + 1) & 0x0fff;

    if (btn_dn::read())
        i = (i - 1) & 0x0fff;

    spi::write(mcp48x2_t::encode<chan_a, gain_x2>(i));
    spi::write(mcp48x2_t::encode<chan_b, gain_x2>(i));

    dac::clear();
    dac::set();

    led::toggle();
}

