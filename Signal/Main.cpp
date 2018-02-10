#include <AVR/Main.h>
#include <AVR/ADC.h>
#include <AVR/SPI.h>
#include <AVR/MCP48x2.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

template<class T> T max(const T& x, const T& y) { return x > y ? x : y; }

typedef output_t<PB, 2> led;
//typedef output_t<PC, 4> led;
typedef timer_t<0> blink;
typedef timer_t<1> wave;
typedef spi_t<1, msb_first, PA, 6> spi;
typedef output_t<PA, 7> dac;

static void blink_isr()
{
    static uint8_t i = 0;

    if (++i == 0)
        led::toggle();
}

static uint8_t sine[] =
    {128,131,134,137,140,143,146,149,152,156,159,162,165,168,171,174
    ,176,179,182,185,188,191,193,196,199,201,204,206,209,211,213,216
    ,218,220,222,224,226,228,230,232,234,235,237,239,240,242,243,244
    ,246,247,248,249,250,251,251,252,253,253,254,254,254,255,255
    ,255,255,255,255,255,254,254,253,253,252,252,251,250,249,248
    ,247,246,245,244,242,241,239,238,236,235,233,231,229,227,225
    ,223,221,219,217,215,212,210,207,205,202,200,197,195,192,189
    ,186,184,181,178,175,172,169,166,163,160,157,154,151,148,145
    ,142,138,135,132,129,126,123,120,117,113,110,107,104,101,98
    ,95,92,89,86,83,80,77,74,71,69,66,63,60,58,55
    ,53,50,48,45,43,40,38,36,34,32,30,28,26,24,22
    ,20,19,17,16,14,13,11,10,9,8,7,6,5,4,3,3
    ,2,2,1,1,0,0,0,0,0,0,0,1,1,1,2
    ,2,3,4,4,5,6,7,8,9,11,12,13,15,16,18,20
    ,21,23,25,27,29,31,33,35,37,39,42,44,46,49,51
    ,54,56,59,62,64,67,70,73,76,79,81,84,87,90,93
    ,96,99,103,106,109,112,115,118,121,124
    };

static volatile uint16_t period = 500;

static void wave_isr()
{
    static uint8_t i = 0;

    wave::counter() = 65536 - period;
    i += 8;
    spi::write(mcp48x2_t::encode<chan_a>(i));
    spi::write(mcp48x2_t::encode<chan_b>(sine[i]));
    dac::clear();
    dac::set();
}

void setup()
{
    led::setup();
    adc::setup<128>();
    spi::setup();
    dac::setup();
    dac::set();
 
    blink::setup<normal_mode>();
    blink::clock_select<256>();
    blink::isr(blink_isr);
    blink::enable();

    wave::setup<normal_mode>();
    wave::clock_select<1>();
    wave::isr(wave_isr);
    wave::enable();

    sei();
}

void loop()
{
    static uint16_t i = 0;

    period = 2 * max<uint16_t>(adc::read<0>(), 1);
    printf("%d\n", period);

    ++i;
    delay_ms(1);
}

