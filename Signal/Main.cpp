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
typedef output_t<PA, 3> trig;
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

static const uint16_t steps_per_octave = 12 * 8;

static const uint16_t counts[] =
    {1432,1422,1412,1402,1391,1381,1372,1362,1352,1342,1332,1323,1313,1304
    ,1295,1285,1276,1267,1258,1249,1240,1231,1222,1213,1204,1196,1187,1179
    ,1170,1162,1153,1145,1137,1129,1121,1113,1105,1097,1089,1081,1073,1065
    ,1058,1050,1043,1035,1028,1020,1013,1006,998,991,984,977,970,963,956
    ,949,942,936,929,922,916,909,902,896,890,883,877,870,864,858,852,846
    ,840,834,828,822,816,810,804,798,793,787,781,776,770,764,759,753,748
    ,743,737,732,727,722
    };

static const uint8_t sine[] =
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

static volatile uint16_t g_count = 1079;
static volatile uint8_t g_stride = 8;

static void wave_isr()
{
    static const uint16_t call_overhead = 58;       // tune this to isr call overhead
    static uint16_t count = 1079;
    static uint8_t stride = 8;
    static uint8_t i = 0;

    if (i == 0)                                     // we only update on new cycle
    {
        count = g_count - call_overhead;
        stride = g_stride;
    }

    wave::counter() = 65536 - count;

    if (i == 0)
    {
        trig::clear();
        trig::set();
    }

    spi::write(mcp48x2_t::encode<chan_a>(i));
    spi::write(mcp48x2_t::encode<chan_b>(sine[i]));
    dac::clear();
    dac::set();
    i += stride;
}

void setup()
{
    led::setup();
    trig::setup();
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
    uint16_t i = adc::read<0>();
 
    // FIXME: do we need to assign these atomically?
    g_stride = 1 << (i / steps_per_octave);
    g_count = counts[i % steps_per_octave];

    delay_ms(1);
}

