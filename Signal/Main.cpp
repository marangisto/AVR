#define NO_TIMER_VECTORS 1
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

typedef output_t<PD, 5> led;
typedef output_t<PD, 6> trig;
typedef spi_t<1, msb_first, PB, 2> spi;
typedef output_t<PB, 1> dac;
typedef timer_t<0> blink;
typedef timer_t<1> wave;

ISR(TIMER0_OVF_vect)
//static void blink_isr()
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
    {128,131,134,137,140,143,146,149,152,155,158,162,165,167,170,173
    ,176,179,182,185,188,190,193,196,198,201,203,206,208,211,213,215
    ,218,220,222,224,226,228,230,232,234,235,237,238,240,241,243,244
    ,245,246,248,249,250,250,251,252,253,253,254,254,254,255,255,255
    ,255,255,255,255,254,254,254,253,253,252,251,250,250,249,248,246
    ,245,244,243,241,240,238,237,235,234,232,230,228,226,224,222,220
    ,218,215,213,211,208,206,203,201,198,196,193,190,188,185,182,179
    ,176,173,170,167,165,162,158,155,152,149,146,143,140,137,134,131
    ,128,124,121,118,115,112,109,106,103,100,97,93,90,88,85,82
    ,79,76,73,70,67,65,62,59,57,54,52,49,47,44,42,40
    ,37,35,33,31,29,27,25,23,21,20,18,17,15,14,12,11
    ,10,9,7,6,5,5,4,3,2,2,1,1,1,0,0,0
    ,0,0,0,0,1,1,1,2,2,3,4,5,5,6,7,9
    ,10,11,12,14,15,17,18,20,21,23,25,27,29,31,33,35
    ,37,40,42,44,47,49,52,54,57,59,62,65,67,70,73,76
    ,79,82,85,88,90,93,97,100,103,106,109,112,115,118,121,124
    };

static volatile uint16_t g_count = 1079;
static volatile uint8_t g_stride = 8;

ISR(TIMER1_OVF_vect)
//static void wave_isr()
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
    //blink::isr(blink_isr);
    blink::enable();

    wave::setup<normal_mode>();
    wave::clock_select<1>();
    //wave::isr(wave_isr);
    wave::enable();

    sei();
}

void loop()
{
    uint16_t i = adc::read<0>();
    //uint16_t i = adc::read<5>();
 
#if 1
    // FIXME: do we need to assign these atomically?
    g_stride = 1 << (i / steps_per_octave);
    g_count = counts[i % steps_per_octave];
#else
    g_stride = 1;
    g_count = i;
#endif

    delay_ms(1);
}

