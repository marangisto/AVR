#define NO_TIMER_VECTORS 1
#include <AVR/Main.h>
#include <AVR/ADC.h>
#include <AVR/SPI.h>
#include <AVR/MCP48x2.h>
#include <AVR/SN74HC595.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <AVR/Buttons.h>
#include <AVR/UART.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

template<class T> T max(const T& x, const T& y) { return x > y ? x : y; }

typedef output_t<PD, 5> led;
typedef output_t<PD, 6> trig;
typedef spi_t<2, msb_first, PB, 2> spi;
typedef output_t<PB, 1> dac;
typedef timer_t<0> blink;
typedef timer_t<1> wave;

typedef output_t<PD, 3> DATA;
typedef output_t<PD, 4> CLOCK;
typedef output_t<PB, 0> LATCH;

typedef sn74hc595_t<DATA, CLOCK, LATCH, MSB_FIRST> display; // 16 leds

static const int adc_cv = 0;
static const int adc_phase = 1;
static const int adc_pwm = 2;
static const int adc_auxa = 4;
static const int adc_auxb = 3;
static const int adc_btns = 5;

typedef buttons_t<adc_btns> buttons;

static const uint8_t btn_shape_a = 4;
static const uint8_t btn_shape_b = 2;
static const uint8_t btn_octave_a = 5;
static const uint8_t btn_octave_b = 3;

enum shape_t { shape_sin, shape_tri, shape_saw, shape_sqr };

static inline void increment_shape(volatile shape_t& x)
{
    if (x < shape_sqr)
        x = static_cast<shape_t>(static_cast<int>(x) + 1);
    else
        x = shape_sin;
}

struct display_t
{
    unsigned int shape_a : 4;
    unsigned int octave_a : 5;
    unsigned int octave_b : 3;
    unsigned int shape_b : 4;
};

ISR(TIMER0_OVF_vect)
{
    static uint8_t i = 0;

    if (++i == 0)
        led::toggle();
}

static const uint16_t steps_per_octave = 12 * 8;
static const uint8_t max_octave_downshifts = 4;

// we have max(cv) = 1024 / steps_per_octave < 12 possible octaves so we allocate this many
// strides and masks even though some of these won't produce sound

static const uint8_t strides[] = {    1,    1,    1,    1,    1,    1,    1,   1,   1,   2,   4,    8,   16,   32,  64,   0,   0,   0,   0 };
static const uint8_t masks[] =   { 0x7f, 0x3f, 0x1f, 0x0f, 0x07, 0x03, 0x01, 0x0, 0x0, 0x0, 0x0,  0x0,  0x0,  0x0, 0x0, 0x0, 0x0, 0x0, 0x0 };

static const uint16_t counts[] =
    { 658,653,648,643,638,633,628,623,618,613,608,604,599,594,590,585
    , 580,576,571,567,562,558,553,549,544,540,536,532,527,523,519,515
    , 511,507,503,499,495,491,487,483,479,475,471,467,464,460,456,452
    , 449,445,441,438,434,431,427,424,420,417,413,410,407,403,400,397
    , 393,390,387,384,381,377,374,371,368,365,362,359,356,353,350,347
    , 344,341,339,336,333,330,327,324,322,319,316,314,311,308,306,303
    };

static const uint8_t wave_sin[] =
    { 128,131,134,137,140,143,146,149,152,155,158,162,165,167,170,173,176,179,182,185,188,190,193,196,198,201,203,206,208,211,213,215
    , 218,220,222,224,226,228,230,232,234,235,237,238,240,241,243,244,245,246,248,249,250,250,251,252,253,253,254,254,254,255,255,255
    , 255,255,255,255,254,254,254,253,253,252,251,250,250,249,248,246,245,244,243,241,240,238,237,235,234,232,230,228,226,224,222,220
    , 218,215,213,211,208,206,203,201,198,196,193,190,188,185,182,179,176,173,170,167,165,162,158,155,152,149,146,143,140,137,134,131
    , 128,124,121,118,115,112,109,106,103,100,97,93,90,88,85,82,79,76,73,70,67,65,62,59,57,54,52,49,47,44,42,40
    , 37,35,33,31,29,27,25,23,21,20,18,17,15,14,12,11,10,9,7,6,5,5,4,3,2,2,1,1,1,0,0,0
    , 0,0,0,0,1,1,1,2,2,3,4,5,5,6,7,9,10,11,12,14,15,17,18,20,21,23,25,27,29,31,33,35
    , 37,40,42,44,47,49,52,54,57,59,62,65,67,70,73,76,79,82,85,88,90,93,97,100,103,106,109,112,115,118,121,124
    };

static const uint8_t wave_tri[] =
    { 128,129,131,133,135,137,139,141,143,145,147,149,151,153,155,157,159,161,163,165,167,169,171,173,175,177,179,181,183,185,187,189
    , 191,193,195,197,199,201,203,205,207,209,211,213,215,217,219,221,223,225,227,229,231,233,235,237,239,241,243,245,247,249,251,253
    , 255,253,251,249,247,245,243,241,239,237,235,233,231,229,227,225,223,221,219,217,215,213,211,209,207,205,203,201,199,197,195,193
    , 191,189,187,185,183,181,179,177,175,173,171,169,167,165,163,161,159,157,155,153,151,149,147,145,143,141,139,137,135,133,131,129
    , 128,126,124,122,120,118,116,114,112,110,108,106,104,102,100,98,96,94,92,90,88,86,84,82,80,78,76,74,72,70,68,66
    , 64,62,60,58,56,54,52,50,48,46,44,42,40,38,36,34,32,30,28,26,24,22,20,18,16,14,12,10,8,6,4,2
    , 0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62
    , 64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100,102,104,106,108,110,112,114,116,118,120,122,124,126
    };

static const uint8_t wave_saw[] =
    { 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31
    , 32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63
    , 64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95
    , 96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127
    , 128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159
    , 160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191
    , 192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223
    , 224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255
    };

static const uint8_t wave_sqr[] =
    { 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
    , 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
    , 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
    , 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
    , 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    , 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    , 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    , 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    };

static inline void assign_wave(volatile const uint8_t*& wave, shape_t shape)
{
    switch (shape)
    {
        case shape_sin: wave = wave_sin; break;
        case shape_tri: wave = wave_tri; break;
        case shape_saw: wave = wave_saw; break;
        case shape_sqr: wave = wave_sqr; break;
    }
}

static volatile uint16_t g_count = 1079;
static volatile uint8_t g_stride_a = 1;
static volatile uint8_t g_stride_b = 1;
static volatile uint8_t g_mask_a = 0;
static volatile uint8_t g_mask_b = 0;

static volatile shape_t g_shape_a = shape_sin;
static volatile shape_t g_shape_b = shape_sin;
static volatile uint8_t g_octave_a = 2; // ranges 0..4 octave shift with 2 = centre
static volatile uint8_t g_octave_b = 2; // ranges 0..2 octave shift down from octave a with 2 == no shift

static volatile const uint8_t *g_wave_a = wave_sin;
static volatile const uint8_t *g_wave_b = wave_sin;
static volatile uint8_t g_phase_b = 0;

ISR(TIMER1_OVF_vect)
{
    static uint16_t count = 1079;
    static uint8_t stride_a = 0;
    static uint8_t stride_b = 0;
    static uint8_t mask_a = 0;
    static uint8_t mask_b = 0;
    static uint8_t i = 0, j = 0, k = 0;

    wave::counter() = 65536 - count;                // timing correct on next cycle

    if (j == 0)                                     // we only update on new cycle
    {
        count = g_count;
        stride_a = g_stride_a;
        stride_b = g_stride_b;
        mask_a = g_mask_a;
        mask_b = g_mask_b;
        trig::set();
    }
    else if (j == 128)
        trig::clear();

    spi::write(mcp48x2_t::encode<chan_a>(g_wave_a[i]));
    spi::write(mcp48x2_t::encode<chan_b>(g_wave_b[(j + g_phase_b) & 0xff]));

    dac::clear();
    dac::set();

    if ((k & mask_a) == 0)
        i += stride_a;
    if ((k & mask_b) == 0)
        j += stride_b;
    ++k;
}

void setup()
{
    led::setup();
    display::setup();
    buttons::setup();
    trig::setup();
    adc::setup<128>();
    spi::setup();
    dac::setup();
    dac::set();

    blink::setup<normal_mode>();
    blink::clock_select<256>();
    blink::enable();

    wave::setup<normal_mode>();
    wave::clock_select<1>();
    wave::enable();

    UART::setup<9600>();

    sei();

    printf("Hybrid VCO 1.0\n");
}

void loop()
{
    static bool init = true;
    static bool use_adc_cv = true;
    static union { display_t d; uint16_t i; } display_data, last_display;
    bool got_input = false;
    uint8_t x = buttons::read();

    switch (x & buttons::mask)
    {
        case btn_shape_a: increment_shape(g_shape_a); assign_wave(g_wave_a, g_shape_a); break;
        case btn_shape_b:increment_shape(g_shape_b); assign_wave(g_wave_b, g_shape_b); break;
        case btn_octave_a: g_octave_a = g_octave_a < 4 ? g_octave_a + 1 : 0; break;
        case btn_octave_b: g_octave_b = g_octave_b > 0 ? g_octave_b - 1 : 2; break;
        default: ;
    }

    if (x & buttons::mask || init)
    {
        display_data.d.shape_a = 1 << g_shape_a;
        display_data.d.shape_b = 1 << g_shape_b;
        display_data.d.octave_a = 1 << g_octave_a;
        display_data.d.octave_b = 1 << g_octave_b;
        if (display_data.i != last_display.i)
        {
            display::write(display_data.i);
            got_input = true;
        }
        last_display = display_data;
    }

    static uint16_t cv = 512;

    if (use_adc_cv)
        cv = 1023 - adc::read<adc_cv>();        // input is inverted

    g_phase_b = adc::read<adc_phase>() >> 2;    // [0..1023] -> [0..255]

    static char buf[80] = "";

    if (!init && UART::hasChar() && fgets(buf, sizeof(buf), stdin))
    {
        cv = atoi(buf);
        printf("%d\n", cv);
        got_input = true;
        use_adc_cv = false;                     // manual mode from now on
    }

#if 1
    uint8_t index = cv / steps_per_octave + max_octave_downshifts;
    uint8_t index_a = index + g_octave_a - 2;
    uint8_t index_b = index_a - 2 + g_octave_b;
    g_stride_a = strides[index_a];
    g_mask_a = masks[index_a];
    g_stride_b = strides[index_b];
    g_mask_b = masks[index_b];
    g_count = counts[cv % steps_per_octave];

    if (got_input || init)
    {
        printf("cv = %d, index = (%d, %d, %d), ", cv, index, index_a, index_b);
        printf("count = %d, ", g_count);
        printf("a = (%d, %d), ", g_stride_a, g_mask_a);
        printf("b = (%d, %d)\n", g_stride_b, g_mask_b);
        printf("cv> ");
    }
#else
    g_stride_a = g_stride_b = 1;
    g_mask_a = g_mask_b = 0;
    g_count = cv;

    if (got_input || init)
    {
        printf("cv = %d, ", cv);
        printf("count = %d, ", g_count);
        printf("a = (%d, %d), ", g_stride_a, g_mask_a);
        printf("b = (%d, %d)\n", g_stride_b, g_mask_b);
        printf("cv> ");
    }
#endif

    init = false;
    delay_ms(1);
}

