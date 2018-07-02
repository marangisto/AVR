#define NO_TIMER_VECTORS 1
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <Arduino/Pins.h>

typedef D13 led;
typedef timer_t<0> pwm;
typedef timer_t<1> time;

enum shape_t { shape_sin, shape_tri, shape_saw, shape_sqr };

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

static volatile uint16_t g_count = 1079;
static volatile uint8_t g_stride = 1;
static volatile uint8_t g_mask = 0;
static volatile shape_t g_shape = shape_sin;
static volatile const uint8_t *g_wave = wave_sin;

ISR(TIMER1_OVF_vect)
{
    static uint16_t count = 1079;
    static uint8_t stride = 0;
    static uint8_t mask = 0;
    static uint8_t i = 0, k = 0;

    time::counter() = 65536 - count;                // timing correct on next cycle

    if (i == 0)                                     // we only update on new cycle
    {
        count = g_count;
        stride = g_stride;
        mask = g_mask;
        //trig::set();
    }
    //else if (i == 128)
        //trig::clear();

    pwm::output_compare_register<channel_a>() = g_wave[i];

    if ((k & mask) == 0)
        i += stride;
    ++k;
}

void setup()
{
    led::setup();
    const wg_mode mode = pwm_phase_correct;
    //const wg_mode mode = fast_pwm;
    const int prescale = 1;

    pwm::setup<mode, top_0xff>();
    pwm::clock_select<prescale>();
    pwm::output_pin<channel_a>::setup();
    pwm::output_pin<channel_b>::setup();
    pwm::compare_output_mode<channel_a, clear_on_compare_match>();

    time::setup<normal_mode>();
    time::clock_select<1>();
    time::enable();

    sei();
}

void loop()
{
    static const int dcs[] = { 255, 0};
    static unsigned i = 0;
    static bool dir = false;            // scan direction

    pwm::output_compare_register<channel_a>() = dcs[i];

    if (i == 0 || (i + 1) == sizeof(dcs) / sizeof(*dcs))
        dir = !dir;
    
    i += dir ? 1 : -1;

    led::toggle();

    g_stride = 1;
    g_mask = 0;
    g_count = 1000;

    delay_ms(100);
    //delay_us(500);
}

