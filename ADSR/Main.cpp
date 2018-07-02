#define NO_TIMER_VECTORS 1
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <Arduino/Pins.h>

typedef D13 led;
typedef timer_t<0> pwm;
typedef timer_t<1> time;

enum shape_t { shape_lin, shape_exp };

static const uint8_t env_lin[] =
    { 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31
    , 32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63
    , 64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95
    , 96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127
    , 128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159
    , 160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191
    , 192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223
    , 224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255
    };

static const uint8_t env_exp[] =
    { 0,5,10,15,19,24,28,33,37,41,46,50,54,58,61,65,69,73,76,80,83,86,90,93,96,99,102,105,108,111,114,117
    , 119,122,125,127,130,132,135,137,139,141,144,146,148,150,152,154,156,158,160,162,164,166,167,169,171,172,174,176,177,179,180,182
    , 183,185,186,187,189,190,191,193,194,195,196,197,199,200,201,202,203,204,205,206,207,208,209,210,211,212,212,213,214,215,216,217
    , 217,218,219,220,220,221,222,222,223,224,224,225,226,226,227,227,228,229,229,230,230,231,231,232,232,233,233,234,234,234,235,235
    , 236,236,236,237,237,238,238,238,239,239,239,240,240,240,241,241,241,242,242,242,243,243,243,243,244,244,244,244,245,245,245,245
    , 245,246,246,246,246,247,247,247,247,247,247,248,248,248,248,248,249,249,249,249,249,249,249,250,250,250,250,250,250,250,250,251
    , 251,251,251,251,251,251,251,251,252,252,252,252,252,252,252,252,252,252,253,253,253,253,253,253,253,253,253,253,253,253,253,253
    , 254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,254,255,255,255,255,255,255,255,255,255,255,255,255,255
    };

static volatile uint16_t g_count = 1079;
static volatile uint8_t g_stride = 1;
static volatile uint8_t g_mask = 0;
static volatile shape_t g_shape = shape_exp;
static volatile const uint8_t *g_env = env_exp;

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

    pwm::output_compare_register<channel_a>() = g_env[i];

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
    g_count = 2000;

    delay_ms(100);
    //delay_us(500);
}

