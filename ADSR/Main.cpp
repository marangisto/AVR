#define NO_TIMER_VECTORS 1
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <AVR/ADC.h>
#include <Arduino/Pins.h>

typedef D13 led;
typedef input_t<PD, 2> gate;
typedef input_t<PD, 4> trig;

typedef timer_t<0> pwm;
typedef timer_t<1> time;

static const int adc_a = 0;
static const int adc_h = 1;
static const int adc_d = 2;
static const int adc_s = 3;
static const int adc_r = 4;

enum shape_t { shape_lin, shape_exp, shape_sin, shape_sqr };

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

static const uint8_t env_sin[] =
    { 0,0,0,0,0,0,0,0,1,1,1,1,1,2,2,2,2,3,3,3,4,4,5,5,5,6,6,7,7,8,9,9
    , 10,10,11,12,12,13,14,14,15,16,17,17,18,19,20,21,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36
    , 37,38,40,41,42,43,44,45,47,48,49,50,52,53,54,55,57,58,59,61,62,63,65,66,67,69,70,72,73,74,76,77
    , 79,80,82,83,85,86,88,89,90,92,93,95,97,98,100,101,103,104,106,107,109,110,112,113,115,117,118,120,121,123,124,126
    , 128,129,131,132,134,135,137,138,140,142,143,145,146,148,149,151,152,154,155,157,158,160,162,163,165,166,168,169,170,172,173,175
    , 176,178,179,181,182,183,185,186,188,189,190,192,193,194,196,197,198,200,201,202,203,205,206,207,208,210,211,212,213,214,215,217
    , 218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,234,235,236,237,238,238,239,240,241,241,242,243,243,244,245
    , 245,246,246,247,248,248,249,249,250,250,250,251,251,252,252,252,253,253,253,253,254,254,254,254,254,255,255,255,255,255,255,255
    };

static const uint8_t env_sqr[] =
    { 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
    , 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
    , 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
    , 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255
    , 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    , 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    , 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    , 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    };

enum state_t { s_start, s_attack, s_hold, s_decay, s_sustain, s_release, s_stop };

static volatile state_t g_state = s_start;
static volatile uint16_t g_counts[7] = { 0, 0, 0, 0, 0, 0, 0 };
static volatile uint8_t g_stride = 1;
static volatile uint8_t g_mask = 0;
static volatile shape_t g_shape = shape_sin;
static volatile const uint8_t *g_env = env_lin;
static volatile bool g_gate = false;
static volatile bool g_trig = false;
static volatile uint8_t g_sustain = 0;

ISR(TIMER1_OVF_vect)
{
    static uint16_t count = 1079;
    static uint8_t stride = 0;
    static uint8_t mask = 0;
    static uint8_t i = 0, k = 0;
    static uint8_t decay_floor = 0;

    time::counter() = 65536 - count;                // timing correct on next cycle

    if (g_state == s_start)
    {
        i = 0;
    }

    if (i == 0)
    {
        switch (g_state)
        {
        case s_start:
            g_state = s_attack;
            break;
        case s_attack:
            g_state = s_hold;
            break;
        case s_hold:
            g_state = s_decay;
            break;
        case s_decay:
            if (g_gate)
            {
                g_state = s_sustain;
                decay_floor = g_sustain;
            }
            else
            {
                g_state = s_stop;
                decay_floor = 0;
            }
            break;
        case s_sustain:
            g_state = s_release;
            break;
        case s_release:
            g_state = s_stop;
            break;
        default:
            return; // ignore all unhandled states
        }
        count = g_counts[g_state];
        stride = g_stride;
        mask = g_mask;
    }

    switch (g_state)
    {
    case s_attack:
        pwm::output_compare_register<channel_a>() = g_env[i];
        break;
    case s_decay: ;
        pwm::output_compare_register<channel_a>() = decay_floor + (((255 - decay_floor) * (255 - g_env[i])) >> 8);
        break;
    case s_release:
        pwm::output_compare_register<channel_a>() = ((g_sustain * (255 - g_env[i])) >> 8);
        break;
    case s_sustain:
        if (i == 0)
            pwm::output_compare_register<channel_a>() = g_sustain;
        i = g_gate ? 1 : 255;   // gate down -> force overflow
        break;
    default:
            ; // nothing to do
    }

    if ((k & mask) == 0)
        i += stride;
    ++k;
}


ISR(PCINT2_vect)
{
    static bool last_gate = false;

    g_gate = gate::read();

    if (g_gate != last_gate)
    {
        if (g_gate)
            g_state = s_start;
        last_gate = g_gate;
        led::write(g_gate);
    }
}


void setup()
{
    led::setup();
    gate::setup();
    trig::setup();
    adc::setup<128>();

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

    led::clear();

    PCMSK2 |= _BV(PCINT18);
    PCICR |= _BV(PCIE2);
    sei();
}

void loop()
{
    g_stride = 1;
    g_mask = 0;

    // 200-500 is lowest practically reliable count for exponential envelope, for square we can go to 50 for really fast edge

    g_counts[s_sustain] = 100;  // not really used

    g_counts[s_attack] = 100 + (adc::read<adc_a>() << 4);
    g_counts[s_hold] = 100 + (adc::read<adc_h>() << 4);
    g_counts[s_decay] = 100 + (adc::read<adc_d>() << 4);
    g_sustain = adc::read<adc_s>() >> 2;
    g_counts[s_release] = 100 + (adc::read<adc_r>() << 4);

    delay_ms(1);
}

