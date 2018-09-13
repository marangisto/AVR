#define NO_TIMER_VECTORS 1
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <AVR/ADC.h>
#include <AVR/Pins.h>

template <class T> const T& max(const T& a, const T& b) { return (a<b) ? b : a; }
template <class T> const T& min(const T& a, const T& b) { return (a<b) ? a : b; }

static uint32_t map(uint32_t x, uint32_t in_min, uint32_t in_max, uint32_t out_min, uint32_t out_max)
{
    return (x - in_min) * (out_max - out_min + 1) / (in_max - in_min + 1) + out_min;
}

typedef output_t<PD, 2> led_gate;
typedef output_t<PD, 3> led_trig;
typedef output_t<PD, 4> led_out;
typedef output_t<PD, 7> led_rear;

typedef input_t<PB, 0> gate;
typedef input_t<PB, 2> trig;

typedef input_t<PE, 0, enable_pullup> sw_1a;
typedef input_t<PE, 1, enable_pullup> sw_1b;
typedef input_t<PE, 2, enable_pullup> sw_2a;
typedef input_t<PE, 3, enable_pullup> sw_2b;

typedef timer_t<0> time;
typedef timer_t<1> pwm;
typedef timer_t<2> aux;

static const int adc_a = 0;
static const int adc_h = 1;
static const int adc_d = 2;
static const int adc_s = 3;
static const int adc_r = 4;

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

enum state_t { s_start, s_attack, s_hold, s_decay, s_sustain, s_release, s_stop };

static volatile state_t g_state = s_start;
static volatile uint32_t g_counts[7] = { 0, 0, 0, 0, 0, 0, 0 };
static volatile const uint8_t *g_env = env_lin;
static volatile bool g_gate = false;
static volatile bool g_trig = false;

ISR(TIMER0_COMPA_vect)
{
    static uint8_t i = 0;
    static uint32_t k = 0;
    static uint32_t n = 255;
    static uint8_t v = 0;
    static uint8_t start_v = 0;
    static uint8_t sustain = 0;

    if (++i == 0)
        led_rear::toggle();

    switch (g_state)
    {
    case s_start:
        g_state = s_attack;
        n = g_counts[g_state];
        k = 0;
        start_v = v;
        sustain = g_counts[s_sustain] >> 2;
        break;
    case s_attack:
        if (k > n)
        {
            g_state = g_counts[s_hold] > 0 ? s_hold : s_decay;
            n = g_counts[g_state];
            k = 0;
            start_v = v;
        }
        else
        {
            uint8_t j = k < n ? ((uint32_t) k << 8) / n : 255;

            v = map(g_env[j], 0, 255, start_v, 255);
        }
        break;
    case s_hold:
        if (k > n)
        {
            g_state = s_decay;
            n = g_counts[g_state];
            k = 0;
            start_v = v;
        }
        break;
    case s_decay:
        if (k > n)
        {
            g_state = sustain ? s_sustain : s_stop;
            n = g_counts[g_state];
            k = 0;
            start_v = v;
        }
        else
        {
            uint8_t j = k < n ? ((uint32_t) k << 8) / n : 255;

            v = map(g_env[j], 0, 255, start_v, sustain);
        }
        break;
    case s_sustain:
        if (!g_gate)
        {
            g_state = s_release;
            n = g_counts[g_state];
            k = 0;
            start_v = v;
        }
        break;
    case s_release:
        if (k > n)
        {
            g_state = s_stop;
            v = 0;
        }
        else
        {
            uint8_t j = k < n ? ((uint32_t) k << 8) / n : 255;

            v = map(g_env[j], 0, 255, start_v, 0);
        }
        break;
    default:
        v = 0;
    }

    pwm::output_compare_register<channel_a>() = v;

    ++k;
}

ISR(TIMER2_OVF_vect)
{
    static uint8_t i = 0;

    led_out::write(i < pwm::output_compare_register<channel_a>());
    ++i;
}

ISR(PCINT0_vect)
{
    static bool last_gate = false;

    g_gate = gate::read();

    if (g_gate != last_gate)
    {
        if (g_gate)
            g_state = s_start;
        last_gate = g_gate;
        led_gate::write(g_gate);
    }
}


static const uint8_t time_prescale = 8;
static const uint8_t time_count = 249;
static const float ticks_per_second = F_CPU / (time_prescale * (time_count + 1));
static const int adc_max = 1023;

static uint32_t count(float x, float tmax)
{
    float k = tmax * ticks_per_second / (adc_max * (float) adc_max);

    return k * x * x;
}

void setup()
{
    led_gate::setup();
    led_trig::setup();
    led_out::setup();
    led_rear::setup();
    gate::setup();
    trig::setup();
    adc::setup<128>();

    const wg_mode mode = pwm_phase_correct;
    const int prescale = 1;

    pwm::setup<mode, top_0xff>();
    pwm::clock_select<prescale>();
    pwm::output_pin<channel_a>::setup();
    pwm::compare_output_mode<channel_a, clear_on_compare_match>();

    time::setup<ctc_mode, top_ocra>();
    time::clock_select<time_prescale>();
    time::output_compare_register<channel_a>() = time_count; // 250ms period
    time::enable_oca();

    aux::setup<normal_mode>();
    aux::clock_select<1>();
    aux::enable();

    PCMSK0 |= _BV(PCINT0);
    PCICR |= _BV(PCIE0);

    sei();
}

void loop()
{
    static uint8_t i = 0;
    float tmax = 0;

    if (!sw_1a::read())
        tmax = 1;                   // fast
    else if (!sw_1b::read())
        tmax = 1000;                // slow
    else
        tmax = 20;                  // medium

    if (!sw_2a::read())
        g_env = env_sin;
    else if (!sw_2b::read())
        g_env = env_exp;
    else
        g_env = env_lin;

    led_trig::write(trig::read());

    g_counts[s_attack] = 1 + count(adc::read<adc_a>(), tmax);
    g_counts[s_hold] = count(adc::read<adc_h>(), tmax);
    g_counts[s_decay] = count(adc::read<adc_d>(), tmax);
    g_counts[s_sustain] = adc::read<adc_s>();
    g_counts[s_release] = count(adc::read<adc_r>(), tmax);

    i++;
}

