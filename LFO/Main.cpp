#define NO_TIMER_VECTORS 1
#include <AVR/Main.h>
#include <AVR/Timer.h>
#include <AVR/Delay.h>
#include <AVR/ADC.h>
#include <AVR/Pins.h>

template <class T> const T& max(const T& a, const T& b) { return (a<b) ? b : a; }
template <class T> const T& min(const T& a, const T& b) { return (a<b) ? a : b; }

typedef timer_t<0> time;
typedef timer_t<1> pwm;

typedef output_t<PA, 2> out_trig;
typedef output_t<PB, 2> out_led;

typedef input_t<PA, 3> in_sync;

static const uint8_t adc_freq = 0;
static const uint8_t adc_pwm = 1;
static const uint8_t adc_spdts = 7;

enum sw_pos_t { sw_err, sw_dn, sw_mid, sw_up };

static uint8_t read_spdts() // return sw0 | (sw1 << 2)
{
    static uint16_t midpoints[] = { 956, 846, 757, 659, 559, 459, 365, 281, 119 };

    uint16_t x = adc::read<adc_spdts>();

    for (uint8_t i = 0; i < sizeof(midpoints) / sizeof(*midpoints); ++i)
        if (x > midpoints[i])
            switch (i)
            {
                case 0: return sw_dn | (sw_dn << 2);
                case 1: return sw_mid | (sw_dn << 2);
                case 2: return sw_dn | (sw_mid << 2);
                case 3: return sw_mid | (sw_mid << 2);
                case 4: return sw_dn | (sw_up << 2);
                case 5: return sw_mid | (sw_up << 2);
                case 6: return sw_up | (sw_dn << 2);
                case 7: return sw_up | (sw_mid << 2);
                case 8: return sw_up | (sw_up << 2);
            }

    return sw_err | (sw_err << 2);
}

/*
static void show(uint16_t x)
{
    out_trig::clear();
    delay_ms(20);
    out_trig::set();
    delay_ms(5);
    out_trig::clear();
    delay_ms(1);
    for (int8_t i = 7; i >= 0; --i)
    {
        out_trig::write(((x >> i) & 0x1) != 0);
        delay_ms(1);
    }
}
*/

enum state_t { reset, tri_up, tri_dn, saw_up, sin_a, sin_b, sin_c, sin_d };

static volatile state_t start = tri_up;
static volatile state_t state = reset;
static volatile uint16_t duty_cycle = 511;
static volatile uint8_t step = 1;

static const uint8_t sin_tab[] =
    { 0,2,3,5,6,8,9,11,13,14,16,17,19,20,22,23,25,27,28,30,31,33,34,36,37,39,41,42,44,45,47,48
    , 50,51,53,54,56,57,59,60,62,63,65,67,68,70,71,73,74,76,77,79,80,81,83,84,86,87,89,90,92,93,95,96
    , 98,99,100,102,103,105,106,108,109,110,112,113,115,116,117,119,120,122,123,124,126,127,128,130,131,132,134,135,136,138,139,140
    , 142,143,144,146,147,148,149,151,152,153,154,156,157,158,159,161,162,163,164,165,167,168,169,170,171,172,174,175,176,177,178,179
    , 180,181,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,208,209,210,211
    , 212,213,214,215,215,216,217,218,219,220,220,221,222,223,223,224,225,226,226,227,228,228,229,230,231,231,232,232,233,234,234,235
    , 236,236,237,237,238,238,239,240,240,241,241,242,242,243,243,244,244,244,245,245,246,246,247,247,247,248,248,248,249,249,249,250
    , 250,250,251,251,251,252,252,252,252,252,253,253,253,253,253,254,254,254,254,254,254,254,255,255,255,255,255,255,255,255,255,255
    };

ISR(TIM0_COMPA_vect)
{
    static bool triggered = false;
    static uint16_t i = 0;
    int16_t y = 0;

    if (state == reset)
    {
        i = 0;
        state = start;
    }

    if (i == 0)
    {
        out_trig::set();
        triggered = true;
    }
    else if (triggered && i >= duty_cycle)
    {
        out_trig::clear();
        triggered = false;
    }

    switch (state)
    {
    case tri_up:
        if (i < 512)
        {
            y = i;
            break;
        }
        else
            state = tri_dn;
        // fall through...
    case tri_dn:
        y = 512 - (i - 511);
        break;
    case saw_up:
        y = i >> 1;
        break;
    case sin_a:
        if (i < 256)
        {
            y = 256 + static_cast<int16_t>(sin_tab[i]);
            break;
        }
        else
            state = sin_b;
        // fall through
    case sin_b:
        if (i < 512)
        {
            y = 256 + static_cast<int16_t>(sin_tab[256-(i-255)]);
            break;
        }
        else
            state = sin_c;
        // fall through
    case sin_c:
        if (i < 768)
        {
            y = 256 - static_cast<int16_t>(sin_tab[i-512]);
            break;
        }
        else
            state = sin_d;
        // fall through
    case sin_d:
        y = 256 - static_cast<int16_t>(sin_tab[256-(i-767)]);
        break;
    default: ;
    }

    pwm::output_compare_register<channel_a>() = 0x1ff - y;

    if ((i = (i + step) & 0x3ff) == 0)
        state = start;
}

void setup()
{
    out_trig::setup();
    out_led::setup();
    in_sync::setup();

    adc::setup<128>();

    pwm::setup<fast_pwm, top_0x1ff>();
    pwm::clock_select<1>();
    pwm::output_pin<channel_a>::setup();
    pwm::compare_output_mode<channel_a, clear_on_compare_match>();

    time::setup<ctc_mode, top_ocra>();
    time::clock_select<8>();
    time::output_compare_register<channel_a>() = 50;
    time::enable_oca();

    sei();
}

void loop()
{
    static sw_pos_t sw0 = sw_err, sw1 = sw_err;
    uint8_t s = read_spdts();
    sw_pos_t _sw0 = static_cast<sw_pos_t>(s & 0x3);
    sw_pos_t _sw1 = static_cast<sw_pos_t>(s >> 2);

    if (_sw0 != sw0)
    {
        sw0 = _sw0;

        switch (sw0)
        {
            case sw_up: time::clock_select<8>(); break;
            case sw_mid: time::clock_select<64>(); break;
            case sw_dn: time::clock_select<256>(); break;
            default: ;
        }
    }

    if (_sw1 != sw1)
    {
        sw1 = _sw1;

        cli();
        switch (sw1)
        {
            case sw_up: start = saw_up; state = reset; break;
            case sw_mid: start = tri_up; state = reset; break;
            case sw_dn: start = sin_a; state = reset; break;
            default: ;
        }
        sei();
    }

    duty_cycle = max<uint16_t>(1, 0x3ff - adc::read<adc_pwm>());

    time::output_compare_register<channel_a>() = 127;
    //time::output_compare_register<channel_a>() = max<uint16_t>(1, adc::read<adc_freq>() >> 2);
 
    delay_ms(10);
}

