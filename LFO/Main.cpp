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

enum waveform_t { wf_tri, wf_saw, wf_sin };

static volatile waveform_t waveform = wf_tri;

enum state_t { reset, tri_up, tri_dn, saw_up };

static volatile state_t start = tri_up;
static volatile state_t state = reset;

ISR(TIM0_COMPA_vect)
{
    static uint16_t i = 0;
    int16_t y = 0;

    if (state == reset)
    {
        i = 0;
        state = start;
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
    default: ;
    }

    pwm::output_compare_register<channel_a>() = 0x1ff - y;

    if ((i = (i + 1) & 0x3ff) == 0)
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

    read_spdts();
    show(adc::read<adc_pwm>());
 
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
            case sw_up: start = tri_up; state = reset; break;
            case sw_mid: start = saw_up; state = reset; break;
            default: ;
        }

        sei();
    }
    //out_led::toggle();
    //out_trig::toggle();
    //show(read_spdts());
    //read_spdts();
    //show(adc::read<adc_freq>());
    //show(adc::read<adc_pwm>());

    //pwm::output_compare_register<channel_a>() = 0x1ff - (adc::read<adc_freq>() >> 1);
    time::output_compare_register<channel_a>() = max<uint16_t>(1, adc::read<adc_freq>() >> 2);
    delay_ms(10);
    //out_trig::write(in_sync::read());
//    delay_ms(100);
}

