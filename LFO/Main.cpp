#define NO_TIMER_VECTORS 1
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <AVR/ADC.h>
#include <AVR/Pins.h>

template <class T> const T& max(const T& a, const T& b) { return (a<b) ? b : a; }
template <class T> const T& min(const T& a, const T& b) { return (a<b) ? a : b; }

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

void setup()
{
    out_trig::setup();
    out_led::setup();
    in_sync::setup();

    adc::setup<128>();

    // sei();
    read_spdts();
}

void loop()
{
    //out_led::toggle();
    //out_trig::toggle();
    //show(read_spdts());
    //read_spdts();
    //show(adc::read<adc_freq>());
    show(adc::read<adc_pwm>());
    //out_trig::write(in_sync::read());
//    delay_ms(100);
}

