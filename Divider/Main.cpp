#define NO_TIMER_VECTORS 1
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <AVR/ADC.h>
#include <AVR/Pins.h>

template <class T> const T& max(const T& a, const T& b) { return (a<b) ? b : a; }
template <class T> const T& min(const T& a, const T& b) { return (a<b) ? a : b; }

typedef outputs_t
    < output_t<PB, 2>
    , output_t<PB, 0>
    , output_t<PA, 4>
    , output_t<PA, 6>
    , output_t<PA, 2>
    , output_t<PA, 1>
    , output_t<PA, 5>
    , output_t<PA, 0>
    > output;

typedef input_t<PA, 3> in_rst;  // PCINT3
typedef input_t<PB, 1> in_clk;  // PCINT9

static const uint8_t spdts = 7;

static volatile uint16_t i = 0;

ISR(PCINT0_vect)
{
    if (!in_rst::read())    // inverted input
        ; // FIXME: perform reset here
}

ISR(PCINT1_vect)
{
    static uint8_t bits = 0;

    // if (!in_clk::read())    // inverted input
 
    if (i % 2 == 0)
        bits ^= _BV(0);
    if (i % 3 == 0)
        bits ^= _BV(1);
    if (i % 4 == 0)
        bits ^= _BV(2);
    if (i % 5 == 0)
        bits ^= _BV(3);
    if (i % 6 == 0)
        bits ^= _BV(4);
    if (i % 7 == 0)
        bits ^= _BV(5);
    if (i % 8 == 0)
        bits ^= _BV(6);
    if (i % 9 == 0)
        bits ^= _BV(7);

    output::write(bits);
 
    if (++i == 5 * 6 * 7 * 8 * 9)
        i = 0;
}

enum sw_pos_t { sw_err, sw_dn, sw_mid, sw_up };

static uint8_t read_spdts() // return sw0 | (sw1 << 2)
{
    static uint16_t midpoints[] = { 956, 846, 757, 659, 559, 459, 365, 281, 119 };

    uint16_t x = adc::read<spdts>();

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

void setup()
{
    output::setup();

    in_rst::setup();
    in_clk::setup();
    adc::setup<128>();

    PCMSK0 |= _BV(PCINT3);  // enable pin-change interrupt on rst (PCINT3)
    GIMSK |= _BV(PCIE0);    // enable channel 0 pin-change interrupts

    PCMSK1 |= _BV(PCINT9);  // enable pin-change interrupt on clk (PCINT9)
    GIMSK |= _BV(PCIE1);    // enable channel 1 pin-change interrupts

    sei();
}

/*
static bool update(uint8_t& c, uint8_t m)
{
    if (c++ >= m)
    {
        c = 0;
        return true;
    }

    return false;
}
*/

void loop()
{
    static sw_pos_t sw0 = sw_err, sw1 = sw_err;
    uint8_t s = read_spdts();
    sw_pos_t _sw0 = static_cast<sw_pos_t>(s & 0x3);
    sw_pos_t _sw1 = static_cast<sw_pos_t>(s >> 2);

    if (_sw0 != sw0 || _sw1 != sw1)
    {
            i = 0;
            sw0 = _sw0;
            sw1 = _sw1;
    }
}

