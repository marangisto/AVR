#define NO_TIMER_VECTORS 1
#include <AVR/UART.h>
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <AVR/TWI.h>
#include <AVR/ADC.h>
#include <AVR/Pins.h>
#include <AVR/Buttons.h>
#include <stdlib.h>

template <class T> const T& max(const T& a, const T& b) { return (a<b) ? b : a; }
template <class T> const T& min(const T& a, const T& b) { return (a<b) ? a : b; }

static const uint8_t twi_addr = 0x62;   // FIXME: use EEPROM and reset config

//typedef twi_master_t<1> twi;
typedef twi_slave_t<1> twi;

typedef output_t<PB, 2> leds;

typedef output_t<PD, 5> sink_2;
typedef output_t<PD, 6> sink_6;
typedef output_t<PB, 0> sink_7;
typedef output_t<PD, 7> sink_3;
typedef output_t<PD, 4> sink_0;
typedef output_t<PD, 3> sink_4;
typedef output_t<PB, 7> sink_1;
typedef output_t<PB, 6> sink_5;

typedef outputs_t<sink_0, sink_1, sink_2, sink_3, sink_4, sink_5, sink_6, sink_7> sinks;

typedef input_t<PB, 1> sense_a;
typedef input_t<PD, 2> sense_b;

static uint8_t led_state = 0;
static uint8_t swa_state = 0;
static uint8_t swb_state = 0;

static const uint8_t ch0 = 5;
static const uint8_t ch1 = 3;
static const uint8_t ch2 = 0;
static const uint8_t ch3 = 6;
static const uint8_t ch4 = 4;
static const uint8_t ch5 = 2;
static const uint8_t ch6 = 1;
static const uint8_t ch7 = 7;

static uint16_t adc_value[8];

typedef timer_t<2> aux;

ISR(TIMER2_OVF_vect)
{
    static const uint8_t led_duty = 2; // of 255
    static uint8_t i = 0;

    switch (i)
    {
    case 0:
        sinks::write(~led_state);
        leds::set();
        break;
    case led_duty:
        leds::clear();
        break;
    default:
        if (i > led_duty && i - led_duty - 1 < 8)
        {
            uint8_t bit = 1 << (i - led_duty - 1);
            sinks::write(~bit);
            if (sense_a::read())
                swa_state &= ~bit;
            else
                swa_state |= bit;
        }
        else if (i > led_duty + 8 && i - led_duty - 9 < 8)
        {
            uint8_t bit = 1 << (i - led_duty - 9);
            sinks::write(~bit);
            if (sense_b::read())
                swb_state &= ~bit;
            else
                swb_state |= bit;
        }
    }

    i++;
}

ISR(TWI1_vect)
{
    twi::isr();
}

void setup()
{
    leds::setup();
    sinks::setup();
    sinks::write(~1);   // first sink active
    adc::setup<128>();
    aux::setup<normal_mode>();
    aux::clock_select<1>();
    aux::enable();

    twi::setup(twi_addr);

    sei();

    srand(1);

    for (uint8_t i = 0; i < 50; ++i)
    {
        led_state = i < 25 ? rand() : (1 << (i & 0x07));
        delay_ms(25);
    }

    led_state = 0;
}

void loop()
{
    static uint8_t i = 0;

    switch (i++ & 0x07)
    {
        case 0: adc_value[0] = adc::read<ch0>(); break;
        case 1: adc_value[1] = adc::read<ch1>(); break;
        case 2: adc_value[2] = adc::read<ch2>(); break;
        case 3: adc_value[3] = adc::read<ch3>(); break;
        case 4: adc_value[4] = adc::read<ch4>(); break;
        case 5: adc_value[5] = adc::read<ch5>(); break;
        case 6: adc_value[6] = adc::read<ch6>(); break;
        case 7: adc_value[7] = adc::read<ch7>(); break;
    }

    uint8_t buf[2] = { 0, 0 };

    twi::start_with_data(buf, sizeof(buf));
    twi::wait_idle();

    switch (twi::get_buf()[0])
    {
    case 0:                                 // write leds
        led_state = twi::get_buf()[1];
        break;
    case 1:                                 // read step
        {
            uint16_t value = adc_value[twi::get_buf()[1]];
            twi::start_with_data(reinterpret_cast<uint8_t*>(&value), sizeof(value));
            twi::wait_idle();
        }
        break;
    default:
        ;                                   // ignore illegal command
    }

    //led_state = twi::status();

    delay_ms(1);
}

