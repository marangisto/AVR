#define NO_TIMER_VECTORS 1
#include <AVR/UART.h>
#include <AVR/TWI.h>
#include <AVR/ADC.h>
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <Arduino/Pins.h>

typedef D13 LED;
typedef output_t<PD, 2> trig_a;
typedef output_t<PD, 3> trig_b;

typedef input_t<PD, 4, enable_pullup> btn_a;
typedef input_t<PD, 5, enable_pullup> btn_b;

typedef twi_master_t<0> twi;

static uint8_t twi_addr = 0;

ISR(TWI_vect)
{
    twi::isr();
}

static const int adc_bpm = 3;

typedef timer_t<1> pwm;
typedef timer_t<2> aux;

static const uint16_t aux_prescale = 64;
static volatile uint16_t aux_count = 0;
static volatile bool auto_step = true;
static volatile bool do_step = false;

ISR(TIMER2_OVF_vect)
{
    static uint16_t i = 0;

    if (auto_step && i++ >= aux_count)
    {
        do_step = true;
        i = 0;
    }
}

void setup()
{
    LED::setup();
    UART::setup<115200>();
    adc::setup<128>();
    trig_a::setup();
    trig_b::setup();
    btn_a::setup();
    btn_b::setup();

    pwm::setup<fast_pwm, top_0x1ff>();
    pwm::clock_select<1>();
    pwm::output_pin<channel_a>::setup();
    pwm::output_pin<channel_b>::setup();
    pwm::compare_output_mode<channel_a, clear_on_compare_match>();
    pwm::compare_output_mode<channel_b, clear_on_compare_match>();

    aux::setup<normal_mode>();
    aux::clock_select<aux_prescale>();
    aux::enable();

    twi::setup();
    sei();

    printf("Marangisto Sequence 0.1\n");

    for (uint8_t a = 0; a < 128; ++a)
    {
        uint8_t buf[1];

        if ((a & 0x78) == 0 || (a & 0x78) == 0x78)
            continue;   // reserved address
        if (twi::write(a, buf, 0) == 0)
        {
            twi_addr = a;
            printf("found sub-sequence at 0x%02x\n", a);
        }
    }
}

void loop()
{
    static uint8_t i = 0;
    static bool last_btn_a = true;
    static bool last_btn_b = true;

    aux_count = 1 + adc::read<adc_bpm>();

    bool b = btn_a::read();

    if (b != last_btn_a && !b)                      // run / stop toggle
            auto_step = !auto_step;
    last_btn_a = b;

    b = btn_b::read();

    if (b != last_btn_b && !b)                      // run / stop toggle
            do_step = true;
    last_btn_b = b;

    if (do_step)
    {
        LED::toggle();
        do_step = false;
        i = (i + 1) & 0x07;

        uint8_t bit = 1 << i;
        uint8_t led_cmd[2] = { 0, bit };

        twi::write(twi_addr, led_cmd, sizeof(led_cmd));
        twi::wait_idle();
        delay_us(100);

        uint8_t read_cmd[2] = { 1, i };
        uint16_t value = 0;

        twi::write_read(twi_addr, read_cmd, sizeof(read_cmd), reinterpret_cast<uint8_t*>(&value), sizeof(value));
        twi::wait_idle();
        bool sw_a = (value & (1 << 13)) != 0;
        bool sw_b = (value & (1 << 14)) != 0;
        printf("%d %d %s\n", i, value, sw_a ? "a" : (sw_b ? "b" : " "));
        if (sw_a )
        {
            pwm::output_compare_register<channel_a>() = /* 0x1ff - */ (value >> 2);   // inverted output
            trig_a::set();
            delay_us(100);
            trig_a::clear();
        }
        else if (sw_b)
        {
            pwm::output_compare_register<channel_b>() = /* 0x1ff - */ (value >> 2);   // inverted output
            trig_b::set();
            delay_us(100);
            trig_b::clear();
        }
    }

    delay_us(500);
}

