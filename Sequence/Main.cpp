#define NO_TIMER_VECTORS 1
#include <AVR/UART.h>
#include <AVR/TWI.h>
#include <AVR/ADC.h>
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>
#include <Arduino/Pins.h>

typedef D13 LED;

typedef twi_master_t<0> twi;

static uint8_t twi_addr = 0;

ISR(TWI_vect)
{
    twi::isr();
}

static const int adc_bpm = 3;

typedef timer_t<2> aux;

static const uint16_t aux_prescale = 64;
static volatile uint16_t aux_count = 0;
static volatile bool do_step = false;

ISR(TIMER2_OVF_vect)
{
    static uint16_t i = 0;

    if (i++ >= aux_count)
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

    aux_count = 1 + adc::read<adc_bpm>();

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
        printf("%d %d\n", i, value);
    }

    delay_us(500);
}

