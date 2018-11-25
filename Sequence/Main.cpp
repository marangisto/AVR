#include <AVR/UART.h>
#include <AVR/TWI.h>
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <Arduino/Pins.h>

typedef D13 LED;

typedef twi_master_t<0> twi;

static uint8_t twi_addr = 0;

ISR(TWI_vect)
{
    twi::isr();
}

void setup()
{
    LED::setup();
    UART::setup<115200>();
    //twi::setup(0x20);
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
    static uint8_t j = 0;

    if (++j > 250)
    {
        j = 0;
        i = (i + 1) & 0x07;
        uint8_t bit = 1 << i;
        uint8_t led_cmd[2] = { 0, bit };

        twi::write(twi_addr, led_cmd, sizeof(led_cmd));
        twi::wait_idle();

        delay_ms(500);

        uint8_t read_cmd[2] = { 1, i };
        uint16_t value = 0;

        twi::write_read(twi_addr, read_cmd, sizeof(read_cmd), reinterpret_cast<uint8_t*>(&value), sizeof(value));
        twi::wait_idle();
        printf("%d %d\n", i, value);

        LED::toggle();
    }

    delay_us(500);
}

