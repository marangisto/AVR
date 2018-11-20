#include <AVR/TWI.h>
#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <Arduino/Pins.h>
#include <AVR/UART.h>

typedef D13 LED;

//typedef twi_slave_t<0> twi;
typedef twi_master_t<0> twi;

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

/*
    for (uint8_t a = 0; a < 128; ++a)
    {
        uint8_t buf[1];

        if ((a & 0x78) == 0 || (a & 0x78) == 0x78)
            continue;   // reserved address
        if (twi::write(a, buf, 0) == 0)
        {
            lcd::clear();
            lcd::write("TWI DEVICE: 0x");
            lcd::write(a, 16);
            delay_ms(1000);
        }
    }

*/
}

void loop()
{
    static uint8_t i = 0;
    static uint8_t j = 0;
    static const uint8_t twi_addr = 0x20;

    if (j++ == 0)
    {
        i = (i + 1) & 0x07;
        uint8_t bit = 1 << i;
        uint8_t buf[2] = { 0, bit };

        twi::write(twi_addr, buf, 2);
        twi::wait_idle();

        LED::toggle();
    }

    delay_us(500);
}

