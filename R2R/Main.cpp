#include <AVR/Main.h>
#include <AVR/UART.h>
#include <AVR/Delay.h>
#include <AVR/SPI.h>
#include <AVR/Timer.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef timer_t<1> timer;
typedef output_t<PC, 5> trig;
typedef spi_t<2, msb_first> spi;

static volatile uint16_t period = 305;   // this gives us a sample rate of 44.1kHz

static void timer_isr()
{
    static uint8_t i = 0;

    timer::counter() = 65536 - period;
    spi::write(i++);
}

void setup()
{
    spi::setup();
    trig::setup();
    UART::setup<9600>();
    timer::setup<normal_mode>();
    timer::clock_select<1>();
    timer::isr(timer_isr);
    timer::enable();
    sei();
}

void loop()
{
    static int i = 0;
    /*

    if (i == 0)
        trig::toggle();

    spi::write(i++);

    return;

    */
    char buf[80] = "", *p;

    printf("%04d> ", i++);

    if (!fgets(buf, sizeof(buf), stdin))
    {
        printf("reset\n");
        return;
    }

    if ((p = strpbrk(buf, "\r\n")))
        *p = 0;

    uint16_t x = atoi(buf);
    printf("got '%s' = %u\n", buf, x);

    if (*buf != 0)
        period = x;

    delay_ms(10);
}

