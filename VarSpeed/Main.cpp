#include <AVR/Main.h>
#include <AVR/ADC.h>
#include <AVR/UART.h>
#include <AVR/Timer.h>
#include <AVR/Delay.h>
#include <Arduino/Pins.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

template <class T> const T& max(const T& a, const T& b)
{
    return (a<b) ? b : a;
}

typedef output_t<PB, 0> STEP;
typedef output_t<PD, 7> DIR;
typedef output_t<PD, 6> ENABLE;

typedef timer_t<1> timer;

static const int PS = 64;
static uint16_t step_t = 0;

void isr()
{
    timer::counter() = 65535 - step_t;
    STEP::set();
    delay_us(1);    // minumum 1us
    STEP::clear();
}

void setup()
{
    adc::setup<128>();

    timer::setup<normal_mode>();
    timer::clock_select<PS>();
    timer::isr(isr);
    timer::enable();

    STEP::setup();
    DIR::setup();
    ENABLE::setup();
    ENABLE::set();
    UART::setup<9600>();

    sei();
}

void loop()
{
    static uint16_t last_x = -1;
    uint16_t x = adc::read<5>();

    if (x != last_x)
    {
        uint16_t n = max(0U, static_cast<uint16_t>(round(F_CPU / (PS * x) - 1)));

        printf("adc<5> = %d, n = %u\n", x, n);
        step_t = n;
        ENABLE::write(x == 0);
        last_x = x;
    }

    delay_ms(10);
}

