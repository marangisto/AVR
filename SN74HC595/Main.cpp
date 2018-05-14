#include <AVR/Main.h>
#include <AVR/Pins.h>
#include <AVR/Delay.h>
#include <AVR/SN74HC595.h>

typedef output_t<PD, 5> LED;
typedef output_t<PD, 4> CLOCK;
typedef output_t<PB, 0> LATCH;
typedef output_t<PD, 3> DATA;

typedef sn74hc595_t<DATA, CLOCK, LATCH, MSB_FIRST> sn74hc595;

void setup()
{
    LED::setup();
    sn74hc595::setup();
}

void loop()
{
    for (int i = 0; i < 255; ++i)
    {
        uint16_t j = (i << 8) | (255-i);
        LED::toggle();
        delay_ms(25);
        sn74hc595::write(j);
    }
}

