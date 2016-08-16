#include "../AVR/Pins.h"
#include "../AVR/Delay.h"
#include "../AVR/SN74HC595.h"

typedef pin_t<PB, 3> LED;
typedef pin_t<PB, 0> CLOCK;
typedef pin_t<PB, 1> LATCH;
typedef pin_t<PB, 2> DATA;

typedef sn74hc595_t<DATA, CLOCK, LATCH, MSB_FIRST> sn74hc595;

void setup()
{
    digital_out<LED>();
    sn74hc595::setup();
}

void loop()
{
    for (int i = 0; i < 255; ++i)
    {
        toggle<LED>();
        delay_ms(25);
        sn74hc595::write(i);
    }
}

int main()
{
    setup();
    for (;;)
        loop();
}

