#include <AVR/Main.h>
#include <AVR/Pins.h>
#include <AVR/Delay.h>

typedef pin_t<PB, 0> FOCUS;
typedef pin_t<PB, 1> EXPOSE;

void shoot()
{
    set<FOCUS>();
    delay_ms(100);
    set<EXPOSE>();
    delay_ms(100);
    clear<EXPOSE>();
    clear<FOCUS>();
}

void setup()
{
    digital_out<FOCUS>();
    digital_out<EXPOSE>();
}

void loop()
{
    shoot();
    delay_ms(30000);
}

