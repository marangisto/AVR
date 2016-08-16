#include <AVR/Main.h>
#include <AVR/Delay.h>
#include <Arduino/Pins.h>

typedef D13 LED;

void setup()
{
    LED::setup();
}

void loop()
{
    LED::toggle();
    delay_ms(1000);
}

