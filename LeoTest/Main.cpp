#include <AVR/Main.h>
#include <AVR/Pins.h>
#include <AVR/Delay.h>

typedef pin_t<PC,7> LED;

void setup()
{
    digital_out<LED>();
}

void loop()
{
    toggle<LED>();
    delay(250);
}

