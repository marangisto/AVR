#include <AVR/Main.h>
#include <AVR/Delay.h>

#if defined(__AVR_ATtiny84__)
    #include <AVR/Pins.h>
    typedef output_t<PB, 2> LED;
#else
    #include <Arduino/Pins.h>
    typedef D13 LED;
#endif

void setup()
{
    LED::setup();
}

void loop()
{
    LED::toggle();
    delay_ms(1000);
}

