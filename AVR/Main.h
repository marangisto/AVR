#pragma once

#include <avr/io.h>

void setup();
void loop();

int main()
{

#if defined(__AVR_ATmega32U4__)
    // If this is leonardo the boot-loader leaves USB interrupts on
    // and we'd crash on sei! So we turn off USB here...
    USBCON &= ~_BV(USBE);
#endif
    setup();
    for (;;)
        loop();
}

