#include <kinetis.h>

extern "C" void delay(uint32_t ms); // FIXME: figure out where this comes from

// N.B. PORTC-PIN5 == LED 'Pin 13'

extern "C"
void setup()
{
    GPIOC_PDDR |= (1<<5);
    PORTC_PCR5 = PORT_PCR_SRE | PORT_PCR_DSE | PORT_PCR_MUX(1);
}

extern "C"
void loop()
{
    //GPIOC_PSOR = (1<<5);  // SET
    //delay(50);

    //GPIOC_PCOR = (1<<5);  // CLEAR

    GPIOC_PTOR = (1<<5);    // TOGGLE
    delay(500);
}

