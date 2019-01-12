#include <AVR/Main.h>
#include <AVR/Pins.h>
#include <AVR/Delay.h>
#include <avr/interrupt.h>

typedef output_t<PB, 0> OX;

typedef input_t<PB, 1, enable_pullup> IX;

static volatile uint8_t cx = 0;

ISR(PCINT0_vect)
{
    static volatile uint8_t last = PINB;
    uint8_t bits = PINB;

    if (bits != last)
    {
        cx = 25;
        last = bits;
    }
}

void setup()
{
    OX::setup();
    IX::setup();
    GIMSK |= (1 << PCIE);
    PCMSK |= (1 << PCINT1);
    sei();
}

void loop()
{
    if (cx > 0)
    {
        OX::set();
        --cx;
    }
    else
        OX::clear();

    delay_ms(1);
}

