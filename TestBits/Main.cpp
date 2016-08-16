#include <AVR/Main.h>
#include <AVR/Pins.h>
#include <AVR/Delay.h>

typedef output_t<PD, 0> A;
typedef output_t<PD, 1> B;
typedef output_t<PD, 2> C;
typedef output_t<PD, 3> D;
typedef output_t<PD, 4> E;
typedef output_t<PD, 5> F;
typedef output_t<PD, 6> G;
typedef output_t<PD, 7> H;

typedef outputs_t<H, G, F, E, D, C, B, A> bits;

void setup()
{
    bits::setup();
}

void loop()
{
    static uint8_t i = 0;

    bits::write(i++);
    delay_ms(250);
}

