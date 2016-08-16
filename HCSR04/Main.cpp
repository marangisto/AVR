#include <AVR/Main.h>
#include <AVR/Pulse.h>
#include <AVR/SN74HC595.h>

typedef pin_t<PB, 3> TRIG;
typedef pin_t<PB, 4> ECHO;
typedef pin_t<PB, 0> CLOCK;
typedef pin_t<PB, 1> LATCH;
typedef pin_t<PB, 2> DATA;

typedef sn74hc595_t<DATA, CLOCK, LATCH, MSB_FIRST> sn74hc595;

void setup()
{
    digital_out<TRIG>();
    digital_in<ECHO>();
    sn74hc595::setup();
}

void loop()
{
    set<TRIG>();
    delay_us(10);
    clear<TRIG>();

    uint32_t w = pulse_width<ECHO, true>(10000);
    float d = w / (2 * 29.1);

    sn74hc595::write(static_cast<uint8_t>(d));
    delay_ms(20);
}

