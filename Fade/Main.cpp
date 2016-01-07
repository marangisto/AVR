#include <AVR/Pins.h>
#include <AVR/Delay.h>
#include <AVR/Timer.h>

typedef output_t<PB, 1> LED;
typedef timer_t<1> T;

void setup()
{
	LED::setup();
    T::prescale(T::prescale_1);
    T::pwma();
}

void loop()
{
    static const int max_dc = 1023; // max duty-cycle
    static int d_dc = 8;            // duty-cycle increment
    static int dc = 0;              // duty-cycle

    T::ocra() = dc;

    if (dc + d_dc < 0 || dc + d_dc > max_dc)
        d_dc = -d_dc;
    dc += d_dc;

	delay_ms(25);
}

int main()
{
	setup();
	for (;;)
		loop();
}

