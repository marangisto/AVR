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
    static const int max_dc = 0x3ff;    // max duty-cycle
    static bool dir = false;            // scan direction
    static int dc = 0;                  // duty-cycle

    T::ocra() = dc;

    if (dc <= 0 || dc >= max_dc)
        dir = !dir;
    
    if (dir)
        dc = (dc << 1) | 1;
    else
        dc = dc >> 1;

	delay_ms(100);
}

int main()
{
	setup();
	for (;;)
		loop();
}

