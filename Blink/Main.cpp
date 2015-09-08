#include "../AVR/Pins.h"
#include "../AVR/Delay.h"

#if defined(__AVR_ATmega32U4__)
typedef output_t<PC, 7> LED;			// leonardo
#else
typedef output_t<PB, 5> LED;			// uno
#endif

void setup()
{
	LED::setup();
}

void loop()
{
	static bool x = false;

	x = !x;
	LED::write(x);
	delay_ms(150);
}

int main()
{
	setup();
	for (;;)
		loop();
}

