#include "../AVR/Pins.h"
#include "../AVR/Delay.h"

#if defined(__AVR_ATmega32U4__)
typedef pin_t<PC, 7> LED;			// leonardo
#else
typedef pin_t<PB, 5> LED;			// uno
#endif

void setup()
{
	digital_out<LED>();
}

void loop()
{
	toggle<LED>();
	delay_ms(50);
}

int main()
{
	setup();
	for (;;)
		loop();
}

