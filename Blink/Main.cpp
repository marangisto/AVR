#include <AVR/Pins.h>
#include <AVR/Delay.h>

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
	LED::toggle();
	delay_ms(1000);
}

int main()
{
	setup();
	for (;;)
		loop();
}

