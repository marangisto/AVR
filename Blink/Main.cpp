#include "../AVR/Pins.h"
#include "../AVR/Delay.h"

typedef pin_t<PB, 3> LED;

void setup()
{
	digital_out<LED>();
}

void loop()
{
	toggle<LED>();
	delay_ms(250);
}

int main()
{
	setup();
	for (;;)
		loop();
}

