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
	delay(25);
}

int main()
{
	setup();
	for (;;)
		loop();
}

