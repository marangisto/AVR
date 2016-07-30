#include <Arduino/Pins.h>
#include <AVR/Delay.h>

typedef D13 LED;

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

