#include "../AVR/Pins.h"
#include "../AVR/Delay.h"
#include "../AVR/LCD1602A.h"
#include "Buttons.h"
#include <stdlib.h>

typedef pin_t<PB, 0> CLOCK;
typedef pin_t<PB, 1> LATCH;
typedef pin_t<PB, 2> DATA;

typedef lcd1602a_t<DATA, CLOCK, LATCH> lcd;
typedef buttons_t<analog_input_t<3>> btns;

void setup()
{
	btns::setup();
	lcd::setup();
}

void loop()
{
	static char buf[32];
	static int i = 0;

	uint8_t x = btns::read();

	switch (x)
	{
		case 1: --i; break;
		case 2: ++i; break;
		case 3: i = 0; break;
		case 4: i -= 10; break;
		case 5: i += 10; break;
		default: ;
	}

	lcd::set_pos(0, 0);
	lcd::write(itoa(i, buf, 10));
	lcd::write("            ");

	delay_ms(1);
}

int main()
{
	setup();
	for (;;)
		loop();
}

