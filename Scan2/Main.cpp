#include "../AVR/Pins.h"
#include "../AVR/Delay.h"
#include "../AVR/LCD1602A.h"
#include "Buttons.h"
#include <stdlib.h>

typedef pin_t<PC, 1> CLOCK;
typedef pin_t<PC, 2> LATCH;
typedef pin_t<PC, 3> DATA;

typedef lcd1602a_t<DATA, CLOCK, LATCH> lcd;
typedef buttons_t<analog_input_t<0>> btns;

void setup()
{
	btns::setup();
	lcd::setup();
}

void loop()
{
	static int i = 0, j = 0;

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
	lcd::write(i);
	lcd::write("            ");

	if (++j > 200)
	{
		double y = rand() / 32768.0;
		static double z = 0;

		lcd::set_pos(1, 0);
		lcd::write(y);
		lcd::set_pos(1, 8);
		lcd::write_e(y - z, 1);
		j = 0;
		z = y;
	}

	delay_ms(1);
}

int main()
{
	setup();
	for (;;)
		loop();
}

