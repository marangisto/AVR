#include "../AVR/Pins.h"
#include "../AVR/Delay.h"
#include "../AVR/ADC.h"
#include "../AVR/LCD1602A.h"
#include <stdlib.h>

typedef analog_input_t<3> BTNS;
typedef pin_t<PB, 0> CLOCK;
typedef pin_t<PB, 1> LATCH;
typedef pin_t<PB, 2> DATA;

typedef lcd1602a_t<DATA, CLOCK, LATCH> lcd;

void setup()
{
	adc::setup();
	lcd::setup();
}

void loop()
{
	static char buf[32];
	static float y = 0;
	static int i = 0;

	float x = adc::read<BTNS>();

	i++;

	lcd::clear();
	lcd::set_pos(0, 0);
	lcd::write(dtostrf(x, 7, 2, buf));
	lcd::set_pos(1, 0);
	lcd::write(itoa(i, buf, 10));
	lcd::set_pos(1, 8);
	lcd::write(dtostrf(adc::temp(), 7, 2, buf));
	delay_ms(200);
}

int main()
{
	setup();
	for (;;)
		loop();
}

