#include "../AVR/Pins.h"
#include "../AVR/Delay.h"
#include "../AVR/LCD1602A.h"
#include <stdlib.h>

typedef pin_t<PB, 3> BTNS;
typedef pin_t<PB, 0> CLOCK;
typedef pin_t<PB, 1> LATCH;
typedef pin_t<PB, 2> DATA;

typedef lcd1602a_t<DATA, CLOCK, LATCH> lcd;

void setup()
{
	digital_in<BTNS>();
	lcd::setup();

	ADCSRA |= ((1<<ADPS1) | (1<<ADPS0));				// ADC prescale 8 (1MHz / 8 = 125kHz)
	ADMUX |= ((1<<MUX1) | (1<<MUX0));					// Selct PB3 (on Attiny85)
	ADCSRB |= 0;										// free running mode
	ADCSRA |= ((1<<ADEN) | (1 << ADSC) | (1 << ADATE));	// start ADC
}

void loop()
{
	static char buf[32];
	static float y = 0;
	static int i = 0;

	float x = ADCW;

	i++;

	lcd::clear();
	lcd::set_pos(0, 0);
	lcd::write(dtostrf(x, 7, 2, buf));
	lcd::set_pos(1, 0);
	lcd::write(itoa(i, buf, 10));
	delay_ms(200);
}

int main()
{
	setup();
	for (;;)
		loop();
}

