#include <avr/io.h>
#include "../AVR/Pins.h"

void delay_loop_2(uint16_t __count)
{
	__asm__ volatile
	(
		"1: sbiw %0,1" "\n\t"
		"brne 1b"
			: "=w" (__count)
			: "0" (__count)
	);
}

void delay(uint16_t n)
{
	while (n-- > 0)
		delay_loop_2(4000);
}

typedef pin_t<PD,0> ledA;
typedef pin_t<PD,1> ledB;
typedef pin_t<PD,2> btnDn;
typedef pin_t<PD,3> btnUp;

void setup()
{
	digital_in<btnDn, btnUp>();
	set<btnDn, btnUp>(); 			// pull-ups
	digital_out<ledA, ledB>();
}

void loop()
{
	static uint16_t ms = 250;
	bool bu, bd;

	read<btnUp, btnDn>(bu, bd);

	toggle<ledA>();

	if (bu)
	{
		toggle<ledB>();
		ms += 10;
	}
	else if (bd)
	{
		toggle<ledB>();
		ms = ms > 10 ? ms - 10 : 10;
	}

	delay(ms);
}

int main()
{
	setup();
	for (;;)
		loop();
}

