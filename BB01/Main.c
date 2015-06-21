#include <avr/io.h>

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

void setup()
{
	DDRD |= (1 << 0);
}

void loop()
{
	delay(1000);
	PORTD ^= (1 << 0);
}

int main()
{
	setup();
	for (;;)
		loop();
}

