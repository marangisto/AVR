#include <AVR/UART.h>
#include <AVR/Delay.h>
#include <Arduino/Pins.h>
#include <stdio.h>

typedef D13 LED;

void setup()
{
	LED::setup();
    UART::setup<9600>();
}

void loop()
{
    static int i = 0;

	LED::toggle();
    printf("Dead Beef %d\n", i++);
	delay_ms(10);
}

int main()
{
	setup();
	for (;;)
		loop();
}

