#include <AVR/Delay.h>
#include <AVR/LCD1602A/DFR0009.h>
#include <stdlib.h>

typedef D13 LED;
typedef tc1602_t<dfr0009_t> lcd;

void setup()
{
	LED::setup();
    lcd::setup();
}

void loop()
{
    static uint16_t x = 0;

	// LED::toggle();
    lcd::set_pos(0, 0);
    lcd::write(x++, 16);
    lcd::write("      ");
	//delay_ms(50);
}

int main()
{
	setup();
	for (;;)
		loop();
}

