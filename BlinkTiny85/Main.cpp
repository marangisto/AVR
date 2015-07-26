#include "../AVR/Pins.h"
#include "../AVR/Delay.h"

typedef pin_t<PB, 3> LED;

// see CLKPR Clock Prescale Register definition

enum system_clock_prescale_t
	{ system_clock_prescale_1	= 0x00
	, system_clock_prescale_2	= 0x01
	, system_clock_prescale_4	= 0x02
	, system_clock_prescale_8	= 0x03
	, system_clock_prescale_16	= 0x04
	, system_clock_prescale_32	= 0x05
	, system_clock_prescale_64	= 0x06
	, system_clock_prescale_128	= 0x07
	, system_clock_prescale_256	= 0x08
	};

void set_system_clock_prescale(system_clock_prescale_t s)
{
	CLKPR = (1 << 7);	// clock prescaler change enable
	CLKPR = static_cast<uint8_t>(s);
}

void setup()
{
	set_system_clock_prescale(system_clock_prescale_1);
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

