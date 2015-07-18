#include <avr/io.h>
#include "../AVR/Pins.h"

typedef pin_t<PD,1> STEP;

int main()
{
	digital_out<STEP>();

	for (;;)
	{
		set<STEP>();
		clear<STEP>();
	}
}

