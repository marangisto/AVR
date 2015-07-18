#include "../AVR/Pins.h"
#include "../AVR/Delay.h"

typedef pin_t<PC,7> LED;

int main()
{
	digital_out<LED>();

	for (;;)
	{
        toggle<LED>();
		delay(250);
	}
}

