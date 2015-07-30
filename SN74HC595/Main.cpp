#include "../AVR/Pins.h"
#include "../AVR/Delay.h"

template<class DT, class CK>
void shift_out(uint8_t x)
{
	for (int8_t i = 7; i >= 0; --i)
	{
		clear<CK>();
		write<DT>(((x >> i) & 0x01) != 0);
		set<CK>();
	}
}

typedef pin_t<PB, 3> LED;
typedef pin_t<PB, 0> CLOCK;
typedef pin_t<PB, 1> LATCH;
typedef pin_t<PB, 2> DATA;

void setup()
{
	digital_out<LED>();
	digital_out<CLOCK, LATCH, DATA>();
}

void loop()
{
	for (int i = 0; i < 255; ++i)
	{
		toggle<LED>();
		delay(1);
		clear<LATCH>();
		shift_out<DATA, CLOCK>(i);
		set<LATCH>();
	}
}

int main()
{
	setup();
	for (;;)
		loop();
}

