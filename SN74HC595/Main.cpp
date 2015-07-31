#include "../AVR/Pins.h"
#include "../AVR/Delay.h"

enum shift_order_t { LSB_FIRST = 0, MSB_FIRST = 1};

template<class DT, class CK>
static inline void clock_out_bit(uint8_t x, int i)
{
	clear<CK>();
	write<DT>(((x >> i) & 0x01) != 0);
	set<CK>();
}

template<class DT, class CK, shift_order_t SD>
struct shift_out_impl
{
	static void shift_out(uint8_t x)
	{
		for (int8_t i = 7; i >= 0; --i)
			clock_out_bit<DT, CK>(x, i);
	}
};

template<class DT, class CK>
struct shift_out_impl<DT, CK, LSB_FIRST>
{
	static void shift_out(uint8_t x)
	{
		for (int8_t i = 0; i < 8; ++i)
			clock_out_bit<DT, CK>(x, i);
	}
};

template<class DT, class CK, shift_order_t SD>
void shift_out(uint8_t x)
{
	shift_out_impl<DT, CK, SD>::shift_out(x);
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
		shift_out<DATA, CLOCK, MSB_FIRST>(i);
		set<LATCH>();
	}
}

int main()
{
	setup();
	for (;;)
		loop();
}

