#ifndef SN75HC595_H
#define SN75HC595_H

#include "../AVR/Pins.h"

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

template<class DT, class CK, class LT, shift_order_t SD>
struct sn74hc595_t
{
	static void setup()
	{
		digital_out<DT, CK, LT>();
	}

	static void write(uint8_t x)
	{
		clear<LT>();
		shift_out<DT, CK, SD>(x);
		set<LT>();
	}
};

#endif // SN75HC595_H

