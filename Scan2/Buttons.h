#ifndef BUTTONS_H
#define BUTTONS_H

#include "../AVR/ADC.h"

template<class CH>
class buttons_t
{
public:
	static void setup()
	{
		adc::setup();
	}

	static uint8_t read()
	{
		static uint8_t last_read = 0;
		uint8_t x = raw_read();

		if (x != last_read)
		{
			last_read = x;
			return x;
		}

		return 0;
	}

private:
	static uint8_t raw_read()
	{
		static uint16_t limits[] = { 940, 751, 533, 302, 92 };
		static uint8_t n_limits = sizeof(limits) / sizeof(*limits);
		uint16_t x = adc::read<CH>();

		for (uint8_t i = 0; i < n_limits; ++i)
			if (x > limits[i])
				return i;

		return n_limits;
	}
};

#endif // BUTTONS_H
