#ifndef A4988_H
#define A4988_H

#include "../AVR/Pins.h"
#include "../AVR/Bits.h"
#include "../AVR/Delay.h"

struct micro_step_t
{
	enum e
		{ full_step			= 0x0
		, half_step			= 0x1
		, quarter_step		= 0x2
		, eigth_step		= 0x3
		, sixteenth_step	= 0x7
		};

	static const char *to_string(e ms)
	{
		switch (ms)
		{
			case full_step:			return "1/1 ";
			case half_step:			return "1/2 ";
			case quarter_step:		return "1/4 ";
			case eigth_step:  		return "1/8 ";
			case sixteenth_step:	return "1/16";
			default: return "illegal-step";
		}
	}

	static uint8_t shift(e ms)
	{
		switch (ms)
		{
			case full_step:			return 0;
			case half_step:			return 1;
			case quarter_step:		return 2;
			case eigth_step:  		return 3;
			case sixteenth_step:	return 4;
			default: 				return 0;
		}
	}
};


template<class DIR, class STEP, class RESET, class MS1, class MS2, class MS3, class ENABLE>
class a4988_t
{
public:
	static void setup()
	{
		digital_out<DIR>();
		digital_out<STEP>();
		digital_out<RESET>();
		digital_out<MS1>();
		digital_out<MS2>();
		digital_out<MS3>();
		digital_out<ENABLE>();
		set<ENABLE>();				// active low
		set<RESET>();				// active low
	}

	static inline void enable()
	{
		clear<ENABLE>();
		delay_us(100);			// guessing, didn't find spec
	}

	static inline void disable() { set<ENABLE>(); }

	static inline void dir(bool d) { write<DIR>(d); }

	static inline void step()
	{
		set<STEP>();			// 2 cycles
		nop<14>();				// need 1us so total 16 cycles FIXME: delay based on F_CPU
		clear<STEP>();
	}

	static void reset()
	{
		clear<RESET>();			// 2 cycles
		delay_us(10);			// 4us is shaky, so 10us should be reliable, didn't find spec
		set<RESET>();
	}

	typedef bits_t<MS1, MS2, MS3> MS;

	static void micro_step(micro_step_t::e ms)
	{
		write_bits<MS>(ms);
		nop<4>();				// need 200ns, FIXME: delay based on F_CPU
	}

private:
};

#endif // A4988_H

