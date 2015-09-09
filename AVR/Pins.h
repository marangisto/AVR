#pragma once

#include <avr/io.h>
#include "Traits.h"

template<class PORT> struct port_t
{
	static inline volatile uint8_t& ddr();
	static inline volatile uint8_t& port();
	static inline const volatile uint8_t& pin();
};

static const bool enable_pullup = true;

template<class PORT, unsigned BIT, bool PULLUP = false> struct input_t : port_t<PORT>
{
	static_assert(BIT < 8, "bit out of range");

	static const int bit = BIT;
	static const uint8_t mask = 1 << BIT;

	static void setup()
	{
		port_t<PORT>::ddr() &= ~mask;
		if (PULLUP)
			port_t<PORT>::reg() |= mask;
	}

	static bool read()
	{
		return (port_t<PORT>::pin() & mask) != 0;
	}
};

template<class PORT, unsigned BIT> struct output_t : port_t<PORT>
{
	static_assert(BIT < 8, "bit out of range");

	static const int bit = BIT;
	static const uint8_t mask = 1 << BIT;

	static inline void setup()
	{
		port_t<PORT>::ddr() |= mask;
	}

	static inline void set()
	{
		port_t<PORT>::reg() |= mask;
	}

	static inline void clear()
	{
		port_t<PORT>::reg() &= ~mask;
	}

	static inline void write(bool x)
	{
		x ? set() : clear();
	}

	static inline void toggle()
	{
		port_t<PORT>::reg() ^= mask;
	}
};

struct NO_PORT;

template<> struct port_t<NO_PORT>
{
	typedef NO_PORT port;
};

typedef output_t<NO_PORT, 0> no_output_t;

struct PB;

#if defined(__AVR_ATmega328P__) || defined(__AVR_ATmega328__) || defined(__AVR_ATmega32U4__)
struct PC;
#endif

#if defined(__AVR_ATmega328P__) || defined(__AVR_ATmega328__) || defined(__AVR_ATmega32U4__)
struct PD;
#endif

#if defined(PE)
struct PE;
#endif

template<> struct port_t<PB>
{
	typedef PB port;
	static inline volatile uint8_t& ddr() { return DDRB; }
	static inline volatile uint8_t& reg() { return PORTB; }
	static inline volatile const uint8_t& pin() { return PINB; }
};

#if defined(__AVR_ATmega328P__) || defined(__AVR_ATmega328__) || defined(__AVR_ATmega32U4__)
template<> struct port_t<PC>
{
	typedef PC port;
	static inline volatile uint8_t& ddr() { return DDRC; }
	static inline volatile uint8_t& reg() { return PORTC; }
	static inline volatile const uint8_t& pin() { return PINC; }
};
#endif

#if defined(__AVR_ATmega328P__) || defined(__AVR_ATmega328__) || defined(__AVR_ATmega32U4__)
template<> struct port_t<PD>
{
	typedef PD port;
	static inline volatile uint8_t& ddr() { return DDRD; }
	static inline volatile uint8_t& reg() { return PORTD; }
	static inline volatile const uint8_t& pin() { return PIND; }
};
#endif

#if defined(PE)
template<> struct port_t<PE>
{
	typedef PE port;
	static inline volatile uint8_t& ddr() { return DDRE; }
	static inline volatile uint8_t& reg() { return PORTE; }
	static inline volatile const uint8_t& pin() { return PINE; }
};
#endif

// multi-bit stuff

template<int N, bool IS_NEGATIVE>
struct shift_impl { static uint8_t shift(uint8_t x) { return x << N; } };

template<int N>
struct shift_impl<N, true> { static uint8_t shift(uint8_t x) { return x >> -N; } };

template<int N> 
uint8_t shift(uint8_t x) { return shift_impl<N, N < 0>::shift(x); }

template
	< class T0
	, class T1 = no_output_t
	, class T2 = no_output_t
	, class T3 = no_output_t
	, class T4 = no_output_t
	, class T5 = no_output_t
	, class T6 = no_output_t
	, class T7 = no_output_t
>							// LSB to MSB order
struct bits_t
{
	typedef T0 BIT0;
	typedef T1 BIT1;
	typedef T2 BIT2;
	typedef T3 BIT3;
	typedef T4 BIT4;
	typedef T5 BIT5;
	typedef T6 BIT6;
	typedef T7 BIT7;
};

template<class PORT, class PINPORT, class PIN, int BIT>
struct map_bit_impl
{
	static uint8_t map_bit(uint8_t x)
	{
		return 0;
	}

	static const uint8_t mask = 0;
};

template<class PORT, class PIN, int BIT>
struct map_bit_impl<PORT, PORT, PIN, BIT>
{
	static uint8_t map_bit(uint8_t x)
	{
		return shift<PIN::bit - BIT>(static_cast<uint8_t>(x & (1 << BIT)));
	}

	static const uint8_t mask = PIN::mask;
};

template<class PORT, class BITS>
struct map_bits_impl
{
	static uint8_t map_bits(uint8_t x)
	{
		return map_bit_impl<PORT, typename BITS::BIT0::port, typename BITS::BIT0, 0>::map_bit(x)
			 | map_bit_impl<PORT, typename BITS::BIT1::port, typename BITS::BIT1, 1>::map_bit(x)
			 | map_bit_impl<PORT, typename BITS::BIT2::port, typename BITS::BIT2, 2>::map_bit(x)
			 | map_bit_impl<PORT, typename BITS::BIT3::port, typename BITS::BIT3, 3>::map_bit(x)
			 | map_bit_impl<PORT, typename BITS::BIT4::port, typename BITS::BIT4, 4>::map_bit(x)
			 | map_bit_impl<PORT, typename BITS::BIT5::port, typename BITS::BIT5, 5>::map_bit(x)
			 | map_bit_impl<PORT, typename BITS::BIT6::port, typename BITS::BIT6, 6>::map_bit(x)
			 | map_bit_impl<PORT, typename BITS::BIT7::port, typename BITS::BIT7, 7>::map_bit(x)
			 ;
	}

	static const uint8_t mask
		= map_bit_impl<PORT, typename BITS::BIT0::port, typename BITS::BIT0, 0>::mask
		| map_bit_impl<PORT, typename BITS::BIT1::port, typename BITS::BIT1, 1>::mask
		| map_bit_impl<PORT, typename BITS::BIT2::port, typename BITS::BIT2, 2>::mask
		| map_bit_impl<PORT, typename BITS::BIT3::port, typename BITS::BIT3, 3>::mask
		| map_bit_impl<PORT, typename BITS::BIT4::port, typename BITS::BIT4, 4>::mask
		| map_bit_impl<PORT, typename BITS::BIT5::port, typename BITS::BIT5, 5>::mask
		| map_bit_impl<PORT, typename BITS::BIT6::port, typename BITS::BIT6, 6>::mask
		| map_bit_impl<PORT, typename BITS::BIT7::port, typename BITS::BIT7, 7>::mask
	    ;
};

template<class PORT, class BITS, int MASK>
struct write_bits_impl
{
	static void write_bits(volatile uint8_t& reg, uint8_t x)
	{
		reg = map_bits_impl<PORT, BITS>::map_bits(x) | (reg & ~MASK);
	}
};

template<class PORT, class BITS>
struct write_bits_impl<PORT, BITS, 0>
{
	static void write_bits(volatile uint8_t& reg, uint8_t x)
	{
	}
};

template<class BITS>
static void write_bits(uint8_t bits)
{
	write_bits_impl<PB, BITS, map_bits_impl<PB, BITS>::mask>::write_bits(PORTB, bits);
	write_bits_impl<PC, BITS, map_bits_impl<PC, BITS>::mask>::write_bits(PORTC, bits);
	write_bits_impl<PD, BITS, map_bits_impl<PD, BITS>::mask>::write_bits(PORTD, bits);
//	write_bits_impl<PE, BITS, map_bits_impl<PE, BITS>::mask>::write_bits(PORTE, bits);
}

template<class BITS>
static void setup_bits()
{
	write_bits_impl<PB, BITS, map_bits_impl<PB, BITS>::mask>::write_bits(DDRB, 0xff);
	write_bits_impl<PC, BITS, map_bits_impl<PC, BITS>::mask>::write_bits(DDRC, 0xff);
	write_bits_impl<PD, BITS, map_bits_impl<PD, BITS>::mask>::write_bits(DDRD, 0xff);
//	write_bits_impl<PE, BITS, map_bits_impl<PE, BITS>::mask>::write_bits(DDRE, 0xff);
}

