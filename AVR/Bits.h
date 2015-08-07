#ifndef BITS_H
#define BITS_H

#include "Pins.h"

template<int N, bool IS_NEGATIVE>
struct shift_impl { static uint8_t shift(uint8_t x) { return x << N; } };

template<int N>
struct shift_impl<N, true> { static uint8_t shift(uint8_t x) { return x >> N; } };

template<int N> 
uint8_t shift(uint8_t x) { return shift_impl<N, N < 0>::shift(x); }

template<class T2, class T1, class T0>	// note natural bit ordering with LSB to the right
struct bits_t
{
	typedef T0 BIT0;
	typedef T1 BIT1;
	typedef T2 BIT2;
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
		return shift<(PIN::pin - BIT)>(static_cast<uint8_t>(x & (1 << BIT)));
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
//			 | map_bit_impl<PORT, typename BITS::BIT3::port, typename BITS::BIT3, 3>::map_bit(x)
//			 | map_bit_impl<PORT, typename BITS::BIT4::port, typename BITS::BIT4, 4>::map_bit(x)
//			 | map_bit_impl<PORT, typename BITS::BIT5::port, typename BITS::BIT5, 5>::map_bit(x)
//			 | map_bit_impl<PORT, typename BITS::BIT6::port, typename BITS::BIT6, 6>::map_bit(x)
//			 | map_bit_impl<PORT, typename BITS::BIT7::port, typename BITS::BIT7, 7>::map_bit(x)
			 ;
	}

	static const uint8_t mask
		= map_bit_impl<PORT, typename BITS::BIT0::port, typename BITS::BIT0, 0>::mask
		| map_bit_impl<PORT, typename BITS::BIT1::port, typename BITS::BIT1, 1>::mask
		| map_bit_impl<PORT, typename BITS::BIT2::port, typename BITS::BIT2, 2>::mask
//		| map_bit_impl<PORT, typename BITS::BIT3::port, typename BITS::BIT3, 3>::mask
//		| map_bit_impl<PORT, typename BITS::BIT4::port, typename BITS::BIT4, 4>::mask
//		| map_bit_impl<PORT, typename BITS::BIT5::port, typename BITS::BIT5, 5>::mask
//		| map_bit_impl<PORT, typename BITS::BIT6::port, typename BITS::BIT6, 6>::mask
//		| map_bit_impl<PORT, typename BITS::BIT7::port, typename BITS::BIT7, 7>::mask
//		| map_bit_impl<PORT, typename BITS::BIT8::port, typename BITS::BIT8, 8>::mask
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

	// FIXME: introduce NOPIN to extend bits template to all pins
}

#endif

