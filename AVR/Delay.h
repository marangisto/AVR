#ifndef DELAY_H
#define DELAY_H

#include <util/delay_basic.h>

template<int N>
struct no_operation
{
	static inline void run()
	{
		__asm__ volatile("nop");
		::no_operation<N-1>::run();
	}
};

template<>
struct no_operation<0>
{
	static inline void run()
	{
	}
};

template<int N>
static inline void nop()
{
	no_operation<N>::run();
}

void delay(uint16_t n)
{
	while (n-- > 0)
		_delay_loop_2(4000);	// FIXME: use controller frequency!
}

#endif DELAY_H

