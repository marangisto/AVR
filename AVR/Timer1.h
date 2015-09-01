#pragma once

#include <avr/interrupt.h>

struct timer1_t
{
	typedef void (*isr_t)();

	enum prescale_t
		{ prescale_1 = 1
		, prescale_8 = 8
		, prescale_64 = 64
		, prescale_256 = 256
		, prescale_1024 = 1024
		};

	static void prescale(prescale_t s)
	{
		uint8_t tccr1a = 0, tccr1b = 0;

		switch (s)
		{
			case prescale_1:	tccr1b |= (1 << CS10);					break;
			case prescale_8:	tccr1b |= (1 << CS11);					break;
			case prescale_64:	tccr1b |= (1 << CS10) | (1 << CS11);	break;
			case prescale_256:	tccr1b |= (1 << CS12);					break;
			case prescale_1024:	tccr1b |= (1 << CS12) | (1 << CS10);	break;
		}

		TCCR1A = tccr1a;
		TCCR1B = tccr1b;
	}

	static inline volatile uint16_t& counter()
	{
		return TCNT1;
	}

	static void enable()
	{
		TIMSK1 |= (1 << TOIE1);     // enable timer overflow interrupt
	}

	static void disable()
	{
		TIMSK1 &= ~(1 << TOIE1);     // enable timer overflow interrupt
	}

	static void isr(isr_t f)
	{
		g_isr = f;
	}

	static void dummy_isr()
	{
	}

	static isr_t g_isr;
};

timer1_t::isr_t timer1_t::g_isr = timer1_t::dummy_isr;

ISR(TIMER1_OVF_vect)
{
	timer1_t::g_isr();
}

