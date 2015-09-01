#pragma once

#include <avr/interrupt.h>

struct timer0_t
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
		uint8_t tccr0a = 0, tccr0b = 0;

		switch (s)
		{
			case prescale_1:	tccr0b |= (1 << CS00);					break;
			case prescale_8:	tccr0b |= (1 << CS01);					break;
			case prescale_64:	tccr0b |= (1 << CS00) | (1 << CS01);	break;
			case prescale_256:	tccr0b |= (1 << CS02);					break;
			case prescale_1024:	tccr0b |= (1 << CS02) | (1 << CS00);	break;
		}

		TCCR0A = tccr0a;
		TCCR0B = tccr0b;
	}

	static inline volatile uint8_t& counter()
	{
		return TCNT0;
	}

	static void enable()
	{
		TIMSK0 |= (1 << TOIE0);     // enable timer overflow interrupt
	}

	static void disable()
	{
		TIMSK0 &= ~(1 << TOIE0);     // enable timer overflow interrupt
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

timer0_t::isr_t timer0_t::g_isr = timer0_t::dummy_isr;

ISR(TIMER0_OVF_vect)
{
	timer0_t::g_isr();
}

