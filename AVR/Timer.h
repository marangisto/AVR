#pragma once

#include <avr/interrupt.h>

template<int TNO>
struct timer_traits
{
};

template<>
struct timer_traits<0>
{
	typedef uint8_t count_t;

	static inline volatile uint8_t& tccra() { return TCCR0A; }
	static inline volatile uint8_t& tccrb() { return TCCR0B; }
	static inline volatile uint8_t& timsk() { return TIMSK0; }
	static inline volatile count_t& tcnt() { return TCNT0; }
	static const uint8_t cs0 = CS00;
	static const uint8_t cs1 = CS01;
	static const uint8_t cs2 = CS02;
	static const uint8_t toie = TOIE0;
};

template<>
struct timer_traits<1>
{
	typedef uint16_t count_t;

	static inline volatile uint8_t& tccra() { return TCCR1A; }
	static inline volatile uint8_t& tccrb() { return TCCR1B; }
	static inline volatile uint8_t& timsk() { return TIMSK1; }
	static inline volatile count_t& tcnt() { return TCNT1; }
	static const uint8_t cs0 = CS10;
	static const uint8_t cs1 = CS11;
	static const uint8_t cs2 = CS12;
	static const uint8_t toie = TOIE1;
};

template<>
struct timer_traits<2>
{
	typedef uint8_t count_t;

	static inline volatile uint8_t& tccra() { return TCCR2A; }
	static inline volatile uint8_t& tccrb() { return TCCR2B; }
	static inline volatile uint8_t& timsk() { return TIMSK2; }
	static inline volatile count_t& tcnt() { return TCNT2; }
	static const uint8_t cs0 = CS20;
	static const uint8_t cs1 = CS21;
	static const uint8_t cs2 = CS22;
	static const uint8_t toie = TOIE2;
};

template<int TNO>
struct timer_t
{
	typedef void (*isr_t)();

	enum prescale_t
		{ prescale_1 = 1
		, prescale_8 = 8
		, prescale_32 = 32
		, prescale_64 = 64
		, prescale_128 = 128
		, prescale_256 = 256
		, prescale_1024 = 1024
		};

	static void prescale(prescale_t s)
	{
		timer_traits<TNO>::tccra() = 0;

		// FIXME: enable correct bits for timer-2 and prevent them in the others!

		switch (s)
		{
			case prescale_1:
				timer_traits<TNO>::tccrb() |= (1 << timer_traits<TNO>::cs0);
				break;
			case prescale_8:
				timer_traits<TNO>::tccrb() |= (1 << timer_traits<TNO>::cs1);
				break;
			case prescale_64:
				timer_traits<TNO>::tccrb() |= (1 << timer_traits<TNO>::cs1) | (1 << timer_traits<TNO>::cs0);
				break;
			case prescale_256:
				timer_traits<TNO>::tccrb() |= (1 << timer_traits<TNO>::cs2);
				break;
			case prescale_1024:
				timer_traits<TNO>::tccrb() |= (1 << timer_traits<TNO>::cs2) | (1 << timer_traits<TNO>::cs0);
				break;
		}
	}

	static inline volatile typename timer_traits<TNO>::count_t& counter()
	{
		return timer_traits<TNO>::tcnt();
	}

	static void enable()
	{
		timer_traits<TNO>::timsk() |= (1 << timer_traits<TNO>::toie);	// enable timer overflow interrupt
	}

	static void disable()
	{
		timer_traits<TNO>::timsk() &= ~(1 << timer_traits<TNO>::toie);	// disable timer overflow interrupt
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

template<int TNO>
typename timer_t<TNO>::isr_t timer_t<TNO>::g_isr = timer_t<TNO>::dummy_isr;

ISR(TIMER0_OVF_vect)
{
	timer_t<0>::g_isr();
}

ISR(TIMER1_OVF_vect)
{
	timer_t<1>::g_isr();
}

ISR(TIMER2_OVF_vect)
{
	timer_t<2>::g_isr();
}

