#pragma once

#include <avr/interrupt.h>

template<int TNO>
struct timer_traits
{
};

template<int TNO, int PRESCALE> struct clock_source_traits {};

template<>
struct timer_traits<0>
{
	typedef uint8_t count_t;

	static inline volatile uint8_t& tccra() { return TCCR0A; }
	static inline volatile uint8_t& tccrb() { return TCCR0B; }
	static inline volatile uint8_t& timsk() { return TIMSK0; }
	static inline volatile count_t& ocra() { return OCR0A; }
	static inline volatile count_t& ocrb() { return OCR0B; }
	static inline volatile count_t& tcnt() { return TCNT0; }
	static const uint8_t cs0 = CS00;
	static const uint8_t cs1 = CS01;
	static const uint8_t cs2 = CS02;
	static const uint8_t toie = TOIE0;
    static const uint8_t coma0 = COM0A0;
    static const uint8_t coma1 = COM0A1;
    static const uint8_t comb0 = COM0B0;
    static const uint8_t comb1 = COM0B1;
    static const uint8_t wgm0 = WGM00;
    static const uint8_t wgm1 = WGM01;
    static const uint8_t wgm2 = WGM02;
    static const uint8_t clock_source_mask = _BV(CS02) | _BV(CS01) | _BV(CS00);
};

//template<> struct clock_source_traits<0, 1> { static const uint8_t bits = _BV(CS00); };
template<> struct clock_source_traits<0, 1> { static inline uint8_t bits() { return _BV(CS00); } };
template<> struct clock_source_traits<0, 8> { static inline uint8_t bits() { return _BV(CS01); } };
template<> struct clock_source_traits<0, 64> { static inline uint8_t bits() { return _BV(CS01) | _BV(CS00); } };
template<> struct clock_source_traits<0, 256> { static inline uint8_t bits() { return _BV(CS02); } };
template<> struct clock_source_traits<0, 1024> { static inline uint8_t bits() { return _BV(CS02) | _BV(CS00); } };

template<>
struct timer_traits<1>
{
	typedef uint16_t count_t;

	static inline volatile uint8_t& tccra() { return TCCR1A; }
	static inline volatile uint8_t& tccrb() { return TCCR1B; }
	static inline volatile uint8_t& timsk() { return TIMSK1; }
	static inline volatile count_t& ocra() { return OCR1A; }
	static inline volatile count_t& ocrb() { return OCR1B; }
	static inline volatile count_t& tcnt() { return TCNT1; }
	static const uint8_t cs0 = CS10;
	static const uint8_t cs1 = CS11;
	static const uint8_t cs2 = CS12;
	static const uint8_t toie = TOIE1;
    static const uint8_t coma0 = COM1A0;
    static const uint8_t coma1 = COM1A1;
    static const uint8_t comb0 = COM1B0;
    static const uint8_t comb1 = COM1B1;
    static const uint8_t wgm0 = WGM10;
    static const uint8_t wgm1 = WGM11;
    static const uint8_t wgm2 = WGM12;
};

template<> struct clock_source_traits<1, 1> { static inline uint8_t bits() { return _BV(CS10); } };
template<> struct clock_source_traits<1, 8> { static inline uint8_t bits() { return _BV(CS11); } };
template<> struct clock_source_traits<1, 64> { static inline uint8_t bits() { return _BV(CS11) | _BV(CS10); } };
template<> struct clock_source_traits<1, 256> { static inline uint8_t bits() { return _BV(CS12); } };
template<> struct clock_source_traits<1, 1024> { static inline uint8_t bits() { return _BV(CS12) | _BV(CS10); } };

template<>
struct timer_traits<2>
{
	typedef uint8_t count_t;

	static inline volatile uint8_t& tccra() { return TCCR2A; }
	static inline volatile uint8_t& tccrb() { return TCCR2B; }
	static inline volatile uint8_t& timsk() { return TIMSK2; }
	static inline volatile count_t& ocra() { return OCR2A; }
	static inline volatile count_t& ocrb() { return OCR2B; }
	static inline volatile count_t& tcnt() { return TCNT2; }
	static const uint8_t cs0 = CS20;
	static const uint8_t cs1 = CS21;
	static const uint8_t cs2 = CS22;
	static const uint8_t toie = TOIE2;
    static const uint8_t coma0 = COM2A0;
    static const uint8_t coma1 = COM2A1;
    static const uint8_t comb0 = COM2B0;
    static const uint8_t comb1 = COM2B1;
    static const uint8_t wgm0 = WGM20;
    static const uint8_t wgm1 = WGM21;
    static const uint8_t wgm2 = WGM22;
};

template<> struct clock_source_traits<2, 1> { static inline uint8_t bits() { return _BV(CS20); } };
template<> struct clock_source_traits<2, 8> { static inline uint8_t bits() { return _BV(CS21); } };
template<> struct clock_source_traits<2, 32> { static inline uint8_t bits() { return _BV(CS21) | _BV(CS20); } };
template<> struct clock_source_traits<2, 64> { static inline uint8_t bits() { return _BV(CS22); } };
template<> struct clock_source_traits<2, 128> { static inline uint8_t bits() { return _BV(CS22) | _BV(CS20); } };
template<> struct clock_source_traits<2, 256> { static inline uint8_t bits() { return _BV(CS22) | _BV(CS21); } };
template<> struct clock_source_traits<2, 1024> { static inline uint8_t bits() { return _BV(CS22) | _BV(CS21) | _BV(CS20); } };

template<int TNO>
struct timer_t
{
	typedef void (*isr_t)();

	enum prescale_t
		{ prescale_1 = 1
		, prescale_8 = 8
//		, prescale_32 = 32
		, prescale_64 = 64
//		, prescale_128 = 128
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
				timer_traits<TNO>::tccrb() |= _BV(timer_traits<TNO>::cs0);
				break;
			case prescale_8:
				timer_traits<TNO>::tccrb() |= _BV(timer_traits<TNO>::cs1);
				break;
			case prescale_64:
				timer_traits<TNO>::tccrb() |= _BV(timer_traits<TNO>::cs1) | _BV(timer_traits<TNO>::cs0);
				break;
			case prescale_256:
				timer_traits<TNO>::tccrb() |= _BV(timer_traits<TNO>::cs2);
				break;
			case prescale_1024:
				timer_traits<TNO>::tccrb() |= _BV(timer_traits<TNO>::cs2) | _BV(timer_traits<TNO>::cs0);
				break;
		}
	}

	static inline volatile typename timer_traits<TNO>::count_t& ocra()
	{
		return timer_traits<TNO>::ocra();
	}

	static inline volatile typename timer_traits<TNO>::count_t& ocrb()
	{
		return timer_traits<TNO>::ocrb();
	}

	static inline volatile typename timer_traits<TNO>::count_t& counter()
	{
		return timer_traits<TNO>::tcnt();
	}

    static void pwma()
    {
		timer_traits<TNO>::tccra() |= _BV(timer_traits<TNO>::coma1)	    // clear on match, set at bottom
		                            | _BV(timer_traits<TNO>::wgm1)	    // fast pwm mode
		                            | _BV(timer_traits<TNO>::wgm0);	    // fast pwm mode
    }

	static void enable()
	{
		timer_traits<TNO>::timsk() |= _BV(timer_traits<TNO>::toie);		// enable timer overflow interrupt
	}

	static void disable()
	{
		timer_traits<TNO>::timsk() &= ~_BV(timer_traits<TNO>::toie);	// disable timer overflow interrupt
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

