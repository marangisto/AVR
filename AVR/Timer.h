#pragma once

#include <avr/interrupt.h>

template<int TNO>
struct timer_traits
{
};

enum wg_mode { normal_mode, ctc_mode, fast_pwm, pwm_phase_correct, pwm_phase_frequency_correct };
enum wg_top { top_default, top_0xff, top_0x1ff, top_0x3ff, top_ocra, top_icr };

template<int TNO, wg_mode MODE, wg_top TOP> struct waveform_generator_traits {};

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
	static const uint8_t toie = TOIE0;
    static const uint8_t coma0 = COM0A0;
    static const uint8_t coma1 = COM0A1;
    static const uint8_t comb0 = COM0B0;
    static const uint8_t comb1 = COM0B1;
    static const uint8_t clock_source_mask = _BV(CS02) | _BV(CS01) | _BV(CS00);
};

template<> struct waveform_generator_traits<0, normal_mode, top_default>
{ 
    static const uint8_t bitsa = 0;
    static const uint8_t bitsb = 0;
};

template<> struct waveform_generator_traits<0, pwm_phase_correct, top_0xff>
{ 
    static const uint8_t bitsa = _BV(WGM00);
    static const uint8_t bitsb = 0;
};

template<> struct waveform_generator_traits<0, ctc_mode, top_ocra>
{ 
    static const uint8_t bitsa = _BV(WGM01);
    static const uint8_t bitsb = 0;
};

template<> struct waveform_generator_traits<0, fast_pwm, top_0xff>
{ 
    static const uint8_t bitsa = _BV(WGM01) | _BV(WGM00);
    static const uint8_t bitsb = 0;
};

template<> struct waveform_generator_traits<0, pwm_phase_correct, top_ocra>
{ 
    static const uint8_t bitsa = _BV(WGM00);
    static const uint8_t bitsb = _BV(WGM02);
};

template<> struct waveform_generator_traits<0, fast_pwm, top_ocra>
{ 
    static const uint8_t bitsa = _BV(WGM01) | _BV(WGM00);
    static const uint8_t bitsb = _BV(WGM02);
};

template<> struct clock_source_traits<0, 1> { static const uint8_t bits = _BV(CS00); };
template<> struct clock_source_traits<0, 8> { static const uint8_t bits = _BV(CS01); };
template<> struct clock_source_traits<0, 64> { static const uint8_t bits = _BV(CS01) | _BV(CS00); };
template<> struct clock_source_traits<0, 256> { static const uint8_t bits = _BV(CS02); };
template<> struct clock_source_traits<0, 1024> { static const uint8_t bits = _BV(CS02) | _BV(CS00); };

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
	static const uint8_t toie = TOIE1;
    static const uint8_t coma0 = COM1A0;
    static const uint8_t coma1 = COM1A1;
    static const uint8_t comb0 = COM1B0;
    static const uint8_t comb1 = COM1B1;
    static const uint8_t clock_source_mask = _BV(CS12) | _BV(CS11) | _BV(CS10);
};

template<> struct waveform_generator_traits<1, normal_mode, top_default>
{ 
    static const uint8_t bitsa = 0;
    static const uint8_t bitsb = 0;
};

template<> struct waveform_generator_traits<1, pwm_phase_correct, top_0xff>
{ 
    static const uint8_t bitsa = _BV(WGM10);
    static const uint8_t bitsb = 0;
};

template<> struct waveform_generator_traits<1, pwm_phase_correct, top_0x1ff>
{ 
    static const uint8_t bitsa = _BV(WGM11);
    static const uint8_t bitsb = 0;
};

template<> struct waveform_generator_traits<1, pwm_phase_correct, top_0x3ff>
{ 
    static const uint8_t bitsa = _BV(WGM11) | _BV(WGM10);
    static const uint8_t bitsb = 0;
};

template<> struct waveform_generator_traits<1, ctc_mode, top_ocra>
{ 
    static const uint8_t bitsa = 0;
    static const uint8_t bitsb = _BV(WGM12);
};

template<> struct waveform_generator_traits<1, fast_pwm, top_0xff>
{ 
    static const uint8_t bitsa = _BV(WGM10);
    static const uint8_t bitsb = _BV(WGM12);
};

template<> struct waveform_generator_traits<1, fast_pwm, top_0x1ff>
{ 
    static const uint8_t bitsa = _BV(WGM11);
    static const uint8_t bitsb = _BV(WGM12);
};

template<> struct waveform_generator_traits<1, fast_pwm, top_0x3ff>
{ 
    static const uint8_t bitsa = _BV(WGM11) | _BV(WGM10);
    static const uint8_t bitsb = _BV(WGM12);
};

template<> struct waveform_generator_traits<1, pwm_phase_frequency_correct, top_icr>
{ 
    static const uint8_t bitsa = 0;
    static const uint8_t bitsb = _BV(WGM13);
};

template<> struct waveform_generator_traits<1, pwm_phase_frequency_correct, top_ocra>
{ 
    static const uint8_t bitsa = _BV(WGM10);
    static const uint8_t bitsb = _BV(WGM13);
};

template<> struct waveform_generator_traits<1, pwm_phase_correct, top_icr>
{ 
    static const uint8_t bitsa = _BV(WGM11);
    static const uint8_t bitsb = _BV(WGM13);
};

template<> struct waveform_generator_traits<1, pwm_phase_correct, top_ocra>
{ 
    static const uint8_t bitsa = _BV(WGM11) | _BV(WGM10);
    static const uint8_t bitsb = _BV(WGM13);
};

template<> struct waveform_generator_traits<1, ctc_mode, top_icr>
{ 
    static const uint8_t bitsa = 0;
    static const uint8_t bitsb = _BV(WGM13) | _BV(WGM12);
};

template<> struct waveform_generator_traits<1, fast_pwm, top_icr>
{ 
    static const uint8_t bitsa = _BV(WGM11);
    static const uint8_t bitsb = _BV(WGM13) | _BV(WGM12);
};

template<> struct waveform_generator_traits<1, fast_pwm, top_ocra>
{ 
    static const uint8_t bitsa = _BV(WGM11) | _BV(WGM10);
    static const uint8_t bitsb = _BV(WGM13) | _BV(WGM12);
};

template<> struct clock_source_traits<1, 1> { static const uint8_t bits = _BV(CS10); };
template<> struct clock_source_traits<1, 8> { static const uint8_t bits = _BV(CS11); };
template<> struct clock_source_traits<1, 64> { static const uint8_t bits = _BV(CS11) | _BV(CS10); };
template<> struct clock_source_traits<1, 256> { static const uint8_t bits = _BV(CS12); };
template<> struct clock_source_traits<1, 1024> { static const uint8_t bits = _BV(CS12) | _BV(CS10); };

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
	static const uint8_t toie = TOIE2;
    static const uint8_t coma0 = COM2A0;
    static const uint8_t coma1 = COM2A1;
    static const uint8_t comb0 = COM2B0;
    static const uint8_t comb1 = COM2B1;
    static const uint8_t clock_source_mask = _BV(CS22) | _BV(CS21) | _BV(CS20);
};

template<> struct waveform_generator_traits<2, normal_mode, top_default>
{ 
    static const uint8_t bitsa = 0;
    static const uint8_t bitsb = 0;
};

template<> struct waveform_generator_traits<2, pwm_phase_correct, top_0xff>
{ 
    static const uint8_t bitsa = _BV(WGM20);
    static const uint8_t bitsb = 0;
};

template<> struct waveform_generator_traits<2, ctc_mode, top_ocra>
{ 
    static const uint8_t bitsa = _BV(WGM21);
    static const uint8_t bitsb = 0;
};

template<> struct waveform_generator_traits<2, fast_pwm, top_0xff>
{ 
    static const uint8_t bitsa = _BV(WGM21) | _BV(WGM20);
    static const uint8_t bitsb = 0;
};

template<> struct waveform_generator_traits<2, pwm_phase_correct, top_ocra>
{ 
    static const uint8_t bitsa = _BV(WGM20);
    static const uint8_t bitsb = _BV(WGM22);
};

template<> struct waveform_generator_traits<2, fast_pwm, top_ocra>
{ 
    static const uint8_t bitsa = _BV(WGM21) | _BV(WGM20);
    static const uint8_t bitsb = _BV(WGM22);
};

template<> struct clock_source_traits<2, 1> { static const uint8_t bits = _BV(CS20); };
template<> struct clock_source_traits<2, 8> { static const uint8_t bits = _BV(CS21); };
template<> struct clock_source_traits<2, 32> { static const uint8_t bits = _BV(CS21) | _BV(CS20); };
template<> struct clock_source_traits<2, 64> { static const uint8_t bits = _BV(CS22); };
template<> struct clock_source_traits<2, 128> { static const uint8_t bits = _BV(CS22) | _BV(CS20); };
template<> struct clock_source_traits<2, 256> { static const uint8_t bits = _BV(CS22) | _BV(CS21); };
template<> struct clock_source_traits<2, 1024> { static const uint8_t bits = _BV(CS22) | _BV(CS21) | _BV(CS20); };

template<int TNO>
struct timer_t
{
	typedef void (*isr_t)();

    template<wg_mode MODE, wg_top TOP>
    static inline void setup()
    {
		timer_traits<TNO>::tccra() = waveform_generator_traits<TNO, MODE, TOP>::bitsa;
		timer_traits<TNO>::tccrb() = waveform_generator_traits<TNO, MODE, TOP>::bitsb;
    }

    template<int PRESCALE>
    static inline void start()
    {
		timer_traits<TNO>::tccrb() = (timer_traits<TNO>::tccrb() & ~timer_traits<TNO>::clock_source_mask)
                                   | clock_source_traits<TNO, PRESCALE>::bits
                                   ;
    }

    static inline void stop()
    {
		timer_traits<TNO>::tccrb() = timer_traits<TNO>::tccrb() & ~timer_traits<TNO>::clock_source_mask;
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
		timer_traits<TNO>::tccra() |= _BV(timer_traits<TNO>::coma1);    // clear on match, set at bottom
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

