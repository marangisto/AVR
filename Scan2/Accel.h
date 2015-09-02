#ifndef ACCEL_H
#define ACCEL_H

#include "../AVR/Delay.h"
#include "../AVR/Timer.h"
#include "A4988.h"

template<class A4988, class LIML, class LIMR>
class accel_t
{
public:
	static void setup()
	{
		A4988::setup();
		timer::prescale(timer::prescale_8);
	}

	static void run(bool dir, uint16_t n, uint16_t c, micro_step_t::e ms)
	{
		cli();					// disable global interrupts
		inflight = true;
		n_steps = n << micro_step_t::shift(ms);
		n_accel = n_steps >> 1;;		// max number of acceleration steps
		step_i = 0;
		step_t = c;
		A4988::dir(dir);
		A4988::reset();
		A4988::micro_step(ms);
		A4988::enable();
		timer::isr(accel_isr);
		timer::enable();
		sei();
		while (inflight)
			delay_ms(1);
		A4988::disable();
		timer::disable();
		timer::isr(timer::dummy_isr);
	}

	static uint16_t min_step() { return min_t; }

	static int16_t calibrate()
	{
		micro_step_t::e ms = micro_step_t::quarter_step;
		const uint8_t nms = 1 << micro_step_t::shift(ms);
		uint8_t t = 1;				// millisecs per (micro-) step
		A4988::dir(true);			// reverse
		A4988::reset();
		A4988::micro_step(ms);
		A4988::enable();

		int16_t n_steps = 0;

		while (!read<LIML>())
		{
			for (uint8_t s = 0; s < nms; ++s)
			{
				A4988::step();
				delay_ms(t);
			}

			--n_steps;
		}

		delay_ms(100);

		A4988::dir(false);

		t = 15;				// millisecs per (micro-) step

		while (read<LIML>())
		{
			for (uint8_t s = 0; s < nms; ++s)
			{
				A4988::step();
				delay_ms(t);
			}

			++n_steps;
		}

		A4988::disable();

		return n_steps;
	}

private:
	typedef timer_t<1> timer;

	static inline uint16_t eq12(uint16_t c, uint16_t n, bool acc)
	{
    	uint16_t k = (c << 1) / ((n << 2) + 1);
    	return acc ? (c - k) : (c + k);
	}

	static void accel_isr()
	{
		if (step_i < n_steps)
		{
			if (read<LIML>() || read<LIMR>())
			{
				inflight = false;
				return;
			}

			A4988::step();
			timer::counter() = 65535 - step_t;

        	if (++step_i < n_accel)  // still time to accelerate
        	{
            	uint16_t step_t_ = eq12(step_t, step_i, true);
            	if (step_t_ == step_t)
                	n_accel = step_i;	// actual acceleration steps
            	else
                	step_t = step_t_;
				min_t = step_t;		// record minimum step length
        	}
        	else if (step_i + n_accel > n_steps) // time to decelerate
            	step_t = eq12(step_t, n_steps - step_i, false);
		}
		else
			inflight = false;
	}

	static volatile uint16_t n_steps;   // tell isr how many steps to run
	static volatile uint16_t n_accel;	// max acceleration steps
	static volatile uint16_t step_i;	// current step counter
	static volatile uint16_t step_t;	// step length in timer cycles
	static volatile uint16_t min_t;		// minimum step length achieved
	static volatile bool inflight;		// we are running stepper
};


template<class T0, class T1, class T2> volatile uint16_t accel_t<T0, T1, T2>::n_steps = 0;		// tell isr how many steps to run
template<class T0, class T1, class T2> volatile uint16_t accel_t<T0, T1, T2>::n_accel = 0;		// max acceleration steps
template<class T0, class T1, class T2> volatile uint16_t accel_t<T0, T1, T2>::step_i = 0;		// current step counter
template<class T0, class T1, class T2> volatile uint16_t accel_t<T0, T1, T2>::step_t = 0;		// step length in timer cycles
template<class T0, class T1, class T2> volatile uint16_t accel_t<T0, T1, T2>::min_t = 0;		// minimum step length achieved
template<class T0, class T1, class T2> volatile bool accel_t<T0, T1, T2>::inflight = false;	// we are running stepper

#endif // ACCEL_H

