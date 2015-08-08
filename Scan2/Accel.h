#ifndef ACCEL_H
#define ACCEL_H

#include "../AVR/Delay.h"
#include "../AVR/Timer1.h"
#include "A4988.h"

template<class A4988>
class accel_t
{
public:
	static void setup()
	{
		A4988::setup();
		timer1_t::prescale(timer1_t::prescale_8);
		timer1_t::isr(isr);
	}

	static void run(bool dir, uint16_t n, uint16_t c)
	{
		cli();					// disable global interrupts
		inflight = true;
		n_steps = n;
		n_accel = n >> 1;;		// max number of acceleration steps
		step_i = 0;
		step_t = c;
		A4988::dir(dir);
		A4988::reset();
		A4988::enable();
		timer1_t::enable();
		sei();
		while (inflight)
			delay_ms(1);
		A4988::disable();
	}

private:
	static inline uint16_t eq12(uint16_t c, uint16_t n, bool acc)
	{
    	uint16_t k = (c << 1) / ((n << 2) + 1);
    	return acc ? (c - k) : (c + k);
	}

	static void isr()
	{
		if (step_i < n_steps)
		{
			A4988::step();
			timer1_t::counter() = 65535 - step_t;

        	if (++step_i < n_accel)  // still time to accelerate
        	{
            	uint16_t step_t_ = eq12(step_t, step_i, true);
            	if (step_t_ == step_t)
                	n_accel = step_i;      // actual acceleration steps
            	else
                	step_t = step_t_;
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
	static volatile bool inflight;		// we are running stepper
};


template<class A4988> volatile uint16_t accel_t<A4988>::n_steps = 0;		// tell isr how many steps to run
template<class A4988> volatile uint16_t accel_t<A4988>::n_accel = 0;		// max acceleration steps
template<class A4988> volatile uint16_t accel_t<A4988>::step_i = 0;		// current step counter
template<class A4988> volatile uint16_t accel_t<A4988>::step_t = 0;		// step length in timer cycles
template<class A4988> volatile bool accel_t<A4988>::inflight = false;	// we are running stepper

#endif // ACCEL_H
