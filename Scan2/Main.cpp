#include "../AVR/Pins.h"
#include "../AVR/Delay.h"
#include "../AVR/Timer1.h"
#include "../AVR/LCD1602A.h"
#include "Buttons.h"
#include "A4988.h"
#include <stdlib.h>

typedef pin_t<PC, 3> CLOCK;
typedef pin_t<PC, 4> LATCH;
typedef pin_t<PC, 5> DATA;

typedef lcd1602a_t<DATA, CLOCK, LATCH> lcd;

typedef analog_input_t<2> A2;

typedef buttons_t<A2> btns;

typedef pin_t<PB, 1> DIR;
typedef pin_t<PB, 2> STEP;
typedef pin_t<PD, 4> RESET;
typedef pin_t<PD, 5> MS3;
typedef pin_t<PD, 6> MS2;
typedef pin_t<PD, 7> MS1;
typedef pin_t<PB, 0> ENABLE;

typedef a4988_t<DIR, STEP, RESET, MS1, MS2, MS3, ENABLE> a4988;

static volatile uint16_t n_steps = 0;   // tell isr how many steps to run
static volatile uint16_t n_accel = 0;	// max acceleration steps
static volatile bool inflight = false;	// we are running stepper
static volatile uint16_t step_i = 0;	// current step counter
static volatile uint16_t step_t = 0;		// step length in timer cycles

static inline uint16_t eq12(uint16_t c, uint16_t n, bool acc)
{
    uint16_t k = (c << 1) / ((n << 2) + 1);
    return acc ? (c - k) : (c + k);
}

static void isr()
{
	if (step_i < n_steps)
	{
		a4988::step();
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

void run_stepper(bool dir, uint16_t n, uint16_t c)
{
	cli();					// disable global interrupts
	inflight = true;
	n_steps = n;
	n_accel = n >> 1;;		// max number of acceleration steps
	step_i = 0;
	step_t = c;
	a4988::dir(dir);
	a4988::reset();
	a4988::enable();
	timer1_t::enable();
	sei();
	while (inflight)
		delay_ms(1);
	a4988::disable();
}


void setup()
{
	lcd::setup();
	btns::setup();
	a4988::setup();
	timer1_t::prescale(timer1_t::prescale_8);
	timer1_t::isr(isr);
}

void loop()
{
	static int i = 0, j = 0;

	uint8_t x = btns::read();

	static bool d = false;
	static uint16_t c = 16000;
	static a4988::micro_step_t ms = a4988::full_step;

	switch (x)
	{
		case 1: 
			for (uint8_t j = 0; j < 10; ++j)
			{
				d = (random() & 1) != 0;
				run_stepper(d, 200 << a4988::micro_shift(ms), c >> a4988::micro_shift(ms));
				delay_ms(200);
			}
			break;
		case 2: d = !d; a4988::dir(d); break;
		case 3:
			{
				uint16_t n = 200 << a4988::micro_shift(ms);

				a4988::enable();

				for (uint16_t i = 0; i < n; ++i)
				{
					delay_us(1000);
					a4988::step();
				}

				a4988::disable();
			}
			break;
		case 4:
			switch (ms)
			{
			case a4988::full_step: ms = a4988::half_step; break;
			case a4988::half_step: ms = a4988::quarter_step; break;
			case a4988::quarter_step: ms = a4988::eigth_step; break;
			case a4988::eigth_step: ms = a4988::sixteenth_step; break;
			case a4988::sixteenth_step: ms = a4988::full_step; break;
			}
			a4988::micro_step(ms);
			lcd::set_pos(1, 0);
			lcd::write(a4988::to_string(ms));
			break;
		case 5: a4988::reset(); break;
		default: ;
	}

	lcd::set_pos(0, 0);
	lcd::write(i);
	lcd::write("            ");

	if (++j > 200)
	{
		double y = rand() / 32768.0;
		static double z = 0;

		lcd::set_pos(1, 8);
		lcd::write_e(y - z, 1);
		j = 0;
		z = y;
	}

	delay_ms(1);
}


int main()
{
	setup();
	for (;;)
		loop();
}

