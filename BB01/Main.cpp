#include <avr/io.h>
#include <avr/interrupt.h>
#include "../AVR/Pins.h"

template<class T> T min(const T& x, const T& y) { return x < y ? x : y; }
template<class T> T max(const T& x, const T& y) { return x > y ? x : y; }

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

void delay_loop_2(uint16_t __count)
{
	__asm__ volatile
	(
		"1: sbiw %0,1" "\n\t"
		"brne 1b"
			: "=w" (__count)
			: "0" (__count)
	);
}

void delay(uint16_t n)
{
	while (n-- > 0)
		delay_loop_2(4000);
}

typedef bool dir_t;
static const bool Left = false;
static const bool Right = true;

typedef pin_t<PD,0> DIR;
typedef pin_t<PD,1> STEP;
typedef pin_t<PD,2> btnDn;
typedef pin_t<PD,3> btnUp;

enum prescale_t { prescale_1 = 1, prescale_8 = 8, prescale_64 = 64, prescale_256 = 256, prescale_1024 = 1024 };

static void timer1_config(prescale_t s)
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

static void timer1_enable()
{
	TIMSK1 |= (1 << TOIE1);     // enable timer overflow interrupt
}

static volatile uint16_t n_steps = 0;    // tell isr how many steps to run
static volatile uint16_t n_accel = 0;	// max acceleration steps
static volatile uint16_t cur_step = 0;   // current isr step
static volatile uint16_t max_i = 0;        // max speed
static volatile uint8_t limit_mask = 0;
static volatile uint16_t half_time_step = 10000;	// half-step duration
static volatile bool inflight = false;
static volatile uint16_t step_i = 0;
static volatile uint16_t dt = 0;
static const uint16_t micro_steps = 0;  // micro-steps shifts

static inline uint16_t eq12(uint16_t c, uint16_t n, bool acc)
{
    uint16_t k = (c << 1) / ((n << 2) + 1);
    return acc ? (c - k) : (c + k);
}

ISR(TIMER1_OVF_vect)
{
	if (step_i < n_steps)
	{
		set<STEP>();			// 2 cycles
		nop<14>();				// need 1us so total 16 cycles
		clear<STEP>();
		TCNT1 = 65535 - (dt << 1);

        if (++step_i < n_accel)  // still time to accelerate
        {
            uint16_t dt_ = eq12(dt, step_i, true);
            if (dt_ == dt)
                n_accel = step_i;      // actual acceleration steps
            else
                dt = dt_;
        }
        else if (step_i + n_accel > n_steps) // time to decelerate
            dt = eq12(dt, n_steps - step_i, false);
	}
	else
		inflight = false;
}

void speedTest(dir_t dir, uint16_t n, uint16_t c)
{
	cli();                     // disable global interrupts
	inflight = true;
	n_steps = n;
	n_accel = n >> 1;;		// max number of acceleration steps
	step_i = 0;
	dt = c;
	write<DIR>(dir == Left);
	timer1_config(prescale_8);
	timer1_enable();
	sei();
	while (inflight)
		delay(1);
}

void setup()
{
	digital_in<btnDn, btnUp>();
	set<btnDn, btnUp>(); 			// pull-ups
	digital_out<DIR, STEP>();

//	homePosition();
}

void loop()
{
	static bool dir = false;
//	delay(100);
//	speedTest(dir, 2000, 15000);	// conservative limit
//	speedTest(dir, 100, 5000);		// good short run
//	speedTest(dir, 100, 3000);		// fantastic!
	speedTest(dir, 100, 1500);		// still works - we need to explore the envelope
	dir = !dir;
}

int main()
{
	setup();
	for (;;)
		loop();
}

