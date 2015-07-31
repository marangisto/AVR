#include "../AVR/Pins.h"
#include "../AVR/Delay.h"
#define F_CPU 1000000L
#include <util/delay.h>
#include "../AVR/SN74HC595.h"

template<class DT, class CK, class LT>
class lcd1602a_t
{
public:
	static void setup()
	{
		sr::setup();
		_delay_ms(15);				// startup delay > 15ms
		send(0x3);
		_delay_ms(5);				// wait time > 4.1ms
		send(0x3);
		_delay_us(100);				// wait time > 100us
		send(0x3);					// function set
		send(0x2);					// 4-bit mode

		send(0x2);					// function set
		send(0x8);					// two-line mode

		send(0x0);					// items on/off
		send(0x8 | 0x4);			// 0x8, display = 0x4 | cursor = 0x2 | blink = 0x1

		send(0x0);					// entry mode
		send(0x0);					// 0x4, right direction = 0x2, shift display = 0x1

		clear();
	}

	static void write(uint8_t x)
	{
		send(0);
	}

	static void write(const char *p)
	{
		uint8_t i = 0;

		while (*p && i++ < 15)
			write_char(*p++);
	}

	static void write_char(char c)
	{
		send(RS | ((c >> 4) & 0xf));
		send(RS | (c & 0xf));
	}

	static void set_pos(uint8_t r, uint8_t c)
	{
		uint8_t w = (1 << 7) | (r ? 0x40 : 0) | (c & 0x1f);	// set ddram address + address
		send((w >> 4) & 0xf);
		send(w & 0xf);
	}

	static void cursor(bool on, bool blink)
	{
		send(0x0);					// items on/off
		send(0x8 | 0x4 | (on ? 0x2 : 0) | (blink ? 0x1 : 0));
	}

	static void clear()
	{
		send(0x0);					// display clear
		send(0x1);
		_delay_us(1700);			// wait time > 1.64ms
	}

private:
	typedef sn74hc595_t<DT, CK, LT, MSB_FIRST> sr;

	static const uint8_t RS = (1 << 7);
	static const uint8_t RW = (1 << 6);
	static const uint8_t E  = (1 << 5);

	static void send(uint8_t w)
	{
		sr::write(w | E);
		nop<1>();					// minimum pulse width 140ns
		sr::write(w & ~E);
		_delay_us(40);				// min cycle time is min op time is 37us
	}
};

typedef pin_t<PB, 3> LED;
typedef pin_t<PB, 0> CLOCK;
typedef pin_t<PB, 1> LATCH;
typedef pin_t<PB, 2> DATA;

typedef lcd1602a_t<DATA, CLOCK, LATCH> lcd;

static const char *fortunes[] =
	{ "You can never be sure how many beers you had last night."
	, "We have reason to be afraid. This is a terrible place."
	, "It is annoying to be honest to no purpose."
	, "Man belongs wherever he wants to go."
	, "Inspiration without perspiration is usually sterile."
	, "Beauty may be skin deep, but ugly goes clear to the bone."
	, "You're either part of the solution or part of the problem."
	};

static const uint16_t n_fortunes = sizeof(fortunes) / sizeof(*fortunes);

void setup()
{
	digital_out<LED>();
	lcd::setup();
}

void loop()
{
	static uint16_t i = 0;

	toggle<LED>();

	lcd::clear();
	lcd::cursor(i & 1, i & 2);
	lcd::set_pos(i & 1, i & 2);
	_delay_ms(1000);
	lcd::write(fortunes[i]);
	if (++i >= n_fortunes)
		i = 0;
	_delay_ms(1000);
}

int main()
{
	setup();
	for (;;)
		loop();
}

