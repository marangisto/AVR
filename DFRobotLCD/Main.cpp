#include "Uno.h"
#include <AVR/Delay.h>
#include <stdlib.h>

typedef D13 LED;

class DFR0009
{
public:
	static void setup()
	{
		static uint8_t data[] =
			{ 0x3					// sync
			, 0x3					// sync
			, 0x3					// function set
			, 0x2					// 4-bit mode
			, 0x2					// function set
			, 0x8					// two-line mode
			, 0x0					// items on/off
			, 0x8 | 0x4				// 0x8, display = 0x4 | cursor = 0x2 | blink = 0x1
			, 0x0					// entry mode
			, 0x0					// 0x4, right direction = 0x2, shift display = 0x1
			};

		tc1602::setup();

		const uint8_t *p = data;

		delay_ms(15);				// startup delay > 15ms
		send(*p++);
		delay_ms(5);				// wait time > 4.1ms
		send(*p++);
		delay_us(100);				// wait time > 100us

		while (p < data + sizeof(data) / sizeof(*data))
			send(*p++);

		clear();
	}

	static void write(int x, int radix = 10)
	{
		write(itoa(x, buf, 10));
	}

	static void write(unsigned x, int radix = 10)
	{
		write(utoa(x, buf, 10));
	}

	static void write(long x, int radix = 10)
	{
		write(ltoa(x, buf, 10));
	}

	static void write(unsigned long x, int radix = 10)
	{
		write(ultoa(x, buf, 10));
	}

	static void write(double x, signed char w = 8, unsigned char p = 2)
	{
		write(dtostrf(x, w, p, buf));
	}

	static void write_e(double x, unsigned char p = 2)
	{
		write(dtostre(x, buf, p, 0x01));
	}

	static void write(const char *p)
	{
		uint8_t i = 0;

		while (*p && i++ < 15)
			write_char(*p++);
	}

	static void write_char(char c)
	{
		send(rs | ((c >> 4) & 0xf));
		send(rs | (c & 0xf));
	}

	static void set_pos(uint8_t r, uint8_t c)
	{
		uint8_t w = _BV(7) | (r ? 0x40 : 0) | (c & 0x1f);	// set ddram address + address
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
		delay_us(1700);				// wait time > 1.64ms
	}

private:
    typedef D10 BL;
    typedef D9 E;
    typedef D8 RS;
    typedef D7 DB7;
    typedef D6 DB6;
    typedef D5 DB5;
    typedef D4 DB4;

    typedef outputs_t<DB4, DB5, DB6, DB7, RS, E, BL> tc1602;

	static const uint8_t rs = _BV(4);
	static const uint8_t e  = _BV(5);
	static const uint8_t l  = _BV(6);

	static void send(uint8_t w)
	{
		tc1602::write(w | l | e);
		nop<1>();					// minimum pulse width 140ns
		tc1602::write((w | l) & ~e);
		delay_us(40);				// min cycle time is min op time is 37us
	}

	static char buf[33];			// radix 2 on a 32-bit number plus terminator
};

char DFR0009::buf[33];


void setup()
{
	LED::setup();
    DFR0009::setup();
}

void loop()
{
    static uint16_t x = 0;

	// LED::toggle();
    DFR0009::set_pos(0, 0);
    DFR0009::write(x++);
    DFR0009::write("      ");
	//delay_ms(50);
}

int main()
{
	setup();
	for (;;)
		loop();
}

