#pragma once

#include "Pins.h"
#include "Delay.h"
#include "SN74HC595.h"
#include <stdlib.h>

template<class DT, class CK, class LT>
class lcd1602a_t
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

		sr::setup();

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
		send(RS | ((c >> 4) & 0xf));
		send(RS | (c & 0xf));
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
	typedef sn74hc595_t<DT, CK, LT, LSB_FIRST> sr;

	static const uint8_t RS = _BV(4);
	static const uint8_t E  = _BV(5);
	static const uint8_t L  = _BV(6);

	static void send(uint8_t w)
	{
		sr::write(w | L | E);
		nop<1>();					// minimum pulse width 140ns
		sr::write((w | L) & ~E);
		delay_us(40);				// min cycle time is min op time is 37us
	}

	static char buf[33];			// radix 2 on a 32-bit number plus terminator
};

template<class DT, class CK, class LT>
char lcd1602a_t<DT, CK, LT>::buf[33];

