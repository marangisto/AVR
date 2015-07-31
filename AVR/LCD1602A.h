#ifndef LCD1602A_H
#define LCD1602A_H

#include "Pins.h"
#include "Delay.h"
#include "SN74HC595.h"

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
		delay_us(1700);				// wait time > 1.64ms
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
		delay_us(40);				// min cycle time is min op time is 37us
	}
};

#endif // LCD1602A_H

