#include "../AVR/Pins.h"
//#include "../AVR/Delay.h"

typedef pin_t<PC,7> LED;
typedef pin_t<PB,6> RS;
typedef pin_t<PB,5> RW;
typedef pin_t<PB,4> E;
typedef pin_t<PD,2> DB0;
typedef pin_t<PD,3> DB1;
typedef pin_t<PD,1> DB2;
typedef pin_t<PD,0> DB3;
typedef pin_t<PD,4> DB4;
typedef pin_t<PC,6> DB5;
typedef pin_t<PD,7> DB6;
typedef pin_t<PE,6> DB7;

static void exec_display()
{
	set<E>();
	__asm__ volatile("nop");	// FIXME: nop<1>();	// minimum pulse width 140ns
	clear<E>();
	delayMicroseconds(40);		// min cycle time is min op time is 37us
}

static void write_data(uint8_t x)
{
	write<DB0>(x & (1 << 0));
	write<DB1>(x & (1 << 1));
	write<DB2>(x & (1 << 2));
	write<DB3>(x & (1 << 3));
	write<DB4>(x & (1 << 4));
	write<DB5>(x & (1 << 5));
	write<DB6>(x & (1 << 6));
	write<DB7>(x & (1 << 7));
}

static void write_char(char c)
{
	set<RS>();
	clear<RW>();
	write_data(static_cast<uint8_t>(c));
	exec_display();
}

static void write_string(const char *p)
{
	uint8_t i = 0;

	while (*p && i++ < 15)
		write_char(*p++);
}

static void position_display(uint8_t r, uint8_t c)
{
	clear<RS, RW>();	// command mode
	write_data((1 << 7) | (r ? 0x40 : 0) | (c & 0x1f));	// set ddram address + address
	exec_display();
}

static void clear_display()
{
	clear<RS, RW>();	// command mode
	write_data(0);
	set<DB0>();			// display clear
	exec_display();
	delayMicroseconds(1700);	// requires 1.64ms
}

static void cursor_display(bool on, bool blink)
{
	clear<RS, RW>();	// command mode
	write_data(0);
	set<DB3>();			// display & cursor on/off
	set<DB2>();			// display on
	write<DB1>(on);		// cursor on
	write<DB0>(blink);	// cursor blink
	exec_display();
}

static void init_display()
{
	clear<RS, RW>();	// command mode

	write_data(0);
	set<DB5>();			// function set
	set<DB4>();			// 8-bit mode
	set<DB3>();			// two-line mode
//	set<DB2>();			// 5x11 pixel glyphs
	exec_display();

	write_data(0);
	set<DB3>();			// display & cursor on/off
	set<DB2>();			// display on
	set<DB1>();			// cursor on
	set<DB0>();			// cursor blink
	exec_display();

	write_data(0);
	set<DB2>();			// entry mode
//	set<DB1>();			// right direction
//	set<DB0>();			// shift display
	exec_display();

	clear_display();
}

void setup()
{
	digital_out<LED>();
	digital_out<RS>();
	digital_out<RW>();
	digital_out<E>();
	digital_out<DB0>();
	digital_out<DB1>();
	digital_out<DB2>();
	digital_out<DB3>();
	digital_out<DB4>();
	digital_out<DB5>();
	digital_out<DB6>();
	digital_out<DB7>();
	init_display();
}

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

void loop()
{
	static uint16_t i = 0;

	toggle<LED>();

	clear_display();
	position_display(i & 1, i & 2);
	write_string(fortunes[i]);
	cursor_display(i & 1, i & 2);
	if (++i >= n_fortunes)
		i = 0;
	delay(250);
}

