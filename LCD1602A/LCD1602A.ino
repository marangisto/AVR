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
	delay(1);	// FIXME: pulse width min 140ns
	clear<E>();
	delay(1);	// FIXME: min cycle time is 1.2us but min op time is 37us
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

	write_data(0);
	set<DB0>();			// display clear
	exec_display();
	delay(2);			// requires 1.64ms
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

void loop()
{
	static const char msg[] = "This is the 1602A-1 LCD display in direct control using 8 bit data transfer... ";
	static const uint16_t msg_len = sizeof(msg) - 1;
	static uint16_t p = 0;

	toggle<LED>();
	write_char(msg[p++]);
	if (p >= msg_len)
		p = 0;
	delay(100);
}

