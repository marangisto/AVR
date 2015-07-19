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
	toggle<RS>();
}

void write_data(uint8_t x)
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

void loop()
{
	static uint8_t x = 1;

	write_data(x);
	x <<= 1;
	if (x == 0)
		x = 1;
	toggle<RS>();
	toggle<RW>();
	toggle<LED>();
	delay(500);
}

