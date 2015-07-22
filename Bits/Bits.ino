#include "../AVR/Bits.h"
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

typedef bits_t<DB5, DB6, DB7> DB;

void setup()
{
	digital_out<RS, RW, E>();
	digital_out<DB0, DB1, DB2, DB3, DB4, DB6>();
	digital_out<DB5, LED>();
	digital_out<DB7>();
}

void loop()
{
	static uint16_t i = 0;

	toggle<LED>();
	toggle<DB1>();
	write_bits<DB>(i++);
	delay(250);
}

